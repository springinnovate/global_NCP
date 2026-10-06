"""Recompute the nitrogen retention ratio for Path A on aligned grids (2026-10-01).

ratio = n_retention / (n_retention + n_export), per pixel, for 1992 and 2020, plus 2020 - 1992.

Why: Python_scripts/calculate_ratios.py pairs the two inputs by pixel index. The current N export
grid starts at -180.0 / 60.000417 while N retention starts at -179.9 / 60.0, so index pairing
compares pixels 36 columns (0.1 degree) apart. Path A's ratio change raster (March 2026) was also
built from an older N export version. Here N export is read onto the retention grid with a
nearest-pixel shift derived from the two transforms (same resolution, so the shift is constant:
36 columns, 0 rows; the 0.15-pixel y offset rounds to 0). Ratios are not converted to per-ha.

Steps:
  1. ratio 1992, ratio 2020, ratio change   -> data/raw/n_ret_ratio_aligned/
  2. coordinate-based spot check of the aligned ratio against the raw rasters
  3. regional zonal stats (country, region_wb, income_grp, biome) on all three rasters
     -> zonal_stats_toolkit/runner.py analysis_configs/path_a_n_ret_ratio_rerun.ini

Run inside the project Docker image:
  MSYS_NO_PATHCONV=1 docker run --rm -v "C:/projects:/projects" \
    -v "C:/Users/JerónimoRodríguezEsc/data/global_ncp:/gncp" \
    therealspring/global_ncp-computational-environment:latest bash -c \
    "micromamba run -n geopy311 pip install -q datasketches tqdm fiona && \
     micromamba run -n geopy311 python /projects/global_NCP/Python_scripts/rerun_n_ret_ratio_path_a.py"
"""

import math
import subprocess
import sys
import time
from pathlib import Path

import numpy as np
import rasterio
from rasterio.windows import Window

GNCP = Path("/projects/global_NCP")
TOOLKIT = Path("/projects/zonal_stats_toolkit")
BASE = GNCP / "data/raw/base_years"
INPUTS = {
    1992: (BASE / "global_n_retention_ESAmar_1992_fertilizer_current_valid_md5_86031b-010.tif",
           BASE / "global_n_export_tnc_esa1992_compressed_md5_728edc-004.tif"),
    2020: (BASE / "global_n_retention_ESAmar_2020_fertilizer_current_valid_md5_82fc1e-005.tif",
           BASE / "global_n_export_tnc_esa2020_compressed_md5_1d3c17-007.tif"),
}
OUT_DIR = GNCP / "data/raw/n_ret_ratio_aligned"
OUT = {1992: OUT_DIR / "n_retention_ratio_1992.tif",
       2020: OUT_DIR / "n_retention_ratio_2020.tif",
       "diff": OUT_DIR / "n_retention_ratio_diff_1992_2020.tif"}
INI = GNCP / "analysis_configs/path_a_n_ret_ratio_rerun.ini"
STRIP = 256  # rows per strip; matches the 256x256 input blocks


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def pixel_shift(ret, exp):
    """Constant (row, col) offset so that exp[i + dr, j + dc] is the nearest export pixel to ret[i, j]."""
    rt, et = ret.transform, exp.transform
    assert ret.crs == exp.crs, "CRS differ"
    assert math.isclose(rt.a, et.a, rel_tol=1e-9) and math.isclose(rt.e, et.e, rel_tol=1e-9), "resolutions differ"
    assert rt.b == rt.d == et.b == et.d == 0, "rotated grid"
    dx = (rt.c - et.c) / et.a  # retention left edge, in export pixel units
    dy = (rt.f - et.f) / et.e  # retention top edge, in export pixel units
    return math.floor(0.5 + dy), math.floor(0.5 + dx), dy, dx


def read_masked(src, window):
    a = src.read(1, window=window, boundless=True, fill_value=src.nodata).astype("float32")
    a[(a == src.nodata) | ~np.isfinite(a)] = np.nan
    return a


def ratio(r, e):
    with np.errstate(divide="ignore", invalid="ignore"):
        out = r / (r + e)
    out[~np.isfinite(out)] = np.nan
    return out


def build_rasters():
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    srcs = {y: (rasterio.open(r), rasterio.open(e)) for y, (r, e) in INPUTS.items()}
    ref = srcs[1992][0]
    for y, (r, e) in srcs.items():
        assert r.transform == ref.transform and r.shape == ref.shape, f"retention {y} grid differs"
        dr, dc, dy, dx = pixel_shift(r, e)
        log(f"{y}: export offset vs retention = {dy:.4f} rows, {dx:.4f} cols -> shift ({dr}, {dc})")
    dr, dc, _, _ = pixel_shift(*srcs[1992])
    assert (dr, dc) == pixel_shift(*srcs[2020])[:2]

    profile = ref.profile.copy()
    profile.update(dtype="float32", nodata=np.nan, compress="lzw", tiled=True,
                   blockxsize=256, blockysize=256, bigtiff="YES")
    dsts = {k: rasterio.open(p, "w", **profile) for k, p in OUT.items()}
    width, height = ref.width, ref.height
    stats = {k: [0, 0.0] for k in OUT}  # count, sum
    try:
        for row in range(0, height, STRIP):
            n = min(STRIP, height - row)
            w_ret = Window(0, row, width, n)
            w_exp = Window(dc, row + dr, width, n)
            rat = {y: ratio(read_masked(r, w_ret), read_masked(e, w_exp)) for y, (r, e) in srcs.items()}
            rat["diff"] = rat[2020] - rat[1992]
            for k, a in rat.items():
                dsts[k].write(a, 1, window=w_ret)
                v = a[np.isfinite(a)]
                stats[k][0] += v.size
                stats[k][1] += float(v.sum(dtype="float64"))
            if (row // STRIP) % 20 == 0:
                log(f"rows {row}/{height}")
    finally:
        for d in dsts.values():
            d.close()
        for r, e in srcs.values():
            r.close()
            e.close()
    for k, (cnt, s) in stats.items():
        log(f"{k}: valid pixels {cnt}, mean {s / max(cnt, 1):.6f}")


def spot_check(n=2000, seed=0):
    """Recompute the ratio at random points from the raw rasters by coordinate and compare to the output."""
    rng = np.random.default_rng(seed)
    with rasterio.open(OUT[2020]) as o, rasterio.open(INPUTS[2020][0]) as r, rasterio.open(INPUTS[2020][1]) as e:
        rows = rng.integers(0, o.height, n * 50)
        cols = rng.integers(0, o.width, n * 50)
        xs, ys = rasterio.transform.xy(o.transform, rows, cols)
        pts = list(zip(xs, ys))
        out = np.array([v[0] for v in o.sample(pts)], dtype="float64")
        rv = np.array([v[0] for v in r.sample(pts)], dtype="float64")
        ev = np.array([v[0] for v in e.sample(pts)], dtype="float64")
        ok = np.isfinite(out) & (rv != r.nodata) & (ev != e.nodata) & ((rv + ev) != 0)
        expect = (rv[ok] / (rv[ok] + ev[ok]))[:n]
        got = out[ok][:n]
        log(f"spot check: {got.size} points, max |diff| = {np.max(np.abs(got - expect)):.2e}")
        assert got.size > 100 and np.allclose(got, expect, atol=1e-5), "aligned ratio != coordinate-based ratio"


def main():
    if all(p.exists() for p in OUT.values()):
        log("step 1: ratio rasters exist, skipping")
    else:
        log("step 1: aligned ratio rasters")
        build_rasters()
    log("step 2: spot check")
    spot_check()
    log("step 3: regional zonal statistics")
    subprocess.run([sys.executable, str(TOOLKIT / "runner.py"), str(INI)], check=True, cwd=str(TOOLKIT))
    log("done")


if __name__ == "__main__":
    main()
