"""Map tiles (XYZ PNG, web mercator) of change from the native pixels, for the explorer's change maps.

Two metrics:
  spc  symmetric percentage change, as paper Figure 1 (scripts/mapping/make_native_change_maps.py): one
       scale for all services, signed so that positive = adverse for the service, square-root stretch
       over -200..+200%, teal = favourable, orange = adverse; near-zero change fades in, none is transparent.
  abs  absolute change (2020 minus 1992, per hectare, native units), as the Supplement's absolute map:
       linear scale per service, symmetric at the 1st/99th percentile of the 0.05-degree block means,
       same colours and orientation. The limits are written to tiles/abs_limits.csv for the app legend.
Each tile pixel is the mean of the native pixels it covers (average resampling): zoom 0-4 from the
0.05-degree aggregates (analysis/native_spc.py), deeper zooms from the native rasters (SPC) or the raw
1992 and 2020 rasters (absolute). Empty tiles are not written; Leaflet enlarges tiles beyond the last level.

Usage: .venv/Scripts/python.exe scripts/dashboard/make_change_tiles.py <service|all> [spc|abs] [max_zoom] [workers]
Writes: data/processed/dashboard/tiles/<service>[_abs]/{z}/{x}/{y}.png
"""
import csv
import math
import sys
import time
from multiprocessing import Pool
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_bounds
from rasterio.warp import reproject, transform_bounds, Resampling
from rasterio.windows import from_bounds as win_from_bounds, Window
from PIL import Image

ROOT = Path(__file__).resolve().parents[2]
NAT = ROOT / "data" / "processed" / "native_spc"
RAW = ROOT / "data" / "raw" / "base_years"
OUT = ROOT / "data" / "processed" / "dashboard" / "tiles"
SERVICES = {   # native SPC raster, aggregate, raw 1992 / 2020 globs, +1 if an increase is adverse, unit, max zoom
    "n_export": ("n_export_spc_native.tif", "n_export_agg_f18.tif",
                 "global_n_export_tnc_esa1992*.tif", "global_n_export_tnc_esa2020*.tif", 1, "kg N/ha/yr", 9),
    "sed_export": ("sed_export_spc_native.tif", "sed_export_agg_f18.tif",
                   "global_sed_export_marine_mod_ESA_1992*.tif", "global_sed_export_marine_mod_ESA_2020*.tif", 1, "t/ha/yr", 9),
    "pollination": ("pollination_spc_native.tif", "pollination_agg_f18.tif",
                    "realized_polllination_on_ag_ESA1992*.tif", "realized_polllination_on_ag_ESA2020*.tif", -1,
                    "people fed equiv./ha", 9),
    "nature_access": ("nature_access_spc_native.tif", "nature_access_agg_f3.tif",
                      "nature_access_lspop2019_ESA1992*.tif", "nature_access_lspop2019_ESA2020*.tif", -1, "access index", 7),
}
R = 20037508.342789244
TEAL, WHITE, ORANGE = np.array([0, 145, 145]), np.array([255, 255, 255]), np.array([240, 125, 0])


def colour(t, alpha):
    """t in [-1, 1] (positive = adverse) and alpha in [0, 1] -> RGBA uint8."""
    t = t[..., None]
    rgb = np.where(t >= 0, WHITE + (ORANGE - WHITE) * t, WHITE + (TEAL - WHITE) * (-t))
    return np.concatenate([rgb, alpha[..., None] * 255], axis=-1).round().astype(np.uint8)


def one(glob):
    f = sorted(RAW.glob(glob))
    assert len(f) == 1, (glob, f)
    return f[0]


def abs_limit(svc):
    """Symmetric limit for absolute change: max |1st, 99th percentile| of the aggregate block means."""
    with rasterio.open(NAT / SERVICES[svc][1]) as r:
        n, s1, s2 = r.read(5), r.read(8), r.read(9)
    v = ((s2 - s1) / np.where(n > 0, n, np.nan))[n > 0]
    v = v[np.isfinite(v) & (v != 0)]
    return float(np.max(np.abs(np.quantile(v, [0.01, 0.99]))))


_src = {}


def init(svc, metric, limit):
    native, agg, g1, g2, sign, _, _ = SERVICES[svc]
    _src.update(svc=svc, metric=metric, sign=sign, limit=limit, agg=rasterio.open(NAT / agg))
    if metric == "spc":
        _src["native"] = rasterio.open(NAT / native)
    else:
        _src["r1"], _src["r2"] = rasterio.open(one(g1)), rasterio.open(one(g2))


def tile_bounds(z, x, y):
    size = 2 * R / 2 ** z
    return (-R + x * size, R - (y + 1) * size, -R + (x + 1) * size, R - y * size)


def window_for(src, b):
    sb = transform_bounds("EPSG:3857", src.crs, *b, densify_pts=21)
    w = win_from_bounds(*sb, transform=src.transform).round_offsets().round_lengths()
    w = Window(w.col_off - 2, w.row_off - 2, w.width + 4, w.height + 4)
    try:
        w = w.intersection(Window(0, 0, src.width, src.height))
    except Exception:
        return None
    return w if w.width >= 1 and w.height >= 1 else None


def read_values(z, b):
    """Values to draw for a tile, on the source grid: (array, source dataset, window)."""
    if z <= 4:
        src = _src["agg"]
        w = window_for(src, b)
        if w is None:
            return None, None, None
        if _src["metric"] == "spc":
            a = src.read(1, window=w).astype("float32")
        else:
            n = src.read(5, window=w)
            a = ((src.read(9, window=w) - src.read(8, window=w)) / np.where(n > 0, n, np.nan)).astype("float32")
        return a, src, w
    if _src["metric"] == "spc":
        src = _src["native"]
        w = window_for(src, b)
        if w is None:
            return None, None, None
        a = src.read(1, window=w).astype("float32")
        a[a == -32768] = np.nan
        return a / 100.0, src, w
    r1, r2 = _src["r1"], _src["r2"]
    w = window_for(r1, b)
    if w is None:
        return None, None, None
    a1, a2 = r1.read(1, window=w).astype("float32"), r2.read(1, window=w).astype("float32")
    for a, r in ((a1, r1), (a2, r2)):
        bad = ~np.isfinite(a)
        if r.nodata is not None and np.isfinite(r.nodata):
            bad |= a == r.nodata
        a[bad] = np.nan
    return a2 - a1, r1, w


def render(args):
    z, x, y = args
    b = tile_bounds(z, x, y)
    a, src, w = read_values(z, b)
    if a is None or not np.isfinite(a).any():
        return 0
    dst = np.full((256, 256), np.nan, dtype="float32")
    reproject(a, dst, src_transform=src.window_transform(w), src_crs=src.crs, src_nodata=np.nan,
              dst_transform=from_bounds(*b, 256, 256), dst_crs="EPSG:3857", dst_nodata=np.nan,
              resampling=Resampling.average)
    v = np.nan_to_num(dst) * _src["sign"]
    if _src["metric"] == "spc":
        ok = np.isfinite(dst) & (np.abs(dst) >= 0.05)
        t = np.sign(v) * np.sqrt(np.clip(np.abs(v), 0, 200) / 200.0)
        alpha = np.minimum(np.abs(v) / 5.0, 1.0)
    else:
        L = _src["limit"]
        ok = np.isfinite(dst) & (np.abs(dst) > 1e-6)
        t = np.clip(v / L, -1, 1)
        alpha = np.minimum(np.abs(t) / 0.08, 1.0)
    if not ok.any():
        return 0
    rgba = colour(t, alpha)
    rgba[~ok, 3] = 0
    p = OUT / (_src["svc"] + ("" if _src["metric"] == "spc" else "_abs")) / str(z) / str(x)
    p.mkdir(parents=True, exist_ok=True)
    Image.fromarray(rgba, "RGBA").save(p / f"{y}.png", optimize=True)
    return 1


def tiles_for(src, z):
    lon0, lat0, lon1, lat1 = transform_bounds(src.crs, "EPSG:4326", *src.bounds, densify_pts=21)
    lat0, lat1 = max(lat0, -85.05), min(lat1, 85.05)
    n = 2 ** z

    def ty(lat):
        return int((1 - math.asinh(math.tan(math.radians(lat))) / math.pi) / 2 * n)
    xs = range(max(0, int((lon0 + 180) / 360 * n)), min(n - 1, int((lon1 + 180) / 360 * n)) + 1)
    ys = range(max(0, ty(lat1)), min(n - 1, ty(lat0)) + 1)
    return [(x, y) for x in xs for y in ys]


def write_limit(svc, limit):
    f = OUT / "abs_limits.csv"
    rows = {}
    if f.exists():
        with open(f, newline="") as h:
            rows = {r["service"]: r for r in csv.DictReader(h)}
    rows[svc] = {"service": svc, "limit": f"{limit:.6g}", "unit": SERVICES[svc][5]}
    OUT.mkdir(parents=True, exist_ok=True)
    with open(f, "w", newline="") as h:
        wr = csv.DictWriter(h, fieldnames=["service", "limit", "unit"])
        wr.writeheader()
        wr.writerows(rows.values())


def main(svc, metric, max_zoom, workers):
    max_zoom = SERVICES[svc][6] if max_zoom is None else max_zoom
    limit = abs_limit(svc) if metric == "abs" else None
    if limit is not None:
        write_limit(svc, limit)
    with rasterio.open(NAT / SERVICES[svc][0]) as src:
        jobs = [(z, x, y) for z in range(max_zoom + 1) for x, y in tiles_for(src, z)]
    t0 = time.time()
    with Pool(workers, initializer=init, initargs=(svc, metric, limit)) as pool:
        written = sum(pool.imap_unordered(render, jobs, chunksize=64))
    d = OUT / (svc + ("" if metric == "spc" else "_abs"))
    size = sum(f.stat().st_size for f in d.rglob("*.png")) / 1e6 if d.exists() else 0
    print(f"{svc} {metric}: {written} tiles of {len(jobs)} (z0-{max_zoom}), {size:.0f} MB, {time.time() - t0:.0f}s",
          flush=True)


if __name__ == "__main__":
    s = sys.argv[1]
    m = sys.argv[2] if len(sys.argv) > 2 else "spc"
    mz = int(sys.argv[3]) if len(sys.argv) > 3 else None
    wk = int(sys.argv[4]) if len(sys.argv) > 4 else 18
    for svc in (SERVICES if s == "all" else [s]):
        main(svc, m, mz, wk)
