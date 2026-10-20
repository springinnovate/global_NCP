"""Symmetric percentage change (SPC) on the native pixels of the 1992 and 2020 service rasters.

Streams the two base-year rasters in data/raw/base_years/ by row bands, computes per-pixel SPC
(200 * (v2020 - v1992) / (|v1992| + |v2020|); 0 where both are zero or the difference is below 1e-6,
as in the 10 km pipeline) and aggregates it to a coarser display grid (factor F pixels per side) in
two ways, so the change maps can be drawn from native pixels and compared with the 10 km grid:

  band 1  mean of per-pixel SPC
  band 2  SPC of the summed native values in the block (what the 10 km grid computes per cell)
  band 3  share of valid pixels with strong adverse change (SPC >= 20 in the adverse direction)
  band 4  share of valid pixels with strong favourable change
  band 5  number of valid pixels
  band 6  share of valid pixels that changed by 1% or more (either direction)
  band 7  share of valid pixels with no change (SPC = 0)
  band 8  sum of 1992 values, band 9  sum of 2020 values (native units, per pixel summed)

It also writes the per-pixel SPC at native resolution as int16 (SPC x 100; nodata -32768):
data/processed/native_spc/<service>_spc_native.tif (insets, the explorer's zoomable layer).

SPC is a ratio of the same pixel in two years, so it needs no per-area conversion.

Usage: .venv/Scripts/python.exe analysis/native_spc.py <service> [factor]   (factor default 18 = 0.05 deg)
Output: data/processed/native_spc/<service>_agg_f<factor>.tif
"""
import sys
import time
from pathlib import Path

import numpy as np
import rasterio
from rasterio.windows import Window

ROOT = Path(__file__).resolve().parents[1]
RAW = ROOT / "data" / "raw" / "base_years"
OUT = ROOT / "data" / "processed" / "native_spc"

SERVICES = {
    # name: (1992 file glob, 2020 file glob, +1 if an increase is adverse else -1)
    "n_export": ("global_n_export_tnc_esa1992*.tif", "global_n_export_tnc_esa2020*.tif", 1),
    "sed_export": ("global_sed_export_marine_mod_ESA_1992*.tif", "global_sed_export_marine_mod_ESA_2020*.tif", 1),
    "pollination": ("realized_polllination_on_ag_ESA1992*.tif", "realized_polllination_on_ag_ESA2020*.tif", -1),
    "nature_access": ("nature_access_lspop2019_ESA1992*.tif", "nature_access_lspop2019_ESA2020*.tif", -1),
}


def one(glob):
    f = sorted(RAW.glob(glob))
    assert len(f) == 1, (glob, f)
    return f[0]


def main(service, factor):
    g1, g2, adverse = SERVICES[service]
    f1, f2 = one(g1), one(g2)
    OUT.mkdir(parents=True, exist_ok=True)
    with rasterio.open(f1) as r1, rasterio.open(f2) as r2:
        assert (r1.width, r1.height, r1.transform) == (r2.width, r2.height, r2.transform), "grids differ"
        W, H = r1.width, r1.height
        nd1, nd2 = r1.nodata, r2.nodata
        ow, oh = -(-W // factor), -(-H // factor)
        acc = {k: np.zeros((oh, ow), dtype=np.float64)
               for k in ("spc_sum", "v1_sum", "v2_sum", "adv", "fav", "n", "chg1", "zero")}
        nprof = r1.profile.copy()
        nprof.update(dtype="int16", nodata=-32768, compress="deflate", tiled=True,
                     blockxsize=512, blockysize=512, predictor=2, BIGTIFF="YES")
        native = rasterio.open(OUT / f"{service}_spc_native.tif", "w", **nprof)
        band = factor * 16   # ~290 rows x full width keeps each array near 0.3 GB
        t0 = time.time()
        for row0 in range(0, H, band):
            h = min(band, H - row0)
            win = Window(0, row0, W, h)
            a = r1.read(1, window=win).astype(np.float64)
            b = r2.read(1, window=win).astype(np.float64)
            valid = np.isfinite(a) & np.isfinite(b)
            if nd1 is not None and np.isfinite(nd1):
                valid &= a != nd1
            if nd2 is not None and np.isfinite(nd2):
                valid &= b != nd2
            a = np.where(valid, a, 0.0)
            b = np.where(valid, b, 0.0)
            d = b - a
            den = np.abs(a) + np.abs(b)
            spc = np.where((den > 0) & (np.abs(d) >= 1e-6), 200.0 * d / np.where(den > 0, den, 1.0), 0.0)
            spc = np.where(valid, spc, 0.0)
            advv = spc * adverse
            # pad to whole blocks and reduce
            ph, pw = -(-h // factor) * factor, ow * factor

            def red(x):
                y = np.zeros((ph, pw), dtype=np.float64)
                y[:h, :W] = x
                return y.reshape(ph // factor, factor, ow, factor).sum(axis=(1, 3))

            r0 = row0 // factor
            r1_ = r0 + ph // factor
            acc["spc_sum"][r0:r1_] += red(spc)
            acc["v1_sum"][r0:r1_] += red(a)
            acc["v2_sum"][r0:r1_] += red(b)
            acc["adv"][r0:r1_] += red((advv >= 20) & valid)
            acc["fav"][r0:r1_] += red((advv <= -20) & valid)
            acc["n"][r0:r1_] += red(valid)
            acc["chg1"][r0:r1_] += red((np.abs(spc) >= 1) & valid)
            acc["zero"][r0:r1_] += red((spc == 0) & valid)
            native.write(np.where(valid, np.round(spc * 100), -32768).astype("int16"), 1, window=win)
            print(f"{service}: rows {row0 + h}/{H}  {time.time() - t0:.0f}s", flush=True)

        native.close()
        n = acc["n"]
        with np.errstate(invalid="ignore", divide="ignore"):
            mean_spc = np.where(n > 0, acc["spc_sum"] / n, np.nan)
            den = np.abs(acc["v1_sum"]) + np.abs(acc["v2_sum"])
            spc_sums = np.where((n > 0) & (den > 0), 200.0 * (acc["v2_sum"] - acc["v1_sum"]) / den,
                                np.where(n > 0, 0.0, np.nan))
            adv = np.where(n > 0, acc["adv"] / n, np.nan)
            fav = np.where(n > 0, acc["fav"] / n, np.nan)
            chg1 = np.where(n > 0, acc["chg1"] / n, np.nan)
            zero = np.where(n > 0, acc["zero"] / n, np.nan)
        prof = r1.profile.copy()
        tr = r1.transform * rasterio.Affine.scale(factor)
        prof.update(width=ow, height=oh, count=9, dtype="float32", nodata=np.nan, transform=tr,
                    compress="deflate", tiled=True, blockxsize=256, blockysize=256, predictor=3)
        out = OUT / f"{service}_agg_f{factor}.tif"
        with rasterio.open(out, "w", **prof) as dst:
            for i, arr in enumerate([mean_spc, spc_sums, adv, fav, n, chg1, zero,
                                     acc["v1_sum"], acc["v2_sum"]], start=1):
                dst.write(arr.astype("float32"), i)
            dst.descriptions = ("mean_pixel_spc", "spc_of_sums", "share_strong_adverse",
                                "share_strong_favourable", "n_valid_pixels", "share_changed_1pct",
                                "share_unchanged", "sum_1992", "sum_2020")
        print(f"wrote {out} in {time.time() - t0:.0f}s")


if __name__ == "__main__":
    main(sys.argv[1], int(sys.argv[2]) if len(sys.argv) > 2 else 18)
