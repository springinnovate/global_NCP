"""Fix the Kamble-regression calibration bug and make the remaining classes' Kc per-pixel, on top
of the combined forest+grassland/shrub Kc (07f_build_kc_combined_forest_grassland.py).

The bug (found 2026-09-24, see docs/swy/message_draft_becky_kc_cn_questions_2026-09-24.md #7):
07b_build_biophysical_table_becky_inputs.py applies Kamble et al. (2013)'s
Kc = 1.457*NDVI - 0.1725 to EVI. The coefficients are fit on NDVI (r2=0.90-0.91 vs. AmeriFlux,
docs/swy/research_notes.md:86) and EVI runs at ~65% of NDVI for the same vegetation in this AOI,
so every class still on that regression got a biased, non-uniformly distorted Kc. This script
applies the regression to the index it was validated on (MOD13A3 NDVI, same 1km grid and QA as
the EVI already used), per pixel instead of as a per-class zonal mean.

NDVI saturation at high biomass (the usual argument for EVI) mostly matters for forest, which
already has its own literature-native EVI model (Negron Juarez, 07d) and is left untouched here,
as are grassland/shrub (Oliveira, 07e) and the fixed-Kc classes (Built-up, Fishpond, Inland
Water). Only these classes change:
    1 Annual Crop, 8 Mangrove Forest, 9 Marshland/Swamp, 11 Open/Barren, 12 Perennial Crop

Pixels whose NDVI fails QA in a given month get the class's NDVI-based mean Kc for that month
(class annual mean if the whole class-month is missing), so no pixel falls back to the old
EVI-derived table value.

Since 07f's output is the starting point, a run on this output differs from 09k ONLY in these
five classes' Kc -- the 09k-vs-09l comparison isolates this fix.

Inputs:
    data/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif
    data/swy/philippines/inputs/mod13a3_ndvi_2020/*_NDVI_*.tif, *_VI_Quality_*.tif
    data/swy/philippines/inputs/kc_combined_forest_grassland_2020/kc_MM.tif

Outputs:
    data/swy/philippines/inputs/kc_ndvi_perpixel_2020/kc_MM.tif
    data/swy/philippines/inputs/kc_ndvi_perpixel_2020/class_kc_summary.csv  (old table Kc vs new
        per-pixel mean/sd, per class per month)
"""
import glob
import os

import numpy as np
import pandas as pd
import rasterio
from rasterio.crs import CRS
from rasterio.enums import Resampling
from rasterio.warp import reproject

INPUTS_SP_DIR = "data/swy/philippines/INPUTS_SP"
LULC_PATH = os.path.join(INPUTS_SP_DIR, "ph_baseline_lulc_md5_7f29da.tif")
OLD_TABLE_PATH = os.path.join(INPUTS_SP_DIR, "ph_biophysical_table_ncp_kc_cn.csv")
NDVI_DIR = "data/swy/philippines/inputs/mod13a3_ndvi_2020"
COMBINED_KC_DIR = "data/swy/philippines/inputs/kc_combined_forest_grassland_2020"
OUT_DIR = "data/swy/philippines/inputs/kc_ndvi_perpixel_2020"

KAMBLE_LULC_IDS = {
    1: "Annual Crop", 8: "Mangrove Forest", 9: "Marshland/Swamp", 11: "Open/Barren",
    12: "Perennial Crop",
}
YEAR = 2020
QA_GOOD_MAX = 0b01  # MOD13 VI_Quality bits 0-1: 00=good, 01=marginal usable, keep both
KC_FLOOR = 0.05  # same floor as 07b/07d


def kamble_kc(ndvi):
    return np.maximum(KC_FLOOR, 1.457 * ndvi - 0.1725)


def main():
    os.makedirs(OUT_DIR, exist_ok=True)

    ndvi_files = sorted(glob.glob(f"{NDVI_DIR}/MOD13A3.061__1_km_monthly_NDVI_doy{YEAR}*.tif"))
    qa_files = sorted(glob.glob(f"{NDVI_DIR}/MOD13A3.061__1_km_monthly_VI_Quality_doy{YEAR}*.tif"))
    assert len(ndvi_files) == 12 and len(qa_files) == 12, (
        f"expected 12 NDVI + 12 QA files, got {len(ndvi_files)}/{len(qa_files)}"
    )

    with rasterio.open(os.path.join(COMBINED_KC_DIR, "kc_01.tif")) as ref:
        ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape
        ref_profile = ref.profile
    with rasterio.open(ndvi_files[0]) as nsrc:
        assert nsrc.shape == ref_shape and nsrc.transform.almost_equals(ref_transform), (
            "NDVI grid differs from the combined Kc grid -- this script assumes they match"
        )

    # same LULC-to-1km mode resampling as 07d/07e/07f, so class masks line up exactly
    lulc_ref = np.empty(ref_shape, dtype="float32")
    with rasterio.open(LULC_PATH) as src:
        reproject(
            source=rasterio.band(src, 1), destination=lulc_ref,
            src_transform=src.transform, src_crs=src.crs,
            dst_transform=ref_transform, dst_crs=ref_crs,
            resampling=Resampling.mode,
        )
    lulc_ref = np.round(lulc_ref)
    class_masks = {c: lulc_ref == c for c in KAMBLE_LULC_IDS}
    for c, name in KAMBLE_LULC_IDS.items():
        print(f"{name}: {class_masks[c].sum()} pixels")

    # read all 12 months first: the fallback for a class-month with no good pixels needs the
    # class's annual mean
    kc_monthly, good_monthly = [], []
    for ndvi_f, qa_f in zip(ndvi_files, qa_files):
        with rasterio.open(ndvi_f) as nsrc:
            ndvi_raw = nsrc.read(1)
            nodata = nsrc.nodata
        with rasterio.open(qa_f) as qsrc:
            qa = qsrc.read(1)
        good = ((qa & 0b11) <= QA_GOOD_MAX) & (ndvi_raw != nodata)
        kc_monthly.append(kamble_kc(ndvi_raw.astype("float32") * 0.0001))
        good_monthly.append(good)

    class_month_mean = {c: [] for c in KAMBLE_LULC_IDS}
    for c, mask in class_masks.items():
        for kc, good in zip(kc_monthly, good_monthly):
            sel = mask & good
            class_month_mean[c].append(float(kc[sel].mean()) if sel.any() else np.nan)
        vals = np.array(class_month_mean[c])
        if np.isnan(vals).any():
            vals[np.isnan(vals)] = np.nanmean(vals)
            class_month_mean[c] = vals.tolist()

    old_table = pd.read_csv(OLD_TABLE_PATH).set_index("lulc_id")
    summary = []

    for month in range(1, 13):
        with rasterio.open(os.path.join(COMBINED_KC_DIR, f"kc_{month:02d}.tif")) as src:
            kc_out = src.read(1).astype("float32")
        kc_new, good = kc_monthly[month - 1], good_monthly[month - 1]

        for c, mask in class_masks.items():
            kc_out[mask & good] = kc_new[mask & good]
            n_filled = int((mask & ~good).sum())
            kc_out[mask & ~good] = class_month_mean[c][month - 1]
            vals = kc_out[mask]
            summary.append({
                "lulc_id": c, "class": KAMBLE_LULC_IDS[c], "month": month,
                "kc_old_evi_table": float(old_table.loc[c, f"Kc_{month}"]),
                "kc_new_ndvi_mean": round(float(vals.mean()), 4),
                "kc_new_ndvi_sd": round(float(vals.std()), 4),
                "kc_new_ndvi_p05": round(float(np.percentile(vals, 5)), 4),
                "kc_new_ndvi_p95": round(float(np.percentile(vals, 95)), 4),
                "n_pixels": int(mask.sum()), "n_qa_filled": n_filled,
            })

        out_path = os.path.join(OUT_DIR, f"kc_{month:02d}.tif")
        profile = ref_profile.copy()
        profile.update(dtype="float32", count=1, nodata=np.nan, compress="lzw", crs=ref_crs or CRS.from_epsg(4326))
        with rasterio.open(out_path, "w", **profile) as dst:
            dst.write(kc_out, 1)

    summary_df = pd.DataFrame(summary)
    summary_df.to_csv(os.path.join(OUT_DIR, "class_kc_summary.csv"), index=False)

    annual = summary_df.groupby("class")[["kc_old_evi_table", "kc_new_ndvi_mean", "kc_new_ndvi_sd"]].mean()
    annual["ratio_new_old"] = annual["kc_new_ndvi_mean"] / annual["kc_old_evi_table"]
    qa_share = summary_df.groupby("class").apply(lambda d: d["n_qa_filled"].sum() / d["n_pixels"].sum())
    annual["qa_filled_share"] = qa_share
    print("\n== Annual-mean Kc per class: old (EVI into Kamble, per class) vs new (NDVI, per pixel) ==")
    print(annual.round(3).to_string())
    print(f"\ndone -- see {OUT_DIR}")


if __name__ == "__main__":
    main()
