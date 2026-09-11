"""Build the Borneo SWY biophysical table (CN_A-D, Kc_1-12, root_depth per lucode).

Consolidates the 2026-09-10/11 interactive work into a re-runnable script. Real
simplification made here, not a full implementation: Kc is computed via the Kamble et al.
(2013) NDVI regression for ALL vegetated classes (cropland included), not switched to FAO-56
for cropland — this project does not yet have region-correct crop-calendar timing for FAO-56,
and the NDVI regression is more defensible than applying the wrong (Northern Hemisphere)
calendar. See docs/swy/swy_methods.qmd for the full methodology.

Inputs (must already exist — this script does not download anything):
    data/swy_shared_package/borneo_aoi.gpkg
    data/borneo_lulc/lulc_borneo_2020.tif        (built by extract_borneo_lulc_2020.py)
    data/mod13a3_borneo_2020_verify/*NDVI*.tif and *VI_Quality*.tif
    data/swy_shared_package/cn_tables/gcn250_esa_lc_cn_table.csv

Output:
    data/borneo_lulc/borneo_biophysical_table_2020.csv
"""
import glob
import json
import os

import numpy as np
import pandas as pd
import rasterio
from rasterio.warp import Resampling, reproject

NDVI_DIR = "data/mod13a3_borneo_2020_verify"
LULC_PATH = "data/borneo_lulc/lulc_borneo_2020.tif"
CN_TABLE_PATH = "data/swy_shared_package/cn_tables/gcn250_esa_lc_cn_table.csv"
OUT_DIR = "data/borneo_lulc"
OUT_TABLE_PATH = os.path.join(OUT_DIR, "borneo_biophysical_table_2020.csv")

# Non-vegetated / special classes: fixed Kc per InVEST's own guidance, not NDVI-derived.
# Water set to CN=100 (full runoff, standard convention — CN's premise doesn't apply to
# open water, GCN250 itself leaves this class blank).
FIXED_KC = {190: 0.10, 210: 1.05, 220: 0.10, 0: 0.0, 255: 0.0}
WATER_CN = 100

# MOD13 VI_Quality bits 0-1: 00=good, 01=marginal usable, 10/11=unusable. Keep 0 and 1.
QA_GOOD_MAX = 0b01


def compute_class_monthly_ndvi():
    """Zonal mean NDVI per LULC class per month, QA-masked, on the NDVI (1km) grid."""
    ndvi_files = sorted(glob.glob(f"{NDVI_DIR}/MOD13A3.061__1_km_monthly_NDVI_doy2020*.tif"))
    qa_files = sorted(glob.glob(f"{NDVI_DIR}/MOD13A3.061__1_km_monthly_VI_Quality_doy2020*.tif"))
    assert len(ndvi_files) == 12 and len(qa_files) == 12, (
        f"expected 12 NDVI + 12 QA files, got {len(ndvi_files)}/{len(qa_files)}"
    )

    with rasterio.open(ndvi_files[0]) as ref:
        ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape

    with rasterio.open(LULC_PATH) as lulc_src:
        lulc_resampled = np.empty(ref_shape, dtype=lulc_src.dtypes[0])
        reproject(
            source=rasterio.band(lulc_src, 1),
            destination=lulc_resampled,
            src_transform=lulc_src.transform,
            src_crs=lulc_src.crs,
            dst_transform=ref_transform,
            dst_crs=ref_crs,
            resampling=Resampling.mode,
        )

    classes = sorted(int(c) for c in np.unique(lulc_resampled) if c != 255)
    monthly_class_ndvi = {c: [] for c in classes}

    for ndvi_f, qa_f in zip(ndvi_files, qa_files):
        with rasterio.open(ndvi_f) as nsrc:
            ndvi = nsrc.read(1).astype("float32")
            nodata = nsrc.nodata
        with rasterio.open(qa_f) as qsrc:
            qa = qsrc.read(1)
        good = ((qa & 0b11) <= QA_GOOD_MAX) & (ndvi != nodata)
        ndvi_scaled = ndvi * 0.0001
        for c in classes:
            mask_c = good & (lulc_resampled == c)
            monthly_class_ndvi[c].append(
                float(ndvi_scaled[mask_c].mean()) if mask_c.sum() > 0 else np.nan
            )

    os.makedirs(OUT_DIR, exist_ok=True)
    with open(os.path.join(OUT_DIR, "class_monthly_ndvi.json"), "w") as f:
        json.dump(monthly_class_ndvi, f, indent=2)
    return monthly_class_ndvi


def build_biophysical_table(monthly_class_ndvi):
    cn_table = pd.read_csv(CN_TABLE_PATH).set_index("LC_ID")
    rows = []
    for lc_id_str, monthly_ndvi in monthly_class_ndvi.items():
        lc_id = int(lc_id_str)
        vals = np.array(monthly_ndvi, dtype="float64")
        if np.isnan(vals).any() and not np.isnan(vals).all():
            vals[np.isnan(vals)] = np.nanmean(vals)

        if lc_id in FIXED_KC:
            kc = [FIXED_KC[lc_id]] * 12
        else:
            kc = [max(0.05, round(1.457 * v - 0.1725, 3)) for v in vals]

        cn_row = cn_table.loc[lc_id] if lc_id in cn_table.index else None
        cn_a, cn_b, cn_c, cn_d = (
            (WATER_CN,) * 4 if lc_id == 210
            else (cn_row["CN_A"], cn_row["CN_B"], cn_row["CN_C"], cn_row["CN_D"])
            if cn_row is not None else (np.nan,) * 4
        )
        row = {
            "lucode": lc_id,
            "LC_Name": cn_row["LC_Name"] if cn_row is not None else "unknown",
            "CN_A": cn_a, "CN_B": cn_b, "CN_C": cn_c, "CN_D": cn_d,
            "root_depth": 1000,  # confirmed dead parameter in this inspring fork, placeholder only
        }
        for m in range(1, 13):
            row[f"Kc_{m}"] = round(kc[m - 1], 3)
        rows.append(row)

    df = pd.DataFrame(rows).sort_values("lucode")
    assert not df[["CN_A", "CN_B", "CN_C", "CN_D"]].isna().any().any(), "unfilled CN values remain"
    df.to_csv(OUT_TABLE_PATH, index=False)
    return df


if __name__ == "__main__":
    monthly_class_ndvi = compute_class_monthly_ndvi()
    df = build_biophysical_table(monthly_class_ndvi)
    print(df.to_string(index=False))
    print(f"\nWritten to {OUT_TABLE_PATH}")
