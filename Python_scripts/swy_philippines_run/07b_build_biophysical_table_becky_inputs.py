"""Build a biophysical table for Becky's Philippines run using this project's own CN/Kc methodology.

Becky's ask (2026-09-14): reuse her exact inputs (her DEM [not delivered — see note below], her
LULC, her CMIP6 climate stack) but swap in this project's own Kc and CN, as an isolated-variable
comparison against her original baseline run. This script produces that swapped biophysical table.

Mechanically mirrors `07_build_biophysical_table.py` (same Kamble et al. 2013 EVI regression, same
FIXED_KC-for-non-vegetated convention, same WATER_CN=100 convention) but adapted for Becky's inputs:
  - Her LULC (`INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif`) uses her own 12-class WWF-SIPA typology,
    NOT ESA CCI codes — confirmed by inspection, not assumed. A hand-built crosswalk (below) maps
    each of her classes to its nearest ESA CCI analog so `gcn250_esa_lc_cn_table.csv` (this
    project's own CN source, region-independent) can still be used unmodified.
  - CN, Kc, and every other column (usle_c, usle_p, root_depth, nathab, affected_by_infra) that
    ISN'T part of the requested swap are carried over unchanged from her own
    `biophysical_template_PH_revised.csv` — this script only overwrites CN_A-D and Kc_1-12.
  - Kc uses EVI, not NDVI (methodology decision made 2026-09-14, see docs/swy/swy_methods.qmd) —
    the Philippines EVI fetch (`inputs/mod13a3_evi_2020/`) landed the same evening as this folder.

Mangrove (8) and Marshland/Swamp (9) are deliberately crosswalked to the SAME ESA class as Closed
Forest, mirroring what Becky's own table already does (her CN for both equals her Closed Forest CN
exactly) rather than using ESA's separate flooded-forest/flooded-herbaceous classes (160/170/180).
This is a known, still-open simplification — not a blocker, see "Mangrove/flooded-grasslands CN
patch" in docs/swy/research_notes.md — and low-stakes for typical downstream-beneficiary /
hydropower use cases of SWY specifically, since mangrove is at the tidal/estuarine end of the
watershed with essentially nothing further downstream of it.

Known gap, not resolved here: Becky's `INPUTS_SP/` folder and her `.ini` both omit a DEM path
entirely. Assumed (not confirmed) to be the standard SRTM product this project already uses
(`swy_philippines_run/02_fetch_dem.py`'s SRTMGL3 output) — flag this assumption to Becky in the
next communication rather than treating it as resolved.

Inputs:
    data/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif        (Becky's LULC, 12 classes)
    data/swy/philippines/INPUTS_SP/biophysical_template_PH_revised.csv    (Becky's table, source of
                                                                            passthrough columns)
    data/swy/philippines/inputs/mod13a3_evi_2020/*_EVI_*.tif and *_VI_Quality_*.tif
    data/swy/shared/cn_tables/gcn250_esa_lc_cn_table.csv                  (shared, region-independent)

Output:
    data/swy/philippines/INPUTS_SP/ph_biophysical_table_ncp_kc_cn.csv
"""
import glob
import json
import os

import numpy as np
import pandas as pd
import rasterio
from rasterio.warp import Resampling, reproject

INPUTS_SP_DIR = "data/swy/philippines/INPUTS_SP"
LULC_PATH = os.path.join(INPUTS_SP_DIR, "ph_baseline_lulc_md5_7f29da.tif")
BECKY_TABLE_PATH = os.path.join(INPUTS_SP_DIR, "biophysical_template_PH_revised.csv")
EVI_DIR = "data/swy/philippines/inputs/mod13a3_evi_2020"
CN_TABLE_PATH = "data/swy/shared/cn_tables/gcn250_esa_lc_cn_table.csv"
OUT_TABLE_PATH = os.path.join(INPUTS_SP_DIR, "ph_biophysical_table_ncp_kc_cn.csv")

# Becky's lulc_id -> nearest ESA CCI LC_ID, picked by land-cover semantics (not by which ESA CN
# number happens to sit numerically closest to hers). See this script's docstring for the
# mangrove/marshland/open-forest rationale.
BECKY_LULC_TO_ESA = {
    1: 10,    # Annual Crop -> Cropland, rainfed
    2: 120,   # Brush/Shrubs -> Shrubland
    3: 190,   # Built-up -> Urban areas
    4: 50,    # Closed Forest -> Tree cover, Br., Ev., closed to open (>15%)
    6: 130,   # Grassland -> Grassland
    8: 50,    # Mangrove Forest -> same ESA class as Closed Forest (see docstring)
    9: 50,    # Marshland/Swamp -> same ESA class as Closed Forest (see docstring)
    10: 50,   # Open Forest -> same ESA class as Closed Forest (ESA doesn't split closed/open
              # canopy for broadleaf evergreen, the dominant PH forest type — only for needleleaf
              # and broadleaf-deciduous types)
    11: 200,  # Open/Barren -> Bare areas
    12: 30,   # Perennial Crop -> Mosaic cropland (>50%)/natural vegetation (<50%)
}
WATER_LULC = {5, 7}  # Fishpond, Inland Water -> WATER_CN convention, not an ESA crosswalk
WATER_CN = 100

# Fixed Kc for non-vegetated classes, mirroring 07_build_biophysical_table.py's ESA-keyed
# FIXED_KC (190 urban ->0.10, 210 water ->1.05) for the equivalent Becky classes. Open/Barren (11)
# is deliberately NOT fixed here, matching the ESA script's own choice to let bare areas fall
# through to the EVI regression (self-corrects near zero via the regression's 0.05 floor).
FIXED_KC = {3: 0.10, 5: 1.05, 7: 1.05}

QA_GOOD_MAX = 0b01  # MOD13 VI_Quality bits 0-1: 00=good, 01=marginal usable, keep both


def compute_class_monthly_evi():
    """Zonal mean EVI per Becky's LULC class per month, QA-masked, on the EVI (1km) grid."""
    evi_files = sorted(glob.glob(f"{EVI_DIR}/MOD13A3.061__1_km_monthly_EVI_doy2020*.tif"))
    qa_files = sorted(glob.glob(f"{EVI_DIR}/MOD13A3.061__1_km_monthly_VI_Quality_doy2020*.tif"))
    assert len(evi_files) == 12 and len(qa_files) == 12, (
        f"expected 12 EVI + 12 QA files, got {len(evi_files)}/{len(qa_files)}"
    )

    with rasterio.open(evi_files[0]) as ref:
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

    classes = sorted(int(c) for c in np.unique(lulc_resampled) if c in BECKY_LULC_TO_ESA or c in FIXED_KC or c in WATER_LULC)
    monthly_class_evi = {c: [] for c in classes}

    for evi_f, qa_f in zip(evi_files, qa_files):
        with rasterio.open(evi_f) as esrc:
            evi = esrc.read(1).astype("float32")
            nodata = esrc.nodata
        with rasterio.open(qa_f) as qsrc:
            qa = qsrc.read(1)
        good = ((qa & 0b11) <= QA_GOOD_MAX) & (evi != nodata)
        evi_scaled = evi * 0.0001
        for c in classes:
            mask_c = good & (lulc_resampled == c)
            monthly_class_evi[c].append(
                float(evi_scaled[mask_c].mean()) if mask_c.sum() > 0 else np.nan
            )

    os.makedirs(INPUTS_SP_DIR, exist_ok=True)
    with open(os.path.join(INPUTS_SP_DIR, "ph_class_monthly_evi_becky_lulc.json"), "w") as f:
        json.dump(monthly_class_evi, f, indent=2)
    return monthly_class_evi


def build_biophysical_table(monthly_class_evi):
    cn_table = pd.read_csv(CN_TABLE_PATH).set_index("LC_ID")
    becky_table = pd.read_csv(BECKY_TABLE_PATH).set_index("lulc_id")

    df = becky_table.copy()

    for lulc_id in df.index:
        if lulc_id in WATER_LULC:
            cn_a, cn_b, cn_c, cn_d = (WATER_CN,) * 4
        else:
            esa_id = BECKY_LULC_TO_ESA[lulc_id]
            cn_row = cn_table.loc[esa_id]
            cn_a, cn_b, cn_c, cn_d = cn_row["CN_A"], cn_row["CN_B"], cn_row["CN_C"], cn_row["CN_D"]
        df.loc[lulc_id, ["CN_A", "CN_B", "CN_C", "CN_D"]] = [cn_a, cn_b, cn_c, cn_d]

        if lulc_id in FIXED_KC:
            kc = [FIXED_KC[lulc_id]] * 12
        else:
            vals = np.array(monthly_class_evi[lulc_id], dtype="float64")
            if np.isnan(vals).any() and not np.isnan(vals).all():
                vals[np.isnan(vals)] = np.nanmean(vals)
            kc = [max(0.05, round(1.457 * v - 0.1725, 3)) for v in vals]
        for m in range(1, 13):
            df.loc[lulc_id, f"Kc_{m}"] = round(kc[m - 1], 3)

    assert not df[["CN_A", "CN_B", "CN_C", "CN_D"]].isna().any().any(), "unfilled CN values remain"
    df = df.reset_index().sort_values("lulc_id")
    df.to_csv(OUT_TABLE_PATH, index=False)
    return df


if __name__ == "__main__":
    monthly_class_evi = compute_class_monthly_evi()
    df = build_biophysical_table(monthly_class_evi)
    print(df.to_string(index=False))
    print(f"\nWritten to {OUT_TABLE_PATH}")
