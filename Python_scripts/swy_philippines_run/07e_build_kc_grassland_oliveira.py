"""Build 12 monthly per-pixel Kc rasters using Oliveira et al. (2015)'s Cerrado savanna-woodland
ET model for Brush/Shrubs (lulc_id 2) and Grassland (lulc_id 6) -- Becky's own WWF-SIPA typology,
matching the registry's "Tropical & Subtropical Grasslands, Savannas & Shrublands" row. Every
other class falls through to the existing table-based Kc (07b's Kamble/EVI regression), unchanged.

Isolated-variable test, same discipline as 07d (forest): the fallback here is the ORIGINAL
all-Kamble table, not 07d's forest-modified output, so this result is cleanly attributable to the
grassland/shrub change alone, comparable apples-to-apples against the same all-Kamble baseline
forest was tested against. A combined (forest + grassland/shrub) run is a natural next step once
both are validated independently -- not this script's job.

Oliveira's equation (docs/swy/literature_pdfs/oliveira2015.pdf, eq. 1, fitted coefficients from
Table 3/eq. 5): ET = ETo[a(1-e^(-b*EVI)) - c], a=10.36, b=12.31, c=9.74. Since Kc is defined by
ET = Kc*ETo, Kc = a(1-e^(-b*EVI)) - c directly -- no separate ETo division needed (unlike Negron
Juarez, which gives ET as an additive Rn term, not a pure multiple of ETo). No new data layer
required either: EVI is already fetched, and ETo cancels out of the ratio entirely.

Real caveat carried over from the registry: Oliveira's calibration sites are savanna WOODLAND
(50-70% tree cover), physiognomically closer to Brush/Shrubs than to open Grassland -- applied to
both here since both fall in the same WWF biome cell and there's no better source for Grassland
specifically, but the fit is weaker for open grassland than for shrubland.

Output: data/swy/philippines/inputs/kc_grassland_oliveira_2020/kc_MM.tif
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
BIOPHYSICAL_TABLE_PATH = os.path.join(INPUTS_SP_DIR, "ph_biophysical_table_ncp_kc_cn.csv")
EVI_DIR = "data/swy/philippines/inputs/mod13a3_evi_2020"
OUT_DIR = "data/swy/philippines/inputs/kc_grassland_oliveira_2020"

GRASSLAND_LULC_IDS = {2, 6}  # Brush/Shrubs, Grassland (Becky's WWF-SIPA typology)
YEAR = 2020
QA_GOOD_MAX = 0b01

A, B, C = 10.36, 12.31, 9.74  # Oliveira et al. (2015), unmodified cerrado-woodland fit
KC_FLOOR = 0.05  # matches 07b/07d's own floor convention


def main():
    os.makedirs(OUT_DIR, exist_ok=True)

    evi_files = sorted(glob.glob(f"{EVI_DIR}/MOD13A3.061__1_km_monthly_EVI_doy{YEAR}*.tif"))
    qa_files = sorted(glob.glob(f"{EVI_DIR}/MOD13A3.061__1_km_monthly_VI_Quality_doy{YEAR}*.tif"))
    assert len(evi_files) == 12 and len(qa_files) == 12, (
        f"expected 12 EVI + 12 QA files, got {len(evi_files)}/{len(qa_files)}"
    )

    with rasterio.open(evi_files[0]) as ref:
        ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape
        ref_profile = ref.profile

    with rasterio.open(LULC_PATH) as src:
        lulc_ref = np.empty(ref_shape, dtype=src.dtypes[0])
        reproject(
            source=rasterio.band(src, 1), destination=lulc_ref,
            src_transform=src.transform, src_crs=src.crs,
            dst_transform=ref_transform, dst_crs=ref_crs,
            resampling=Resampling.mode,
        )
    grassland_mask = np.isin(np.round(lulc_ref), list(GRASSLAND_LULC_IDS))
    print(f"grassland/shrub pixels: {grassland_mask.sum()} / {grassland_mask.size}")

    table = pd.read_csv(BIOPHYSICAL_TABLE_PATH).set_index("lulc_id")

    for month in range(1, 13):
        with rasterio.open(evi_files[month - 1]) as esrc:
            evi = esrc.read(1).astype("float32") * 0.0001
            evi_nodata = esrc.nodata
        with rasterio.open(qa_files[month - 1]) as qsrc:
            qa = qsrc.read(1)
        evi_good = (qa & 0b11) <= QA_GOOD_MAX
        if evi_nodata is not None:
            evi_good &= evi != evi_nodata * 0.0001

        evi_clipped = np.clip(evi, 0.0, None)
        kc_grassland = A * (1 - np.exp(-B * evi_clipped)) - C
        kc_grassland = np.clip(kc_grassland, KC_FLOOR, None)

        kc_out = np.full(ref_shape, np.nan, dtype="float32")
        classes_present = [c for c in np.unique(np.round(lulc_ref)) if not np.isnan(c)]
        for lulc_id in classes_present:
            lulc_id_int = int(lulc_id)
            if lulc_id_int not in table.index:
                continue
            class_mask = np.round(lulc_ref) == lulc_id
            kc_out[class_mask] = table.loc[lulc_id_int, f"Kc_{month}"]

        apply_mask = grassland_mask & evi_good & np.isfinite(kc_grassland)
        kc_out[apply_mask] = kc_grassland[apply_mask]

        print(
            f"month {month:02d}: grassland/shrub Kc mean={np.nanmean(kc_out[grassland_mask]):.3f}"
        )

        out_path = os.path.join(OUT_DIR, f"kc_{month:02d}.tif")
        profile = ref_profile.copy()
        profile.update(dtype="float32", count=1, nodata=np.nan, compress="lzw", crs=ref_crs or CRS.from_epsg(4326))
        with rasterio.open(out_path, "w", **profile) as dst:
            dst.write(kc_out, 1)

    print(f"done -- see {OUT_DIR}")


if __name__ == "__main__":
    main()
