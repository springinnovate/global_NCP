"""Combine the two independently-validated Kc corrections -- Negron Juarez forest
(07d_build_kc_forest_negronjuarez.py) and Oliveira grassland/shrub
(07e_build_kc_grassland_oliveira.py) -- into a single monthly Kc raster set, the
natural next step now that both have been tested in isolation and shown a real,
positive (forest) or real-but-weak (grassland/shrub) effect against WWF-SIPA's
baseline (see docs/swy/cn_kc_biome_coverage_registry.md, 2026-09-23 entries).

Both 07d and 07e already produce full-coverage 12-band rasters: their own target
class overridden with the literature-based Kc, every other class (including each
other's target classes) falling through to the SAME original all-Kamble table
(07b_build_biophysical_table_becky_inputs.py). So combining them is a straight
per-pixel selection, not a re-derivation:
    - Forest pixels (Closed Forest=4, Open Forest=10): take 07d's value
    - Grassland/shrub pixels (Brush/Shrubs=2, Grassland=6): take 07e's value
    - Everything else: identical in both inputs (the shared Kamble/fixed-value
      fallback) -- arbitrarily read from 07d's output since it's identical to 07e's
      there, confirmed by construction (both scripts share the same fallback table
      and neither touches the other's classes).

Output: data/swy/philippines/inputs/kc_combined_forest_grassland_2020/kc_MM.tif
"""
import os

import numpy as np
import rasterio
from rasterio.crs import CRS
from rasterio.enums import Resampling
from rasterio.warp import reproject

INPUTS_SP_DIR = "data/swy/philippines/INPUTS_SP"
LULC_PATH = os.path.join(INPUTS_SP_DIR, "ph_baseline_lulc_md5_7f29da.tif")
FOREST_KC_DIR = "data/swy/philippines/inputs/kc_forest_negronjuarez_2020"
GRASSLAND_KC_DIR = "data/swy/philippines/inputs/kc_grassland_oliveira_2020"
OUT_DIR = "data/swy/philippines/inputs/kc_combined_forest_grassland_2020"

FOREST_LULC_IDS = {4, 10}  # Closed Forest, Open Forest
GRASSLAND_LULC_IDS = {2, 6}  # Brush/Shrubs, Grassland


def main():
    os.makedirs(OUT_DIR, exist_ok=True)

    with rasterio.open(os.path.join(FOREST_KC_DIR, "kc_01.tif")) as ref:
        ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape
        ref_profile = ref.profile

    lulc_ref = np.empty(ref_shape, dtype="float32")
    with rasterio.open(LULC_PATH) as src:
        reproject(
            source=rasterio.band(src, 1), destination=lulc_ref,
            src_transform=src.transform, src_crs=src.crs,
            dst_transform=ref_transform, dst_crs=ref_crs,
            resampling=Resampling.mode,
        )
    forest_mask = np.isin(np.round(lulc_ref), list(FOREST_LULC_IDS))
    grassland_mask = np.isin(np.round(lulc_ref), list(GRASSLAND_LULC_IDS))
    print(f"forest pixels: {forest_mask.sum()}, grassland/shrub pixels: {grassland_mask.sum()}")

    for month in range(1, 13):
        forest_path = os.path.join(FOREST_KC_DIR, f"kc_{month:02d}.tif")
        grassland_path = os.path.join(GRASSLAND_KC_DIR, f"kc_{month:02d}.tif")

        with rasterio.open(forest_path) as fsrc:
            kc_forest_src = fsrc.read(1).astype("float32")
        with rasterio.open(grassland_path) as gsrc:
            kc_grassland_src = gsrc.read(1).astype("float32")

        # start from the forest-script output (identical to the grassland-script output
        # everywhere except each script's own target classes)
        kc_out = kc_forest_src.copy()
        apply_grassland = grassland_mask & np.isfinite(kc_grassland_src)
        kc_out[apply_grassland] = kc_grassland_src[apply_grassland]

        # sanity check: outside both target classes, the two source rasters should agree
        # exactly (same shared fallback table) -- confirm this isn't silently false
        other_mask = ~forest_mask & ~grassland_mask & np.isfinite(kc_forest_src) & np.isfinite(kc_grassland_src)
        max_diff = np.nanmax(np.abs(kc_forest_src[other_mask] - kc_grassland_src[other_mask]))
        print(f"month {month:02d}: max fallback disagreement outside both target classes = {max_diff:.6f}")

        out_path = os.path.join(OUT_DIR, f"kc_{month:02d}.tif")
        profile = ref_profile.copy()
        profile.update(dtype="float32", count=1, nodata=np.nan, compress="lzw", crs=ref_crs or CRS.from_epsg(4326))
        with rasterio.open(out_path, "w", **profile) as dst:
            dst.write(kc_out, 1)

    print(f"done -- see {OUT_DIR}")


if __name__ == "__main__":
    main()
