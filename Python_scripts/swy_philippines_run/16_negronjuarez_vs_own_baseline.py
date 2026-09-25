"""Direct comparison: this project's own prior run (09e, all-Kamble/EVI Kc everywhere) vs. the
Negron Juarez forest-Kc test (09i) -- how much did the Kc swap itself change our own output,
independent of WWF-SIPA's baseline (already covered by 15_negronjuarez_comparison.py).

Both runs share the exact same grid (same snapped DEM, same CN), so this is a direct pixel-to-pixel
comparison, no resampling needed -- simpler and more precise than the WWF-SIPA comparison.
"""
import numpy as np
import rasterio
from rasterio.enums import Resampling
from rasterio.warp import reproject

BEFORE_WORKSPACE = "data/swy/philippines/workspace_becky_inputs_90m_snapped"
BEFORE_SUFFIX = "ph_becky_inputs_90m_snapped"
AFTER_WORKSPACE = "data/swy/philippines/workspace_becky_inputs_90m_negronjuarez_kc"
AFTER_SUFFIX = "ph_negronjuarez_forest_kc"
LULC_PATH = f"{AFTER_WORKSPACE}/ph_baseline_lulc_wgs84.tif"

LULC_CLASSES = {
    1: "Annual Crop", 2: "Brush/Shrubs", 3: "Built-up", 4: "Closed Forest", 5: "Fishpond",
    6: "Grassland", 7: "Inland Water", 8: "Mangrove Forest", 9: "Marshland/Swamp",
    10: "Open Forest", 11: "Open/Barren", 12: "Perennial Crop",
}


def compare(var, before_path, after_path):
    with rasterio.open(before_path) as src:
        before = src.read(1).astype("float64")
        before_nodata = src.nodata
    with rasterio.open(after_path) as src:
        after = src.read(1).astype("float64")
        after_nodata = src.nodata
    assert before.shape == after.shape, f"shape mismatch: {before.shape} vs {after.shape}"

    # The LULC raster is the pre-clip, full-DEM-extent reprojection (not AOI-clipped like QF/B
    # are), so it's a different, larger grid -- resample it onto the QF/B grid (mode, categorical).
    with rasterio.open(after_path) as ref:
        ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape
    with rasterio.open(LULC_PATH) as src:
        lulc = np.empty(ref_shape, dtype=src.dtypes[0])
        reproject(
            source=rasterio.band(src, 1), destination=lulc,
            src_transform=src.transform, src_crs=src.crs,
            dst_transform=ref_transform, dst_crs=ref_crs,
            resampling=Resampling.mode,
        )

    valid = np.isfinite(before) & np.isfinite(after)
    if before_nodata is not None:
        valid &= ~np.isclose(before, before_nodata)
    if after_nodata is not None:
        valid &= ~np.isclose(after, after_nodata)

    b, a, lc = before[valid], after[valid], lulc[valid]
    n_identical = int(np.isclose(b, a, atol=1e-6).sum())
    diff = a - b
    pct_diff = np.where(b != 0, 100 * diff / b, np.nan)

    print(f"\n{var}: n_valid={b.size:,}  n_pixel-identical={n_identical:,} ({100*n_identical/b.size:.1f}%)")
    print(f"{var}: before mean={b.mean():.1f}  after mean={a.mean():.1f}  "
          f"change={a.mean()-b.mean():+.1f} ({100*(a.mean()-b.mean())/b.mean():+.1f}%)")

    print(f"{var} by LULC class (before, after, abs change, % change, n_pixels):")
    for code, name in LULC_CLASSES.items():
        m = lc == code
        if m.sum() < 100:
            continue
        bm, am = b[m].mean(), a[m].mean()
        print(f"  {name:18s} before={bm:8.1f}  after={am:8.1f}  "
              f"change={am-bm:+8.1f}  ({100*(am-bm)/bm:+6.1f}%)  n={m.sum():,}")


def main():
    compare(
        "QF",
        f"{BEFORE_WORKSPACE}/QF_{BEFORE_SUFFIX}.tif",
        f"{AFTER_WORKSPACE}/QF_{AFTER_SUFFIX}.tif",
    )
    compare(
        "B",
        f"{BEFORE_WORKSPACE}/B_{BEFORE_SUFFIX}.tif",
        f"{AFTER_WORKSPACE}/B_{AFTER_SUFFIX}.tif",
    )


if __name__ == "__main__":
    main()
