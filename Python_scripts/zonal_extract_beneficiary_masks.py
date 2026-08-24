"""Zonal-extract Rich's fine-resolution beneficiary union-coverage masks onto the
10km analysis grid (grid_fid), producing a per-cell coverage fraction for Phase 4
(Gini/HDI/GDP disproportionality test on beneficiary masks).

Run inside Rich's Docker environment (therealspring/global_ncp-computational-environment,
geopy311 conda env: exactextract 0.2.2, gdal 3.10.3, geopandas 1.0.1, rasterio 1.4.3) --
the canonical environment this project's other zonal-extraction pipelines
(summary_pipeline_landgrid.py) already run in.

Two deliberate departures from summary_pipeline_landgrid.py's generic YAML-driven
pipeline, both required for this specific input:

1. `full_raster_extent_union_coverage.tif` files ship with nodata=0, which is also
   the "not covered" pixel value. exact_extract (via GDAL) silently drops all
   0-valued pixels as nodata and would compute `mean` only over the remaining
   (always-1) pixels -- giving a meaningless, always-1.0 "coverage fraction".
   Fixed by wrapping each source raster in a tiny VRT that omits the
   <NoDataValue> element, so both 0 and 1 are treated as real data and `mean`
   becomes the true area-weighted fraction of the polygon covered by the mask.

2. summary_pipeline_landgrid.py's zonal_stats() falls back to a purely
   positional fid (`gdf.index + 1`) whenever the input vector lacks a column
   literally named "fid" -- the same failure class flagged in
   analysis/WORKLOG.md (2026-07-28 entry, `landgrid_1_clean_enriched_4326.gpkg`
   has no real ID column). To avoid it, this script targets
   `data/processed/10k_change_calc.gpkg` directly (has a real `grid_fid`
   column, already verified against the canonical hotspot outputs) and keeps
   that column name explicitly through exact_extract's `include_cols`, never
   falling back to row position.
"""

from pathlib import Path
import time

import geopandas as gpd
import pandas as pd
import rasterio
from exactextract import exact_extract
from exactextract.raster import RasterioRasterSource

REPO_ROOT = Path(__file__).resolve().parents[1]
GRID_GPKG = REPO_ROOT / "data" / "processed" / "10k_change_calc.gpkg"
RASTERS_DIR = REPO_ROOT / "data" / "processed" / "hotspots_5service" / "rasters_5_var"
OUT_CSV = REPO_ROOT / "data" / "processed" / "tables" / "beneficiary_mask_coverage_10km.csv"

# folder-name suffix -> short category label used downstream in the KS test
CATEGORIES = {
    "water_overlap_downstream_beneficiaries": "water",
    "access_overlap_travel_time_beneficiaries": "access",
    "combined_cross_category_beneficiaries": "combined_cross",
    "hotspot_count_1plus_beneficiaries": "tier_1plus",
    "hotspot_count_2plus_beneficiaries": "tier_2plus",
    "hotspot_count_3plus_beneficiaries": "tier_3plus",
    "hotspot_count_4plus_beneficiaries": "tier_4plus",
    "hotspot_count_5plus_beneficiaries": "tier_5plus",
}

FOLDER_PREFIX = "output_jeronimo_2026_07_29_18_49_00_"


def build_nodata_free_vrt(src_path: Path, vrt_path: Path) -> None:
    """Write a VRT wrapping `src_path` with no <NoDataValue> declared."""
    with rasterio.open(src_path) as src:
        width, height = src.width, src.height
        gt = src.transform
        wkt = src.crs.to_wkt()
        bxsize, bysize = src.block_shapes[0]

    vrt_xml = f"""<VRTDataset rasterXSize="{width}" rasterYSize="{height}">
  <SRS>{wkt}</SRS>
  <GeoTransform>{gt.c}, {gt.a}, {gt.b}, {gt.f}, {gt.d}, {gt.e}</GeoTransform>
  <VRTRasterBand dataType="Byte" band="1">
    <SimpleSource>
      <SourceFilename relativeToVRT="0">{src_path.as_posix()}</SourceFilename>
      <SourceBand>1</SourceBand>
      <SourceProperties RasterXSize="{width}" RasterYSize="{height}" DataType="Byte" BlockXSize="{bxsize}" BlockYSize="{bysize}"/>
      <SrcRect xOff="0" yOff="0" xSize="{width}" ySize="{height}"/>
      <DstRect xOff="0" yOff="0" xSize="{width}" ySize="{height}"/>
    </SimpleSource>
  </VRTRasterBand>
</VRTDataset>
"""
    vrt_path.write_text(vrt_xml)


def zonal_coverage_fraction(gdf: gpd.GeoDataFrame, raster_path: Path, vrt_path: Path) -> pd.DataFrame:
    build_nodata_free_vrt(raster_path, vrt_path)
    ds = rasterio.open(vrt_path)
    assert ds.nodata is None, "VRT nodata override failed"
    rast = RasterioRasterSource(ds, band_idx=1, name="cov")

    extract_kwargs = dict(
        rast=rast,
        vec=gdf,
        ops=["mean"],
        include_cols=["grid_fid"],
        output="pandas",
    )
    if len(gdf) > 500_000:
        extract_kwargs["strategy"] = "raster-sequential"
        extract_kwargs["max_cells_in_memory"] = 30_000_000 * 4

    stats_df = exact_extract(**extract_kwargs)
    ds.close()
    return stats_df


def main():
    print(f"Loading grid geometry from {GRID_GPKG} ...")
    t0 = time.time()
    gdf = gpd.read_file(GRID_GPKG, columns=["grid_fid"])
    print(f"  {len(gdf)} grid cells loaded in {time.time()-t0:.1f}s")
    assert gdf["grid_fid"].is_unique, "grid_fid is not unique in 10k_change_calc.gpkg"

    workspace = REPO_ROOT / "data" / "processed" / "tmp_vrt"
    workspace.mkdir(parents=True, exist_ok=True)

    results = []
    for folder_suffix, label in CATEGORIES.items():
        raster_path = RASTERS_DIR / f"{FOLDER_PREFIX}{folder_suffix}" / "full_raster_extent_union_coverage.tif"
        if not raster_path.exists():
            print(f"  SKIP {label}: {raster_path} not found")
            continue
        vrt_path = workspace / f"{label}_union_coverage_nodatafree.vrt"

        t1 = time.time()
        stats_df = zonal_coverage_fraction(gdf, raster_path, vrt_path)
        stats_df["mean"] = stats_df["mean"].fillna(0.0)  # cells outside raster extent -> not covered
        stats_df["category"] = label
        stats_df = stats_df.rename(columns={"mean": "coverage_frac"})
        results.append(stats_df[["grid_fid", "category", "coverage_frac"]])
        print(f"  {label}: done in {time.time()-t1:.1f}s "
              f"(n>0: {(stats_df['coverage_frac']>0).sum()}, n>=0.5: {(stats_df['coverage_frac']>=0.5).sum()})")

    out = pd.concat(results, ignore_index=True)
    OUT_CSV.parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(OUT_CSV, index=False)
    print(f"Wrote {len(out)} rows to {OUT_CSV} in {time.time()-t0:.1f}s total")


if __name__ == "__main__":
    main()
