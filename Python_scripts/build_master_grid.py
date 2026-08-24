import geopandas as gpd
import pandas as pd
import os
import logging
import sys
import argparse

# --- Boilerplate ---
logging.basicConfig(
    level=logging.INFO,
    stream=sys.stdout,
    format="%(asctime)s [%(levelname)s] %(message)s",
)
LOGGER = logging.getLogger(__name__)

def fix_geometries(gdf, name=""):
    LOGGER.info(f"Validating geometries in {name}...")
    invalid_count = (~gdf.geometry.is_valid).sum()
    if invalid_count > 0:
        LOGGER.warning(f"  Found {invalid_count} invalid geometries. Attempting to fix...")
        gdf["geometry"] = gdf.geometry.buffer(0).make_valid()
        gdf = gdf[~gdf.geometry.is_empty].copy()
        still_invalid = (~gdf.geometry.is_valid).sum()
        if still_invalid > 0:
            LOGGER.warning(f"  Still {still_invalid} invalid geometries after fixing. Removing them.")
            gdf = gdf[gdf.geometry.is_valid].copy()
    LOGGER.info(f"  ✓ {len(gdf)} valid geometries remaining in {name}")
    return gdf

def main():
    parser = argparse.ArgumentParser(description="Enrich, clean, and reproject base QGIS grid to EPSG:4326.")
    parser.add_argument("input_grid", type=str, help="Path to the raw QGIS land grid (e.g., landgrid_1.gpkg).")
    parser.add_argument("--data-root", type=str, default=os.environ.get("GLOBAL_NCP_DATA", os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "data")), help="Path to data directory.")
    parser.add_argument("--output-file", type=str, default=None, help="Path to save the final EPSG:4326 GeoPackage.")
    args = parser.parse_args()

    LOGGER.info("=" * 60)
    LOGGER.info("STEP 1: Loading raw grid...")
    LOGGER.info("=" * 60)
    if not os.path.exists(args.input_grid):
        raise FileNotFoundError(f"Input grid not found: {args.input_grid}")
    
    grid = gpd.read_file(args.input_grid)
    LOGGER.info(f"  ✓ Loaded {len(grid):,} grid cells from {os.path.basename(args.input_grid)}")
    
    LOGGER.info("\n" + "=" * 60)
    LOGGER.info("STEP 2: Joining Biome attributes...")
    LOGGER.info("=" * 60)
    vector_basedata_path = os.path.join(args.data_root, "vector_basedata")
    biomes = gpd.read_file(os.path.join(vector_basedata_path, "Biome.gpkg"), on_invalid="ignore")
    biomes = fix_geometries(biomes, "Biomes")
    
    if grid.crs != biomes.crs:
        biomes = biomes.to_crs(grid.crs)

    grid_for_join = grid.copy()
    grid_for_join["geometry"] = grid_for_join.geometry.representative_point()

    enriched_grid = gpd.sjoin(grid_for_join, biomes[['BIOME', 'WWF_biome', 'geometry']], how="left", predicate="within")
    enriched_grid = enriched_grid[~enriched_grid.index.duplicated(keep='first')].drop(columns=['index_right'], errors='ignore')

    LOGGER.info("\n" + "=" * 60)
    LOGGER.info("STEP 3: Joining Correspondence (Country/Region) attributes...")
    LOGGER.info("=" * 60)
    correspondence = gpd.read_file(os.path.join(vector_basedata_path, "cartographic_ee_r264_correspondence.gpkg"), on_invalid="ignore")
    correspondence = fix_geometries(correspondence, "Correspondence")

    if enriched_grid.crs != correspondence.crs:
        correspondence = correspondence.to_crs(enriched_grid.crs)

    correspondence['c_fid'] = correspondence.get('fid', correspondence.get('id', correspondence.index))
    cols_to_join = ['c_fid', 'nev_name', 'continent', 'region_un', 'region_wb', 'subregion', 'income_grp', 'geometry']
    
    enriched_grid = gpd.sjoin(enriched_grid, correspondence[cols_to_join], how="left", predicate="within")
    enriched_grid = enriched_grid[~enriched_grid.index.duplicated(keep='first')].rename(columns={'nev_name': 'country'})

    LOGGER.info("\n" + "=" * 60)
    LOGGER.info("STEP 4: Restoring geometries and verifying area...")
    LOGGER.info("=" * 60)
    new_cols = [c for c in enriched_grid.columns if c not in grid.columns and c != 'index_right']
    enriched_grid = grid.merge(enriched_grid[new_cols], left_index=True, right_index=True, how="left")
    enriched_grid = fix_geometries(enriched_grid, "Grid after Joins")

    LOGGER.info("\n" + "=" * 60)
    LOGGER.info("STEP 5: Safely reprojecting to EPSG:4326 via Pandas Chunking...")
    LOGGER.info("=" * 60)
    
    # Replacing numpy split with native pandas slicing to remove swapaxes deprecation warnings
    chunk_size = len(enriched_grid) // 200 + 1
    chunks = [enriched_grid.iloc[i:i+chunk_size] for i in range(0, len(enriched_grid), chunk_size)]
    safe_chunks, bad = [], 0

    for i, chunk in enumerate(chunks):
        try:
            chunk_4326 = chunk.to_crs("EPSG:4326")
            chunk_4326["geometry"] = chunk_4326.geometry.buffer(0).make_valid()
            chunk_4326 = chunk_4326[chunk_4326.geometry.notna() & ~chunk_4326.geometry.is_empty & chunk_4326.geometry.is_valid]
            safe_chunks.append(chunk_4326)
        except Exception:
            LOGGER.warning(f"  Chunk {i+1}/{len(chunks)} failed fast-path. Isolating poison polygons row-by-row...")
            for _, row in chunk.iterrows():
                try:
                    single = gpd.GeoDataFrame([row], crs=enriched_grid.crs).to_crs("EPSG:4326")
                    single["geometry"] = single.geometry.buffer(0).make_valid()
                    single = single[single.geometry.notna() & ~single.geometry.is_empty & single.geometry.is_valid]
                    if not single.empty: safe_chunks.append(single)
                    else: bad += 1
                except Exception:
                    bad += 1

    final_gdf = pd.concat(safe_chunks, ignore_index=True) if safe_chunks else gpd.GeoDataFrame(columns=enriched_grid.columns, geometry="geometry", crs="EPSG:4326")
    LOGGER.info(f"  Dropped {bad:,} invalid/unprojectable geometries.")

    LOGGER.info("\n" + "=" * 60)
    LOGGER.info("STEP 6: Saving final master grid...")
    LOGGER.info("=" * 60)
    out_path = args.output_file or os.path.join(args.data_root, "vector_basedata", "landgrid_1_clean_enriched_4326.gpkg")
    final_gdf.to_file(out_path, driver="GPKG")
    LOGGER.info(f"✓ Success! Saved {len(final_gdf):,} cells to: {out_path}")

if __name__ == "__main__":
    main()