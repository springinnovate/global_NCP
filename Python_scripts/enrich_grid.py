import geopandas as gpd
import pandas as pd
import os
import logging
import sys
import argparse
import shapely

# --- Boilerplate from other scripts ---
logging.basicConfig(
    level=logging.INFO,
    stream=sys.stdout,
    format="%(asctime)s [%(levelname)s] %(message)s",
)
LOGGER = logging.getLogger(__name__)

def fix_geometries(gdf, name=""):
    """
    Validate and fix invalid geometries in a GeoDataFrame.
    """
    LOGGER.info(f"Validating geometries in {name}...")
    invalid_count = (~gdf.geometry.is_valid).sum()

    if invalid_count > 0:
        LOGGER.warning(f"  Found {invalid_count} invalid geometries in {name}. Attempting to fix...")
        gdf.geometry = gdf.geometry.buffer(0).make_valid()
        gdf = gdf[~gdf.geometry.is_empty].copy()
        still_invalid = (~gdf.geometry.is_valid).sum()
        if still_invalid > 0:
            LOGGER.warning(f"  Still {still_invalid} invalid geometries after fixing. Removing them.")
            gdf = gdf[gdf.geometry.is_valid].copy()

    LOGGER.info(f"  ✓ {len(gdf)} valid geometries remaining in {name}")
    return gdf

# --- Main logic ---

def main():
    parser = argparse.ArgumentParser(description="Enrich a clean grid with attributes from other layers.")
    parser.add_argument(
        "input_grid",
        type=str,
        help="Path to the clean input grid GeoPackage file.",
    )
    parser.add_argument(
        "--data-root",
        type=str,
        default=os.environ.get("GLOBAL_NCP_DATA"),
        help="Path to the global_ncp data directory.",
    )
    parser.add_argument(
        "--output-file",
        type=str,
        default=None,
        help="Path to save the enriched GeoPackage file.",
    )
    args = parser.parse_args()

    # --- 1. Load Input Grid ---
    LOGGER.info("=" * 60)
    LOGGER.info("STEP 1: Loading clean grid...")
    LOGGER.info("=" * 60)
    if not os.path.exists(args.input_grid):
        raise FileNotFoundError(f"Input grid not found: {args.input_grid}")
    
    grid = gpd.read_file(args.input_grid)
    LOGGER.info(f"  ✓ Loaded {len(grid)} grid cells from {os.path.basename(args.input_grid)}")
    
    # --- 2. Load and Join Biomes ---
    LOGGER.info("\n" + "=" * 60)
    LOGGER.info("STEP 2: Joining Biome attributes...")
    LOGGER.info("=" * 60)
    
    vector_basedata_path = os.path.join(args.data_root, "vector_basedata")
    biome_path = os.path.join(vector_basedata_path, "Biome.gpkg")
    if not os.path.exists(biome_path):
        raise FileNotFoundError(f"Biome file not found: {biome_path}")

    biomes = gpd.read_file(biome_path, on_invalid="ignore")
    biomes = fix_geometries(biomes, "Biomes")
    
    # Ensure CRS match
    if grid.crs != biomes.crs:
        LOGGER.info(f"  Reprojecting biomes to match grid CRS ({grid.crs.to_string()})")
        biomes = biomes.to_crs(grid.crs)

    LOGGER.info("  Performing spatial join with Biomes (using centroids for performance)...")
    # To significantly speed up the spatial join, we convert the grid polygons
    # to their center points and perform a point-in-polygon test.
    grid_for_join = grid.copy()
    # Using representative_point() is safer than centroid as it's guaranteed to be within the polygon.
    grid_for_join.geometry = grid_for_join.geometry.representative_point()

    # Perform the join. The result will have point geometries.
    enriched_grid = gpd.sjoin(grid_for_join, biomes[['BIOME', 'WWF_biome', 'geometry']], how="left", predicate="within")
    
    # sjoin can create duplicates if a grid cell intersects multiple biomes.
    # We'll keep the first match.
    enriched_grid = enriched_grid[~enriched_grid.index.duplicated(keep='first')]
    LOGGER.info(f"  ✓ Grid now has {len(enriched_grid)} cells after biome join.")

    # Drop the 'index_right' column created by the first sjoin to prevent
    # a name clash in the next join.
    enriched_grid = enriched_grid.drop(columns=['index_right'], errors='ignore')

    # --- 3. Load and Join Correspondence Layer ---
    LOGGER.info("\n" + "=" * 60)
    LOGGER.info("STEP 3: Joining Correspondence attributes...")
    LOGGER.info("=" * 60)
    
    correspondence_path = os.path.join(vector_basedata_path, "cartographic_ee_r264_correspondence.gpkg")
    if not os.path.exists(correspondence_path):
        raise FileNotFoundError(f"Correspondence file not found: {correspondence_path}")

    correspondence = gpd.read_file(correspondence_path, on_invalid="ignore")
    correspondence = fix_geometries(correspondence, "Correspondence")

    if enriched_grid.crs != correspondence.crs:
        LOGGER.info(f"  Reprojecting correspondence layer to match grid CRS ({enriched_grid.crs.to_string()})")
        correspondence = correspondence.to_crs(enriched_grid.crs)

    LOGGER.info("  Performing spatial join with Correspondence layer (using centroids for performance)...")
    # The `enriched_grid` currently has point geometries, so this join will also be fast.

    # Capture the original feature ID of the correspondence layer for future tabular joins
    if 'fid' in correspondence.columns:
        correspondence['c_fid'] = correspondence['fid']
    elif 'id' in correspondence.columns:
        correspondence['c_fid'] = correspondence['id']
    else:
        correspondence['c_fid'] = correspondence.index

    # Select columns to join
    cols_to_join = ['c_fid', 'nev_name', 'continent', 'region_un', 'region_wb', 'subregion', 'income_grp', 'geometry']
    enriched_grid = gpd.sjoin(enriched_grid, correspondence[cols_to_join], how="left", predicate="within")
    enriched_grid = enriched_grid[~enriched_grid.index.duplicated(keep='first')]
    LOGGER.info(f"  ✓ Grid now has {len(enriched_grid)} cells after correspondence join.")

    # Rename 'nev_name' to 'country' for compatibility with downstream scripts
    if 'nev_name' in enriched_grid.columns:
        LOGGER.info("  Renaming 'nev_name' to 'country' for compatibility.")
        enriched_grid = enriched_grid.rename(columns={'nev_name': 'country'})

    # --- 4. Restore Original Polygon Geometries ---
    LOGGER.info("\n" + "=" * 60)
    LOGGER.info("STEP 4: Restoring original polygon geometries...")
    LOGGER.info("=" * 60)
    # At this point, `enriched_grid` has all the attributes, but POINT geometries.
    # We drop the point geometry and merge the attributes back to the original `grid`
    # which still has the correct POLYGON geometries. The index is preserved.

    # Identify only the newly added columns to prevent _x and _y duplicate artifacts
    new_cols = [c for c in enriched_grid.columns if c not in grid.columns and c != 'index_right']

    enriched_grid = grid.merge(
        enriched_grid[new_cols], left_index=True, right_index=True, how="left"
    )
    enriched_grid = fix_geometries(enriched_grid, "Grid after Correspondence Join")

    # --- 5. Final Verification ---
    LOGGER.info("\n" + "=" * 60)
    LOGGER.info("STEP 5: Final area verification...")
    LOGGER.info("=" * 60)
    
    LOGGER.info(f"  Grid CRS is: {enriched_grid.crs}")
    if not enriched_grid.crs.is_projected:
        LOGGER.warning("  CRS is not projected. Area calculation may not be in meters.")
    
    try:
        enriched_grid['area_m2'] = enriched_grid.geometry.area
        min_area = enriched_grid['area_m2'].min()
        max_area = enriched_grid['area_m2'].max()
        mean_area = enriched_grid['area_m2'].mean()
        
        LOGGER.info(f"  Final Area stats (m^2):")
        LOGGER.info(f"    Min:  {min_area:,.2f}")
        LOGGER.info(f"    Max:  {max_area:,.2f}")
        LOGGER.info(f"    Mean: {mean_area:,.2f}")
        
        target_area_m2 = 100 * 1000 * 1000
        if abs(mean_area - target_area_m2) / target_area_m2 < 0.001:
             LOGGER.info("  ✓ Area verification successful. All grid cells are ~100 km².")
        else:
            LOGGER.warning("  ✗ Area verification failed. Some grid cells are not 100 km².")

        enriched_grid = enriched_grid.drop(columns=['area_m2', 'index_right'], errors='ignore')
    except Exception as e:
        LOGGER.error(f"  Final area verification failed: {e}")

    # --- 6. Save Enriched Grid ---
    LOGGER.info("\n" + "=" * 60)
    LOGGER.info("STEP 6: Saving enriched grid...")
    LOGGER.info("=" * 60)

    output_path = args.output_file
    if not output_path:
        base, _ = os.path.splitext(args.input_grid)
        output_path = f"{base}_enriched.gpkg"
        
    try:
        LOGGER.info(f"  Saving enriched grid to: {output_path}")
        enriched_grid.to_file(output_path, driver="GPKG")
        LOGGER.info("  ✓ Save successful.")
    except Exception as e:
        LOGGER.error(f"  ✗ Final save of the enriched grid failed: {e}")
        raise

    LOGGER.info("\n✓ Enrichment complete!")

if __name__ == "__main__":
    main()