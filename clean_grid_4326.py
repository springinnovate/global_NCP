import geopandas as gpd
import pandas as pd
import os
import logging
import argparse
import numpy as np

logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(levelname)s] %(message)s")
LOGGER = logging.getLogger(__name__)

def main():
    parser = argparse.ArgumentParser(description="Clean and reproject grid to EPSG:4326.")
    parser.add_argument(
        "--data-root",
        type=str,
        default=os.environ.get("GLOBAL_NCP_DATA", "/data"),
        help="Path to the global_ncp data directory."
    )
    args = parser.parse_args()
    
    data_root = args.data_root
    in_path = os.path.join(data_root, "vector_basedata", "landgrid_1_clean_enriched.gpkg")
    out_path = os.path.join(data_root, "vector_basedata", "landgrid_1_clean_enriched_4326.gpkg")

    LOGGER.info(f"Reading {in_path}...")
    gdf = gpd.read_file(in_path)

    LOGGER.info("Initial geometry validation...")
    gdf["geometry"] = gdf.geometry.buffer(0).make_valid()
    gdf = gdf[gdf.geometry.notna() & ~gdf.geometry.is_empty & gdf.geometry.is_valid]

    LOGGER.info(f"Current CRS: {gdf.crs}. Reprojecting to EPSG:4326 via chunking...")
    
    # Split into chunks to isolate poison polygons that crash the C-level to_crs()
    chunks = np.array_split(gdf, 200)
    safe_chunks = []
    bad = 0

    for i, chunk in enumerate(chunks):
        try:
            # Try the fast, vectorized C-level reprojection
            chunk_4326 = chunk.to_crs("EPSG:4326")
            
            # Fix dateline bowties that may have formed
            chunk_4326["geometry"] = chunk_4326.geometry.buffer(0).make_valid()
            chunk_4326 = chunk_4326[chunk_4326.geometry.notna() & ~chunk_4326.geometry.is_empty & chunk_4326.geometry.is_valid]
            
            safe_chunks.append(chunk_4326)
        except Exception as e:
            LOGGER.warning(f"Chunk {i+1} failed. Isolating poison polygons row-by-row...")
            # Poison polygon in this chunk. Isolate it.
            for idx, row in chunk.iterrows():
                try:
                    single = gpd.GeoDataFrame([row], crs=gdf.crs)
                    single_4326 = single.to_crs("EPSG:4326")
                    
                    single_4326["geometry"] = single_4326.geometry.buffer(0).make_valid()
                    single_4326 = single_4326[single_4326.geometry.notna() & ~single_4326.geometry.is_empty & single_4326.geometry.is_valid]
                    
                    if not single_4326.empty:
                        safe_chunks.append(single_4326)
                    else:
                        bad += 1
                except Exception:
                    bad += 1

    LOGGER.info("Recombining safe chunks...")
    if safe_chunks:
        final_gdf = pd.concat(safe_chunks, ignore_index=True)
        if not isinstance(final_gdf, gpd.GeoDataFrame):
            final_gdf = gpd.GeoDataFrame(final_gdf, geometry="geometry", crs="EPSG:4326")
    else:
        final_gdf = gpd.GeoDataFrame(columns=gdf.columns, geometry="geometry", crs="EPSG:4326")

    LOGGER.info(f"Dropped {bad:,} invalid/unprojectable geometries.")
    LOGGER.info(f"Saving {len(final_gdf):,} safe cells to {out_path}...")
    final_gdf.to_file(out_path, driver="GPKG")
    LOGGER.info("Success!")

if __name__ == "__main__":
    main()