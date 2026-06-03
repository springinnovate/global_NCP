import geopandas as gpd
import os
import logging
import argparse

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

    LOGGER.info(f"Current CRS: {gdf.crs}. Reprojecting to EPSG:4326...")
    gdf = gdf.to_crs("EPSG:4326")

    LOGGER.info("Fixing dateline wraps and validating geometry...")
    gdf["geometry"] = gdf.geometry.buffer(0).make_valid()
    gdf = gdf[~gdf.geometry.is_empty & gdf.geometry.is_valid]

    LOGGER.info(f"Saving {len(gdf)} safe cells to {out_path}...")
    gdf.to_file(out_path, driver="GPKG")
    LOGGER.info("Success!")

if __name__ == "__main__":
    main()