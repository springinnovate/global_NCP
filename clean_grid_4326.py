import geopandas as gpd
import os
import logging
import argparse
from shapely.ops import transform
from pyproj import Transformer

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

    LOGGER.info(f"Current CRS: {gdf.crs}. Safely reprojecting to EPSG:4326 feature-by-feature...")
    transformer = Transformer.from_crs(gdf.crs, "EPSG:4326", always_xy=True)

    bad = 0
    new_geoms = []
    for geom in gdf.geometry:
        try:
            g = transform(transformer.transform, geom)
            g = g.buffer(0).make_valid()
            if g.is_empty or not g.is_valid:
                bad += 1
                new_geoms.append(None)
            else:
                new_geoms.append(g)
        except Exception:
            bad += 1
            new_geoms.append(None)

    gdf["geometry"] = new_geoms
    gdf = gdf[gdf.geometry.notna()]
    gdf = gdf.set_crs("EPSG:4326", allow_override=True)

    LOGGER.info(f"Dropped {bad:,} invalid/unprojectable geometries.")
    LOGGER.info(f"Saving {len(gdf):,} safe cells to {out_path}...")
    gdf.to_file(out_path, driver="GPKG")
    LOGGER.info("Success!")

if __name__ == "__main__":
    main()