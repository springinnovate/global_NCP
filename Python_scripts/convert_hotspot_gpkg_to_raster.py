import argparse
import geopandas as gpd
import pandas as pd
import rasterio
from rasterio.features import rasterize
from rasterio.transform import from_bounds
import numpy as np
import os

def main(input_gpkg, output_raster, resolution=10000):
    """
    Rasterizes the 'hotspot_count' attribute from a GeoPackage of grid cells.

    Args:
        input_gpkg (str): Path to the input GeoPackage file.
        output_raster (str): Path for the output GeoTIFF raster file.
        resolution (int): The resolution of the output raster in meters.
    """
    print(f"Reading vector data from: {input_gpkg}")
    gdf = gpd.read_file(input_gpkg)

    # --- Data Validation ---
    if 'hotspot_count' not in gdf.columns:
        raise ValueError("Input GeoPackage must contain a 'hotspot_count' column.")

    # Ensure the column is numeric, coercing errors
    gdf['hotspot_count'] = pd.to_numeric(gdf['hotspot_count'], errors='coerce').fillna(0).astype(np.int16)

    print("Reprojecting to Equal Earth (EPSG:8857) for rasterization...")
    # Use an equal-area projection suitable for global analysis
    target_crs = "EPSG:8857"
    gdf = gdf.to_crs(target_crs)

    # --- Raster Metadata Calculation ---
    xmin, ymin, xmax, ymax = gdf.total_bounds

    # Calculate output dimensions
    width = int(np.ceil((xmax - xmin) / resolution))
    height = int(np.ceil((ymax - ymin) / resolution))

    # Create the affine transform
    transform = from_bounds(xmin, ymin, xmax, ymax, width, height)

    print(f"Output raster dimensions: {width} x {height}")

    # --- Rasterization ---
    print("Rasterizing 'hotspot_count' attribute...")
    shapes = ((geom, value) for geom, value in zip(gdf.geometry, gdf.hotspot_count))

    # Burn the vector shapes into a raster array
    rasterized_data = rasterize(
        shapes=shapes,
        out_shape=(height, width),
        transform=transform,
        fill=0,  # Use 0 as the fill value, which will become NoData
        dtype=rasterio.int16,
        all_touched=True
    )

    # --- Write Output ---
    profile = {
        'driver': 'GTiff',
        'dtype': rasterio.int16,
        'nodata': 0, # Use 0 as the NoData value, since hotspot counts are >= 1
        'width': width,
        'height': height,
        'count': 1,
        'crs': target_crs,
        'transform': transform,
        'compress': 'lzw'
    }

    print(f"Writing output raster to: {output_raster}")
    os.makedirs(os.path.dirname(output_raster), exist_ok=True)

    with rasterio.open(output_raster, 'w', **profile) as dst:
        dst.write(rasterized_data, 1)

    print("Process complete.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert hotspot count vector layer to a raster.")
    parser.add_argument("input_gpkg", help="Path to the input GeoPackage file (e.g., hotspots_global_pct.gpkg).")
    parser.add_argument("output_raster", help="Path for the output GeoTIFF file.")
    parser.add_argument("--resolution", type=int, default=10000, help="Output raster resolution in meters (default: 10000).")

    args = parser.parse_args()

    main(args.input_gpkg, args.output_raster, args.resolution)