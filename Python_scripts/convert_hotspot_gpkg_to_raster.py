import argparse
import geopandas as gpd
import pandas as pd
import rasterio
from rasterio.features import rasterize
from rasterio.transform import from_bounds
import numpy as np
import os

def main(input_gpkg, output_dir, resolution=10000):
    """
    Rasterizes a fixed list of canonical service hotspot columns from a GeoPackage.

    This is a simplified, high-performance version of the script that targets
    a specific, hardcoded list of columns for rasterization.

    Args:
        input_gpkg (str): Path to the input GeoPackage file.
        output_dir (str): Path to the output directory for GeoTIFFs.
        resolution (int): The resolution of the output raster in meters.
    """
    # --- Configuration: Hardcoded list of columns to rasterize ---
    # This script is now specialized to rasterize only the 'hotspot_count' column.
    columns_to_rasterize = ["hotspot_count"]
    print(f"Target column for rasterization: {columns_to_rasterize[0]}")

    print(f"Reading vector data from: {input_gpkg}")
    gdf = gpd.read_file(input_gpkg)
    print("Vector data loaded.")

    print("Reprojecting to Equal Earth (EPSG:8857) for rasterization...")
    # Use an equal-area projection suitable for global analysis
    target_crs = "EPSG:8857"
    gdf = gdf.to_crs(target_crs)
    print("Reprojection complete.")

    # --- Raster Metadata Calculation (once for all columns) ---
    xmin, ymin, xmax, ymax = gdf.total_bounds

    # Calculate output dimensions
    width = int(np.ceil((xmax - xmin) / resolution))
    height = int(np.ceil((ymax - ymin) / resolution))

    # Create the affine transform
    transform = from_bounds(xmin, ymin, xmax, ymax, width, height)

    print(f"Output raster dimensions: {width} x {height}")
    os.makedirs(output_dir, exist_ok=True)

    # --- Main Processing Loop ---
    for column in columns_to_rasterize:
        print(f"\n--- Processing column: {column} ---")

        if column not in gdf.columns:
            print(f"!!! WARNING: Column '{column}' not found in GeoPackage. Skipping. !!!")
            print(f"Available columns are: {', '.join(gdf.columns)}")
            continue

        # Ensure the column is numeric. Using uint8 as hotspot_count is a small integer (0-8).
        print(f"Preparing column '{column}' for rasterization...")
        # Using .loc to avoid SettingWithCopyWarning
        gdf.loc[:, column] = pd.to_numeric(gdf[column], errors='coerce').fillna(0).astype(np.uint8)

        # --- DIAGNOSTIC STEP ---
        # Check what unique values the script is actually seeing before rasterizing.
        unique_values = gdf[column].unique()
        print(f"--> DIAGNOSTIC: Found unique values in '{column}' to be burned: {unique_values}")
        if len(unique_values) < 2:
            print("--> WARNING: Only one unique value found. The raster should be uniform.")

        print(f"Rasterizing '{column}' attribute...")
        shapes = ((geom, value) for geom, value in zip(gdf.geometry, gdf[column]))

        rasterized_data = rasterize(
            shapes=shapes,
            out_shape=(height, width),
            transform=transform,
            fill=0,  # Use 0 as the fill/nodata value
            dtype=rasterio.uint8,
            all_touched=True
        )

        # --- Write Output ---
        # For hotspot_count, setting nodata=0 means only cells with >0 hotspots are visible.
        profile = {
            'driver': 'GTiff',
            'dtype': rasterio.uint8,
            'nodata': 0,
            'width': width,
            'height': height,
            'count': 1,
            'crs': target_crs,
            'transform': transform,
            'compress': 'lzw'
        }

        output_raster = os.path.join(output_dir, f"{column}.tif")
        print(f"Writing output raster to: {output_raster}")

        with rasterio.open(output_raster, 'w', **profile) as dst:
            dst.write(rasterized_data, 1)

    print("\nProcess complete.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Rasterizes a predefined set of service hotspot columns from a GeoPackage."
    )
    parser.add_argument("input_gpkg", help="Path to the input GeoPackage file (e.g., hotspots_global_pct.gpkg).")
    parser.add_argument("output_dir", help="Path to the directory where output GeoTIFFs will be saved.")
    parser.add_argument("--resolution", type=int, default=10000, help="Output raster resolution in meters (default: 10000).")

    args = parser.parse_args()

    main(args.input_gpkg, args.output_dir, args.resolution)