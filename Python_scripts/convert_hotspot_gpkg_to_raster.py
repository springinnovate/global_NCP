import argparse
import geopandas as gpd
import pandas as pd
import rasterio
from rasterio.features import rasterize
from rasterio.transform import from_bounds
import numpy as np
import os

def rasterize_hotspots(input_gpkg, output_dir, variant_name):
    """
    Rasterizes hotspot_count and binary service columns from a GeoPackage.

    Args:
        input_gpkg (str): Path to the input GeoPackage file.
        output_dir (str): Path to the output directory for GeoTIFFs.
        variant_name (str): Either 'abs' or 'pct' for naming output files.
    """
    # Columns to rasterize
    service_columns = [
        "Nature_Access",
        "Sed_Ret_Ratio",
        "Pollination",
        "C_Risk",
        "C_Risk_Red_Ratio",
        "N_export",
        "N_Ret_Ratio",
        "Sed_export"
    ]

    columns_to_rasterize = ["hotspot_count"] + service_columns

    print(f"\n{'='*60}")
    print(f"Processing: {variant_name.upper()} - {os.path.basename(input_gpkg)}")
    print(f"{'='*60}")
    print(f"Target columns: {columns_to_rasterize}")

    # Load data
    print(f"\nReading vector data from: {input_gpkg}")
    gdf = gpd.read_file(input_gpkg, on_invalid="ignore")
    print(f"Total features loaded: {len(gdf)}")

    # Remove invalid geometries
    invalid_count = (~gdf.geometry.is_valid).sum()
    if invalid_count > 0:
        print(f"WARNING: Found {invalid_count} invalid geometries. Removing them.")
        gdf = gdf[gdf.geometry.is_valid]
    print(f"Using {len(gdf)} valid features for rasterization.")

    # Reproject to Equal Earth
    print("\nReprojecting to Equal Earth (EPSG:8857)...")
    target_crs = "EPSG:8857"
    gdf = gdf.to_crs(target_crs)

    # Calculate raster metadata once
    xmin, ymin, xmax, ymax = gdf.total_bounds
    actual_cell_area = np.median(gdf.geometry.area)
    actual_cell_size = np.sqrt(actual_cell_area)
    print(f"Detected cell size: {actual_cell_size:.1f} meters ({actual_cell_area/1e6:.2f} km²)")

    width = int(np.ceil((xmax - xmin) / actual_cell_size))
    height = int(np.ceil((ymax - ymin) / actual_cell_size))
    transform = from_bounds(xmin, ymin, xmax, ymax, width, height)

    print(f"Output raster dimensions: {width} x {height}")
    os.makedirs(output_dir, exist_ok=True)

    # Process each column
    for column in columns_to_rasterize:
        print(f"\n--- Processing column: {column} ---")

        if column not in gdf.columns:
            print(f"!!! WARNING: Column '{column}' not found. Skipping. !!!")
            print(f"Available columns: {', '.join(gdf.columns)}")
            continue

        # Prepare data
        gdf_temp = gdf.copy()
        gdf_temp[column] = pd.to_numeric(gdf_temp[column], errors='coerce').fillna(0).astype(np.uint8)

        # Check unique values
        unique_values = gdf_temp[column].unique()
        print(f"Unique values: {unique_values}")

        # Rasterize
        shapes = ((geom, value) for geom, value in zip(gdf_temp.geometry, gdf_temp[column]))
        rasterized_data = rasterize(
            shapes=shapes,
            out_shape=(height, width),
            transform=transform,
            fill=255,  # fill value for areas not covered by geometries
            dtype=rasterio.uint8
        )

        # Convert 0s to nodata (255)
        rasterized_data[rasterized_data == 0] = 255

        # Output filename
        output_raster = os.path.join(output_dir, f"{column}_{variant_name}.tif")
        print(f"Writing to: {output_raster}")

        profile = {
            'driver': 'GTiff',
            'dtype': rasterio.uint8,
            'nodata': 255,  # 255 is marked as nodata/NA
            'width': width,
            'height': height,
            'count': 1,
            'crs': target_crs,
            'transform': transform,
            'compress': 'lzw'
        }

        with rasterio.open(output_raster, 'w', **profile) as dst:
            dst.write(rasterized_data, 1)
        print(f"✓ Saved {column} raster")

def main():
    # Define inputs
    inputs = [
        ("C:/projects/global_NCP/data/processed/hotspots/abs/global/hotspots_global_abs.gpkg", "abs"),
        ("C:/projects/global_NCP/data/processed/hotspots/pct/global/hotspots_global_pct.gpkg", "pct"),
    ]

    output_dir = "C:/projects/global_NCP/data/processed/hotspots/rasters/"

    # Process both files
    for input_gpkg, variant in inputs:
        if os.path.exists(input_gpkg):
            rasterize_hotspots(input_gpkg, output_dir, variant)
        else:
            print(f"\n!!! ERROR: File not found: {input_gpkg}")

    print(f"\n{'='*60}")
    print("✓ All rasterization complete!")
    print(f"{'='*60}")

if __name__ == "__main__":
    main()
