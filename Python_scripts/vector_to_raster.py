import argparse
import geopandas as gpd
import pandas as pd
import rasterio
from rasterio.features import rasterize
from rasterio.transform import from_bounds
import numpy as np
import os

# A mapping from string names to numpy dtypes
DTYPE_MAP = {
    'int16': np.int16,
    'int32': np.int32,
    'float32': np.float32,
    'float64': np.float64,
    'uint8': np.uint8,
}

def main(input_vector, output_dir, columns, resolution, target_crs, nodata, dtype_str):
    """
    Rasterizes specified columns from a vector file into GeoTIFFs.

    This is a general-purpose script to convert vector data attributes into
    raster format with configurable parameters.

    Args:
        input_vector (str): Path to the input vector file (e.g., GeoPackage).
        output_dir (str): Path to the output directory for GeoTIFFs.
        columns (list): A list of column names to rasterize.
        resolution (int): The resolution of the output raster in meters.
        target_crs (str): The target CRS for the output raster (e.g., 'EPSG:8857').
        nodata (float): The nodata value to use for the raster.
        dtype_str (str): The desired numpy data type for the raster as a string
                         (e.g., 'int16', 'float32').
    """
    if not columns:
        print("!!! ERROR: No columns specified for rasterization. Please use the --columns argument. !!!")
        return

    # --- Configuration ---
    print(f"Target columns for rasterization: {', '.join(columns)}")
    
    try:
        output_dtype = DTYPE_MAP[dtype_str]
        print(f"Output data type: {dtype_str}")
    except KeyError:
        print(f"!!! ERROR: Invalid dtype '{dtype_str}'. Must be one of {list(DTYPE_MAP.keys())}. !!!")
        return

    print(f"Reading vector data from: {input_vector}")
    gdf = gpd.read_file(input_vector)
    print(f"Vector data loaded. Source CRS detected: {gdf.crs.to_string()}")

    # --- CRS Handling ---
    # The script now assumes the input vector is ALREADY in the target CRS.
    # The --crs parameter is used to tag the output raster correctly.
    if not target_crs:
        target_crs = gdf.crs
        print(f"No target CRS specified. Using source CRS for output: {target_crs.to_string()}")
    elif gdf.crs != target_crs:
        print("!!! WARNING: Input vector CRS does not match the target CRS. !!!")
        print(f"Input CRS: {gdf.crs.to_string()}")
        print(f"Target CRS: {target_crs}")
        print("Results may be incorrect. Please reproject the input vector first using reproject_vector.py.")
    else:
        print(f"Input and target CRS match ({target_crs}). Proceeding.")

    # --- Raster Metadata Calculation ---
    xmin, ymin, xmax, ymax = gdf.total_bounds

    # --- GRID SNAPPING LOGIC ---
    # To ensure perfect alignment and prevent pixels from shifting relative to
    # the vector grid, we "snap" the output raster's origin to the resolution.
    # We define the transform from a snapped origin and fixed resolution, rather
    # than deriving it from the layer's total_bounds, which can introduce
    # floating-point errors and cause misalignment.

    snapped_xmin = np.floor(xmin / resolution) * resolution
    snapped_ymax = np.ceil(ymax / resolution) * resolution

    # The transform is now anchored to a snapped corner with a fixed pixel size.
    # The y-resolution is negative because raster coordinates originate from the top-left.
    transform = rasterio.transform.from_origin(snapped_xmin, snapped_ymax, resolution, -resolution)

    # Calculate the new width and height required to cover the original extent.
    width = int(np.ceil((xmax - snapped_xmin) / resolution))
    height = int(np.ceil((snapped_ymax - ymin) / resolution))

    os.makedirs(output_dir, exist_ok=True)

    # --- Main Processing Loop ---
    for column in columns:
        print(f"\n--- Processing column: {column} ---")

        if column not in gdf.columns:
            print(f"!!! WARNING: Column '{column}' not found in the input file. Skipping. !!!")
            print(f"Available columns are: {', '.join(gdf.columns)}")
            continue

        # Ensure the column is numeric and handle missing data
        print(f"Preparing column '{column}'...")
        # Using .loc to avoid SettingWithCopyWarning
        gdf.loc[:, column] = pd.to_numeric(gdf[column], errors='coerce').fillna(nodata).astype(output_dtype)

        print(f"Rasterizing '{column}' attribute...")
        # Forcing the generator into a list can sometimes resolve obscure iteration bugs.
        shapes = list(zip(gdf.geometry, gdf[column]))

        rasterized_data = rasterize(
            shapes=shapes,
            out_shape=(height, width),
            transform=transform,
            fill=nodata,  # Use the specified nodata value as the fill value
            dtype=output_dtype,
        )

        # --- DIAGNOSTIC STEP ---
        # Check the in-memory array before writing to disk.
        unique_pixels, counts = np.unique(rasterized_data, return_counts=True)
        print(f"--> DIAGNOSTIC: Unique pixel values in memory: {unique_pixels}")
        print(f"--> DIAGNOSTIC: Counts of unique values: {dict(zip(unique_pixels, counts))}")

        # --- Write Output ---
        profile = {
            'driver': 'GTiff',
            'dtype': output_dtype,
            'nodata': nodata,
            'width': width,
            'height': height,
            'count': 1,
            'crs': target_crs,
            'transform': transform,
        }

        output_raster = os.path.join(output_dir, f"{column}.tif")
        print(f"Writing output raster to: {output_raster}")

        with rasterio.open(output_raster, 'w', **profile) as dst:
            dst.write(rasterized_data, 1)

    print("\nProcess complete.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Rasterizes specified columns from a vector file into GeoTIFFs."
    )
    parser.add_argument("input_vector", help="Path to the input vector file (e.g., a GeoPackage).")
    parser.add_argument("output_dir", help="Path to the directory where output GeoTIFFs will be saved.")
    parser.add_argument("--columns", nargs='+', required=True, help="One or more column names to rasterize.")
    parser.add_argument("--resolution", type=int, default=10000, help="Output raster resolution in the units of the target CRS (default: 10000).")
    parser.add_argument("--crs", type=str, default=None, help="Target CRS to tag the output raster with (e.g., 'EPSG:8857'). Assumes input is already in this CRS.")
    parser.add_argument("--nodata", type=float, default=-9999.0, help="Nodata value for the output raster (default: -9999.0).")
    parser.add_argument("--dtype", type=str, default='float32', choices=list(DTYPE_MAP.keys()), help="Output raster data type (default: 'float32').")
    
    args = parser.parse_args()

    main(args.input_vector, args.output_dir, args.columns, args.resolution, args.crs, args.nodata, args.dtype)