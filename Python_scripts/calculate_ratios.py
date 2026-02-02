"""
Calculate sediment and nitrogen retention ratios for 1992 and 2020.

This script reads raw raster data for USLE, sediment export, nitrogen retention,
and nitrogen export, then calculates the respective retention ratios. It saves
these new ratio rasters to a specified output directory and also computes the
bi-temporal difference between the 1992 and 2020 ratios.

This version uses a parallelized, tiled approach for speed and memory efficiency,
ensuring thread-safety by opening file handles within each thread.
"""

import os
import re
import logging
from pathlib import Path
import rasterio
from rasterio.vrt import WarpedVRT
from rasterio.enums import Resampling
import numpy as np
import concurrent.futures
import threading
from tqdm import tqdm

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# Paths
DATA_ROOT = Path("/home/jeronimo/data/global_ncp")
INPUT_DIR = DATA_ROOT / "raw/Spring/Inspring"
OUTPUT_DIR = DATA_ROOT / "raw/Spring/ratios"

# Ensure output directory exists
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# Define file identifiers for the rasters
FILE_IDENTIFIERS = {
    'usle': 'global_usle_marine_mod_ESA',
    'sed_export': 'global_sed_export_marine_mod_ESA',
    'n_retention': 'global_n_retention_ESAmar',
    'n_export': 'global_n_export_tnc_esa'
}

def find_raster_path(variable, year):
    """Finds a raster file path for a given variable and year."""
    identifier = FILE_IDENTIFIERS[variable]
    matches = list(INPUT_DIR.glob(f"*{identifier}*{year}*.tif"))
    if not matches:
        logging.warning(f"No file found for {variable} and {year}")
        return None
    if len(matches) > 1:
        logging.warning(f"Multiple files found for {variable} and {year}, using first one: {matches[0]}")
    return matches[0]

def calculate_and_save_ratio(raster1_path, raster2_path, output_path, formula, num_threads=None):
    """
    Calculates a ratio between two rasters using a parallelized tiling approach.
    """
    if not num_threads:
        num_threads = os.cpu_count() or 4

    if output_path.exists():
        logging.info(f"{output_path.name} already exists. Skipping.")
        return

    logging.info(f"Calculating {output_path.name} with {num_threads} threads.")

    # Open once to get metadata and log them
    with rasterio.open(raster1_path) as src1, rasterio.open(raster2_path) as src2:
        profile = src1.profile.copy()
        windows = [window for ji, window in src1.block_windows(1)]
        logging.info(f"Raster 1 ({raster1_path.name}) profile: {src1.profile}")
        logging.info(f"Raster 2 ({raster2_path.name}) profile: {src2.profile}")

    profile.update(dtype='float32', nodata=np.nan, compress='lzw', bigtiff='YES')

    write_lock = threading.Lock()

    with rasterio.open(output_path, 'w', **profile) as dst:
        def process_and_write(window):
            # Open files inside the thread for thread-safety
            with rasterio.open(raster1_path) as thread_src1, rasterio.open(raster2_path) as thread_src2:
                r1 = thread_src1.read(1, window=window, boundless=True).astype('float32')
                r2 = thread_src2.read(1, window=window, boundless=True).astype('float32')
                nodata1 = thread_src1.nodata
                nodata2 = thread_src2.nodata

            nodata_mask = (r1 == nodata1) | (r2 == nodata2) | np.isnan(r1) | np.isnan(r2)

            with np.errstate(divide='ignore', invalid='ignore'):
                if formula == 'sediment':
                    result = (r1 - r2) / r1  # (USLE - sed_export) / USLE
                elif formula == 'nitrogen':
                    result = r1 / (r1 + r2)  # n_retention / (n_retention + n_export)
                else:
                    result = np.full(r1.shape, np.nan, dtype='float32')

            result[nodata_mask] = np.nan
            result[np.isinf(result)] = np.nan

            with write_lock:
                dst.write(result.astype(rasterio.float32), 1, window=window)

        with concurrent.futures.ThreadPoolExecutor(max_workers=num_threads) as executor:
            list(tqdm(executor.map(process_and_write, windows), total=len(windows), desc=f"Processing {output_path.name}", unit="tile"))

def calculate_difference(path_early, path_late, output_path, num_threads=None):
    """Calculates late - early raster difference using a parallelized tiling approach."""
    if not num_threads:
        num_threads = os.cpu_count() or 4

    if output_path.exists():
        logging.info(f"{output_path.name} already exists. Skipping.")
        return

    logging.info(f"Calculating {output_path.name} with {num_threads} threads.")

    # Open once to get metadata
    with rasterio.open(path_early) as src_early:
        profile = src_early.profile.copy()
        windows = [window for ji, window in src_early.block_windows(1)]

    profile.update(dtype='float32', nodata=np.nan, compress='lzw', bigtiff='YES')

    write_lock = threading.Lock()

    with rasterio.open(output_path, 'w', **profile) as dst:
        def process_and_write(window):
            # Open files inside the thread for thread-safety
            with rasterio.open(path_early) as thread_src_early, rasterio.open(path_late) as thread_src_late:
                d_early = thread_src_early.read(1, window=window, boundless=True).astype('float32')
                d_late = thread_src_late.read(1, window=window, boundless=True).astype('float32')
                nodata_early = thread_src_early.nodata
                nodata_late = thread_src_late.nodata

            nodata_mask = (d_early == nodata_early) | (d_late == nodata_late) | np.isnan(d_early) | np.isnan(d_late)

            diff = d_late - d_early
            diff[nodata_mask] = np.nan

            with write_lock:
                dst.write(diff.astype(rasterio.float32), 1, window=window)

        with concurrent.futures.ThreadPoolExecutor(max_workers=num_threads) as executor:
            list(tqdm(executor.map(process_and_write, windows), total=len(windows), desc=f"Processing {output_path.name}", unit="tile"))

def check_raster_stats(raster_path):
    """Calculates basic statistics for a raster to identify potential outliers."""
    logging.info(f"Analyzing statistics for {raster_path.name}...")

    min_val = float('inf')
    max_val = float('-inf')

    try:
        with rasterio.open(raster_path) as src:
            nodata = src.nodata
            for _, window in src.block_windows(1):
                data = src.read(1, window=window)

                # Create a mask for valid data
                if nodata is not None:
                    mask = (data != nodata) & (~np.isnan(data))
                else:
                    mask = ~np.isnan(data)

                valid_data = data[mask]

                if valid_data.size > 0:
                    min_val = min(min_val, np.min(valid_data))
                    max_val = max(max_val, np.max(valid_data))

        if min_val == float('inf'):
            logging.warning(f"No valid data found in {raster_path.name}")
        else:
            logging.info(f"  Stats for {raster_path.name} -> Min: {min_val:.4f}, Max: {max_val:.4f}")
            if min_val < -1.0 or max_val > 1.0:
                 logging.warning(f"  WARNING: Values outside expected range [-1, 1] detected in {raster_path.name}!")

    except Exception as e:
        logging.error(f"Error checking stats for {raster_path.name}: {e}")

def main():
    """Main function to calculate ratios and differences."""
    max_threads = os.cpu_count() or 4

    # --- Calculate Ratios ---
    for year in [1992, 2020]:
        # Sediment Retention Ratio
        usle_path = find_raster_path('usle', year)
        sed_export_path = find_raster_path('sed_export', year)
        if usle_path and sed_export_path:
            output_path = OUTPUT_DIR / f"sed_retention_ratio_{year}.tif"
            calculate_and_save_ratio(usle_path, sed_export_path, output_path, 'sediment', num_threads=max_threads)

        # Nitrogen Retention Ratio
        n_retention_path = find_raster_path('n_retention', year)
        n_export_path = find_raster_path('n_export', year)
        if n_retention_path and n_export_path:
            output_path = OUTPUT_DIR / f"n_retention_ratio_{year}.tif"
            calculate_and_save_ratio(n_retention_path, n_export_path, output_path, 'nitrogen', num_threads=max_threads)

    # --- Calculate Differences ---
    # Sediment
    sed_ratio_1992 = OUTPUT_DIR / "sed_retention_ratio_1992.tif"
    sed_ratio_2020 = OUTPUT_DIR / "sed_retention_ratio_2020.tif"
    if sed_ratio_1992.exists() and sed_ratio_2020.exists():
        diff_output_path = OUTPUT_DIR / "sed_retention_ratio_diff_1992_2020.tif"
        calculate_difference(sed_ratio_1992, sed_ratio_2020, diff_output_path, num_threads=max_threads)

    # Nitrogen
    n_ratio_1992 = OUTPUT_DIR / "n_retention_ratio_1992.tif"
    n_ratio_2020 = OUTPUT_DIR / "n_retention_ratio_2020.tif"
    if n_ratio_1992.exists() and n_ratio_2020.exists():
        diff_output_path = OUTPUT_DIR / "n_retention_ratio_diff_1992_2020.tif"
        calculate_difference(n_ratio_1992, n_ratio_2020, diff_output_path, num_threads=max_threads)

    # --- Check Statistics ---
    logging.info("--- Checking Output Statistics ---")
    diff_output_sed = OUTPUT_DIR / "sed_retention_ratio_diff_1992_2020.tif"
    diff_output_n = OUTPUT_DIR / "n_retention_ratio_diff_1992_2020.tif"

    if diff_output_sed.exists():
        check_raster_stats(diff_output_sed)

    if diff_output_n.exists():
        check_raster_stats(diff_output_n)

    logging.info("All calculations finished.")

if __name__ == "__main__":
    main()