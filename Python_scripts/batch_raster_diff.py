"""
Batch calculate raster differences (2020 - 1992) for Global NCP analysis.

This script scans the input directory for files containing '1992', attempts to find
a matching '2020' file, calculates the difference (2020 - 1992), and saves the
result with a simplified filename.
"""

import os
import re
import logging
from pathlib import Path
import rasterio
import numpy as np
from rasterio.windows import Window
import concurrent.futures
import threading
from tqdm import tqdm

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# Paths
DATA_ROOT = Path("/home/jeronimo/data/global_ncp")
INPUT_DIR = DATA_ROOT / "raw/Spring/Inspring"
OUTPUT_DIR = DATA_ROOT / "2020_1992_chg"

def get_clean_variable_name(filename):
    """
    Simplifies filename to extract the core variable name.
    Removes years, hashes, and project-specific suffixes.
    """
    name = filename.lower()

    # Remove extension
    if name.endswith('.tif'):
        name = name[:-4]

    # Remove years and everything after (often hashes or compression info)
    # We look for the first occurrence of 1992 or 2020
    match = re.search(r'(.*)(1992|2020)', name)
    if match:
        name = match.group(1)

    # List of strings to remove to clean up the name
    remove_patterns = [
        "global_",
        "marine_mod_esa",
        "tnc_esa",
        "esamar",
        "lspop2019_esa",
        "realized_",
        "_on_ag_esa",
        "_fertilizer",
        "compressed",
        "md5"
    ]

    for pattern in remove_patterns:
        name = name.replace(pattern, "")

    # Clean up underscores
    name = re.sub(r'_+', '_', name)
    name = name.strip('_')

    # Specific fix for pollination typo in original filenames if present
    if "polllination" in name:
        name = name.replace("polllination", "pollination")

    return name

def process_tile(args):
    """Helper to process a single window."""
    window, path_early, path_late, dst, write_lock = args

    # Open datasets in thread-safe manner (or rely on rasterio's internal handling with separate reads)
    # Ideally we pass open dataset handles if using threads, but for safety we can read from the shared handle
    # provided we don't seek. Rasterio read with window is generally thread-safe if GDAL is built correctly.
    # However, to be absolutely robust against race conditions in GDAL's internal state,
    # we often use a lock for IO or open separate handles.
    # Given the context of DEM_Mask.py, we will try using the shared handles for reading.

    try:
        # We need access to the source datasets.
        # Since we can't easily pass open handles to a map function without global or closure,
        # we'll assume src_early and src_late are available or passed.
        # To make this clean with the executor, we'll define this inside calculate_difference or use a class.
        pass
    except Exception as e:
        return f"Error in window {window}: {e}"

def calculate_difference(path_early, path_late, output_path, num_threads=4):
    """Calculates late - early raster difference."""

    with rasterio.open(path_early) as src_early, rasterio.open(path_late) as src_late:
        # Basic checks
        if src_early.shape != src_late.shape:
            logging.warning(f"Shape mismatch for {path_early.name} vs {path_late.name}. Skipping.")
            return

        # Prepare profile
        profile = src_early.profile.copy()
        out_nodata = src_early.nodata if src_early.nodata is not None else -9999
        profile.update(dtype='float32', nodata=out_nodata, compress='lzw', tiled=True)

        logging.info(f"Writing difference to {output_path}")

        # Define windows (tiles)
        # We use a chunk size that fits comfortably in memory (e.g., 1024x1024 or 2048x2048)
        block_shape = (2048, 2048)
        windows = []
        for ji, window in src_early.block_windows(1):
            # Use internal blocks if tiled, otherwise arbitrary windows
            windows.append(window)

        # If not tiled, we might get one huge window, so let's force slicing if needed
        if len(windows) == 1:
            windows = [w for _, w in src_early.block_windows()] # Retry or manual slice
            # Fallback to manual slicing if block_windows returns full image
            if len(windows) <= 1:
                from itertools import product
                h, w = src_early.shape
                windows = []
                for row in range(0, h, block_shape[0]):
                    for col in range(0, w, block_shape[1]):
                        windows.append(Window(col, row, min(block_shape[1], w-col), min(block_shape[0], h-row)))

        write_lock = threading.Lock()

        with rasterio.open(output_path, 'w', **profile) as dst:

            def process_and_write(window):
                # Read
                d_early = src_early.read(1, window=window).astype('float32')
                d_late = src_late.read(1, window=window).astype('float32')

                # Mask
                mask = np.isnan(d_early) | np.isnan(d_late)
                if src_early.nodata is not None:
                    mask |= (d_early == src_early.nodata)
                if src_late.nodata is not None:
                    mask |= (d_late == src_late.nodata)

                # Calc
                diff = d_late - d_early
                diff[mask] = out_nodata

                # Write (locked)
                with write_lock:
                    dst.write(diff, 1, window=window)

            # Run in parallel threads
            with concurrent.futures.ThreadPoolExecutor(max_workers=num_threads) as executor:
                list(tqdm(executor.map(process_and_write, windows), total=len(windows), desc=f"Processing {output_path.name}", unit="tile"))

def main():
    if not INPUT_DIR.exists():
        logging.error(f"Input directory not found: {INPUT_DIR}")
        return

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # List all TIFs and find 1992/2020 pairs
    all_files = list(INPUT_DIR.glob("*.tif"))
    files_1992 = [f for f in all_files if "1992" in f.name]

    tasks = []
    for f_1992 in files_1992:
        parts = f_1992.name.split("1992")
        prefix = parts[0]
        candidates = [f for f in all_files if f.name.startswith(prefix) and "2020" in f.name]

        if len(candidates) == 1:
            f_2020 = candidates[0]
            var_name = get_clean_variable_name(f_1992.name)
            out_path = OUTPUT_DIR / f"{var_name}_diff_1992_2020.tif"

            # Process sequentially file-by-file, but parallel within file
            calculate_difference(f_1992, f_2020, out_path, num_threads=os.cpu_count() or 4)

        elif len(candidates) == 0:
            logging.warning(f"No matching 2020 file found for {f_1992.name}")
        else:
            logging.warning(f"Multiple 2020 matches found for {f_1992.name}")


if __name__ == "__main__":
    main()