"""
Temporary script to calculate raster difference for Nature Access (2020 - 1992).
Explicitly targets specific files to bypass batch matching logic.
"""

import os
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
        block_shape = (2048, 2048)
        windows = []
        for ji, window in src_early.block_windows(1):
            windows.append(window)

        # Fallback if not tiled or single block
        if len(windows) <= 1:
            h, w = src_early.shape
            windows = []
            for row in range(0, h, block_shape[0]):
                for col in range(0, w, block_shape[1]):
                    windows.append(Window(col, row, min(block_shape[1], w-col), min(block_shape[0], h-row)))

        write_lock = threading.Lock()

        with rasterio.open(output_path, 'w', **profile) as dst:

            def process_and_write(window):
                try:
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
                except Exception as e:
                    logging.error(f"Error processing window {window}: {e}")

            # Run in parallel threads
            with concurrent.futures.ThreadPoolExecutor(max_workers=num_threads) as executor:
                list(tqdm(executor.map(process_and_write, windows), total=len(windows), desc=f"Processing {output_path.name}", unit="tile"))

def main():
    path_1992 = Path("/home/jeronimo/data/global_ncp/raw/Spring/Inspring/nature_access_lspop2019_ESA1992_md5_b32b8b.tif")
    path_2020 = Path("/home/jeronimo/data/global_ncp/raw/Spring/Inspring/nature_access_lspop2019_ESA2020_compressed_md5_6727ac.tif")

    # Output directory matches batch_raster_diff.py
    output_dir = Path("/home/jeronimo/data/global_ncp/2020_1992_chg")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Explicit output name based on variable cleaning logic
    output_path = output_dir / "nature_access_diff_1992_2020.tif"

    calculate_difference(path_1992, path_2020, output_path, num_threads=os.cpu_count() or 4)

if __name__ == "__main__":
    main()