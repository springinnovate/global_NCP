import os
import glob
import numpy as np
import rasterio
from rasterio.windows import Window
from multiprocessing import Pool, cpu_count
import re # Import re for regex operations


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
        "global_", "marine_mod_esa", "tnc_esa", "esamar", "lspop2019_esa",
        "realized_", "_on_ag_esa", "_fertilizer", "compressed", "md5"
    ]

    for pattern in remove_patterns:
        name = name.replace(pattern, "")

    name = re.sub(r'_+', '_', name) # Clean up underscores
    name = name.strip('_')

    return name

def process_window(args):
    """
    Worker function to process a single window.
    Reads data from 1992 and 2020 rasters for the given window,
    calculates the difference, and masks pixels where both are 0.
    """
    window, path_1992, path_2020, out_path = args

    with rasterio.open(path_1992) as src92, rasterio.open(path_2020) as src20:
        d92 = src92.read(1, window=window).astype('float32')
        d20 = src20.read(1, window=window).astype('float32')
        # Create Mask: True where both are 0
        mask_zeros = (d92 == 0) & (d20 == 0)
        
        # Apply mask to d92 and d20 if intermediate rasters are to be saved
        # This ensures the saved d92/d20 are the masked versions
        masked_d92 = d92.copy()
        masked_d20 = d20.copy()
        masked_d92[mask_zeros] = np.nan
        masked_d20[mask_zeros] = np.nan
        
        # Calculate Difference (2020 - 1992)
        diff_raster = d20 - d92
        
        # Apply mask: Set difference to NaN where both are 0
        diff_raster[mask_zeros] = np.nan
        
    if save_intermediate:
        return window, diff_raster, masked_d92, masked_d20, mask_zeros.astype('uint8')
    else:
        return window, diff_raster


def mask_zeros_and_diff_py(save_intermediate_rasters=False):
    # --- 1. Setup Paths ---
    # Use a relative path from the script's location
    script_dir = os.path.dirname(os.path.abspath(__file__))
    data_dir = os.path.join(script_dir, "..", "data", "raw")
    out_diff_dir = os.path.join(script_dir, "..", "data", "2020_1992_chg")
    os.makedirs(out_diff_dir, exist_ok=True)
    
    # Assuming you are looking for specific files, you might need to adjust this pattern
    # or pass specific filenames if there are multiple.
    f_1992 = glob.glob(os.path.join(data_dir, "Spring", "Inspring", "*1992*.tif"))
    f_2020 = glob.glob(os.path.join(data_dir, "Spring", "Inspring", "*2020*.tif"))
    
    if not f_1992 or not f_2020:
        raise FileNotFoundError(f"Could not find 1992 or 2020 TIF files in {os.path.join(data_dir, 'Spring', 'Inspring')}.")
        
    path_1992 = f_1992[0] # Take the first match
    path_2020 = f_2020[0] # Take the first match
    
    print(f"Processing:\n {path_1992}\n {path_2020}")

    with rasterio.open(path_1992) as src92, rasterio.open(path_2020) as src20:
        # Validate metadata match
        if src92.meta['width'] != src20.meta['width'] or src92.meta['height'] != src20.meta['height']:
             raise ValueError("Input rasters do not have the same dimensions.")

        profile = src92.profile.copy()
        profile.update(dtype='float32', nodata=np.nan, compress='lzw', BIGTIFF='YES')

        var_name = get_clean_variable_name(os.path.basename(path_1992))
        diff_out_path = os.path.join(out_diff_dir, f"{var_name}_diff_1992_2020.tif")

        # Conditional intermediate raster paths and profiles
        if save_intermediate_rasters:
            mask_out_path = os.path.join(out_diff_dir, f"{var_name}_mask_zeros_both.tif")
            out_1992_masked = os.path.join(out_diff_dir, f"{var_name}_1992_masked.tif")
            out_2020_masked = os.path.join(out_diff_dir, f"{var_name}_2020_masked.tif")

            mask_profile = src92.profile.copy()
            mask_profile.update(dtype='uint8', count=1, nodata=0, compress='lzw', BIGTIFF='YES') # nodata for mask is 0

        # Chunk size (larger is better for I/O, but requires more RAM per core)
        chunk_size = 4096 
        height, width = src92.height, src92.width        
        windows = [Window(col, row, min(chunk_size, width - col), min(chunk_size, height - row))
                   for row in range(0, height, chunk_size) for col in range(0, width, chunk_size)]

        print(f"Divided image into {len(windows)} chunks. Processing...")

        # Using fewer workers limits memory consumption
        num_workers = max(1, cpu_count() - 2)
        tasks = [(win, path_1992, path_2020, save_intermediate_rasters) for win in windows]

        if save_intermediate_rasters:
            with rasterio.open(diff_out_path, 'w', **profile) as dst_diff, \
                 rasterio.open(mask_out_path, 'w', **mask_profile) as dst_mask, \
                 rasterio.open(out_1992_masked, 'w', **profile) as dst_92_masked, \
                 rasterio.open(out_2020_masked, 'w', **profile) as dst_20_masked:
                with Pool(processes=num_workers) as pool:
                    for i, result_tuple in enumerate(pool.imap_unordered(process_window, tasks)):
                        window, diff_raster, masked_d92, masked_d20, mask_zeros = result_tuple
                        dst_diff.write(diff_raster, 1, window=window)
                        dst_mask.write(mask_zeros, 1, window=window)
                        dst_92_masked.write(masked_d92, 1, window=window)
                        dst_20_masked.write(masked_d20, 1, window=window)
                        if (i + 1) % 10 == 0:
                             print(f"Processed {i + 1}/{len(windows)} chunks...")
        else:
            with Pool(processes=num_workers) as pool:
                for i, (window, diff_raster) in enumerate(pool.imap_unordered(process_window, tasks)):
                    dst_diff.write(diff_raster, 1, window=window)
                    if (i + 1) % 10 == 0:
                         print(f"Processed {i + 1}/{len(windows)} chunks...")

    print("Python processing complete. Files exported.")

if __name__ == "__main__":
    # Example usage: run with save_intermediate_rasters=True to get all outputs
    # or leave as default False for just the difference raster
    mask_zeros_and_diff_py(save_intermediate_rasters=True)
