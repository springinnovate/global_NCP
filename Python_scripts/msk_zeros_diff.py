import os
import glob
import numpy as np
import rasterio
from rasterio.windows import Window
from tqdm import tqdm
import concurrent.futures

def process_tile(args):
    """
    Worker function to process a single tile.
    Reads data, creates mask, applies mask, and calculates difference.
    """
    window, path_1992, path_2020 = args
    
    # Open inputs in read mode within the process to ensure thread/process safety
    with rasterio.open(path_1992) as src92, rasterio.open(path_2020) as src20:
        d92 = src92.read(1, window=window).astype('float32')
        d20 = src20.read(1, window=window).astype('float32')
    
    # Create Mask (0 in both)
    mask_zeros = (d92 == 0) & (d20 == 0)
    
    # Apply Mask (Set to NaN)
    d92[mask_zeros] = np.nan
    d20[mask_zeros] = np.nan
    
    # Calculate Difference
    diff_raster = d20 - d92
    
    return window, mask_zeros, d92, d20, diff_raster

def mask_zeros_and_diff_py():
    # --- 1. Setup Paths ---
    # Using raw string (r"...") to handle backslashes in Windows paths
    data_dir = r"C:\Users\JerónimoRodríguezEsc\OneDrive - World Wildlife Fund, Inc\PROJECTS\Global_NCP\data\Raw"
    
    # Find files (equivalent to list.files with pattern)
    f_1992 = glob.glob(os.path.join(data_dir, "*1992*.tif"))
    f_2020 = glob.glob(os.path.join(data_dir, "*2020*.tif"))
    
    if not f_1992:
        raise FileNotFoundError("1992 file not found in the specified directory.")
    if not f_2020:
        raise FileNotFoundError("2020 file not found in the specified directory.")
        
    # Take the first match
    path_1992 = f_1992[0]
    path_2020 = f_2020[0]
    
    print(f"Processing:\n {path_1992}\n {path_2020}")

    # --- 2. Processing with Chunking ---
    # Chunk size (adjust if needed, 2048 is usually good)
    chunk_size = 2048

    with rasterio.open(path_1992) as src92, rasterio.open(path_2020) as src20:
        # Prepare profiles
        profile = src92.profile.copy()
        profile.update(dtype='float32', nodata=np.nan, compress='lzw')

        mask_profile = src92.profile.copy()
        mask_profile.update(dtype='uint8', count=1, nodata=None, compress='lzw')

        # Output paths
        mask_out_path = os.path.join(data_dir, "mask_zeros_both_py.tif")
        out_1992 = path_1992.replace(".tif", "_masked_py.tif")
        out_2020 = path_2020.replace(".tif", "_masked_py.tif")
        diff_out_path = os.path.join(data_dir, "difference_2020_minus_1992_py.tif")

        print("Starting chunked processing...")
        
        with rasterio.open(mask_out_path, 'w', **mask_profile) as dst_mask, \
             rasterio.open(out_1992, 'w', **profile) as dst_92, \
             rasterio.open(out_2020, 'w', **profile) as dst_20, \
             rasterio.open(diff_out_path, 'w', **profile) as dst_diff:
            
            height, width = src92.height, src92.width
            
            # Generate windows
            windows = []
            for row in range(0, height, chunk_size):
                for col in range(0, width, chunk_size):
                    windows.append(Window(col, row, 
                                          min(chunk_size, width - col), 
                                          min(chunk_size, height - row)))
            
            # Prepare arguments for parallel processing
            tasks = [(win, path_1992, path_2020) for win in windows]
            
            # Use ProcessPoolExecutor for parallel execution
            # Adjust max_workers based on your CPU cores (default is usually fine)
            with concurrent.futures.ProcessPoolExecutor() as executor:
                # Submit tasks
                futures = {executor.submit(process_tile, task): task for task in tasks}
                
                # Process results as they complete
                for future in tqdm(concurrent.futures.as_completed(futures), total=len(futures), desc="Processing Tiles"):
                    window, mask, d92, d20, diff = future.result()
                    
                    # Write outputs sequentially (safe)
                    dst_mask.write(mask.astype('uint8'), 1, window=window)
                    dst_92.write(d92, 1, window=window)
                    dst_20.write(d20, 1, window=window)
                    dst_diff.write(diff, 1, window=window)

    print("Python processing complete. Files exported.")

if __name__ == "__main__":
    mask_zeros_and_diff_py()
