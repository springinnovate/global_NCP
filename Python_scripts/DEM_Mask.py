```python
import rasterio
import numpy as np

print("Everything is working!")

```

    Everything is working!



```python
!pip install tqdm
```

    Collecting tqdm
      Downloading tqdm-4.67.1-py3-none-any.whl.metadata (57 kB)
    [2K     [38;2;114;156;31m━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━[0m [32m57.7/57.7 kB[0m [31m3.2 MB/s[0m eta [36m0:00:00[0m
    [?25hDownloading tqdm-4.67.1-py3-none-any.whl (78 kB)
    [2K   [38;2;114;156;31m━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━[0m [32m78.5/78.5 kB[0m [31m7.8 MB/s[0m eta [36m0:00:00[0m
    [?25hInstalling collected packages: tqdm
    Successfully installed tqdm-4.67.1



```python

```


```python
import rasterio
from rasterio.windows import Window
from concurrent.futures import ThreadPoolExecutor
import numpy as np
from tqdm import tqdm

pop_path = "/home/jeronimo/OneDrive/global_NCP/data/analysis/Landscan/landscan-global-2023-assets/landscan-global-2023.tif"
mask_path = "/home/jeronimo/OneDrive/global_NCP/data/input_rasters/aux_data/dem_10m.tif"
output_path = "/home/jeronimo/OneDrive/global_NCP/data/Spring/Change/landscan-global-2023_10m.tif"
print("Opening rasters...")
# Set chunk size (adjust based on your machine — 512x512 is a safe start)

chunk_size = 1024
num_threads = 4  # 

# Open input rasters
with rasterio.open(pop_path) as pop_src, rasterio.open(mask_path) as mask_src:
    profile = pop_src.profile
    profile.update(dtype=rasterio.float32)

    width = pop_src.width
    height = pop_src.height

    # Create output raster file
    with rasterio.open(output_path, "w", **profile) as dst:

        # Create list of windows
        windows = []
        for row in range(0, height, chunk_size):
            for col in range(0, width, chunk_size):
                win = Window(col, row,
                             min(chunk_size, width - col),
                             min(chunk_size, height - row))
                windows.append((row, col, win))

        # Function to process and write one window
        def process_window(args):
            row, col, window = args
            pop_chunk = pop_src.read(1, window=window)
            mask_chunk = mask_src.read(1, window=window)
            masked_chunk = np.where(mask_chunk == 1, pop_chunk, 0)
            dst.write(masked_chunk.astype(np.float32), 1, window=window)
            return f"✅ Wrote row {row}, col {col}"

        # Run in parallel with progress bar
        with ThreadPoolExecutor(max_workers=num_threads) as executor:
            for result in tqdm(executor.map(process_window, windows), total=len(windows)):
                print(result)

print("🎉 All done. Output saved to", output_path)

```
