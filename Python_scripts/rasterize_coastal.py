import geopandas as gpd
import rasterio
from rasterio.features import rasterize
from rasterio.windows import Window
from pathlib import Path
import numpy as np
from tqdm import tqdm

# Paths
points_fp = "/home/jeronimo/OneDrive/global_NCP/data/Spring/Inspring/coastal_protection_Wchange.shp"
template_fp = "/home/jeronimo/OneDrive/global_NCP/data/input_rasters/LandCovers/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_md5_2ed6285e6f8ec1e7e0b75309cc6d6f9f.tif"
output_dir = Path("/home/jeronimo/OneDrive/global_NCP/data/input_rasters/coastal_protection_rast_tiles")
output_dir.mkdir(parents=True, exist_ok=True)

# Parameters
columns = ["Rt_1992", "Rt_2020"]  # Removed Rt_serv_ch

def tile_bounds(width, height, tile_size):
    for y in range(0, height, tile_size):
        for x in range(0, width, tile_size):
            w = min(tile_size, width - x)
            h = min(tile_size, height - y)
            yield x, y, w, h

# Load data
print("Loading points...")
gdf = gpd.read_file(points_fp)
print(f"✓ Loaded {len(gdf)} points")

# Open template
with rasterio.open(template_fp) as src:
    meta = src.meta.copy()
    transform = src.transform
    width = src.width
    height = src.height
    crs = src.crs
    res = src.res

meta.update(dtype="float32", count=1, nodata=np.nan, compress="deflate")
tile_size = 2000  # Adjust based on available memory

# Rasterize each variable by tile
for column in columns:
    print(f"\nRasterizing: {column}")
    tile_files = []

    for x, y, w, h in tqdm(tile_bounds(width, height, tile_size)):
        window = Window(x, y, w, h)
        bbox = rasterio.windows.bounds(window, transform)
        tile_gdf = gdf.cx[bbox[0]:bbox[2], bbox[1]:bbox[3]]
        if tile_gdf.empty:
            continue

        shapes = ((geom, val) for geom, val in zip(tile_gdf.geometry, tile_gdf[column]))
        counts = ((geom, 1) for geom in tile_gdf.geometry)

        sum_r = rasterize(
            shapes,
            out_shape=(h, w),
            transform=rasterio.windows.transform(window, transform),
            fill=0,
            all_touched=False,
            dtype="float32"
        )

        count_r = rasterize(
            counts,
            out_shape=(h, w),
            transform=rasterio.windows.transform(window, transform),
            fill=0,
            all_touched=False,
            dtype="uint16"
        )

        with np.errstate(divide='ignore', invalid='ignore'):
            mean_r = np.where(count_r > 0, sum_r / count_r, np.nan)

        tile_fp = output_dir / f"{column}_x{x}_y{y}.tif"
        tile_meta = meta.copy()
        tile_meta.update({"height": h, "width": w, "transform": rasterio.windows.transform(window, transform)})

        with rasterio.open(tile_fp, "w", **tile_meta) as dst:
            dst.write(mean_r, 1)

        tile_files.append(tile_fp)

    print(f"✓ Finished rasterizing {column}, {len(tile_files)} tiles saved")

# Note: Final mosaic step not included yet

