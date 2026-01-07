import geopandas as gpd
import rasterio
from rasterio.features import rasterize
from rasterio.windows import Window
from rasterio.merge import merge
import os
from pathlib import Path
import numpy as np
from tqdm import tqdm

# Paths (set GLOBAL_NCP_DATA in your environment)
data_root = os.environ.get("GLOBAL_NCP_DATA", "")
if not data_root:
    raise RuntimeError("GLOBAL_NCP_DATA is not set; expected /home/jeronimo/data/global_ncp")
data_root = Path(data_root)

points_fp = Path(
    os.environ.get(
        "COASTAL_POINTS_GPKG",
        data_root / "interim" / "c_protection_1992_2020_joined.gpkg",
    )
)
template_fp = (
    data_root
    / "raw"
    / "LandCovers"
    / "landcover_gl_1992.tif"
)
output_dir = data_root / "interim" / "coastal_protection_rast_tiles"
final_output_dir = data_root / "interim" / "coastal_protection_rasters"
output_dir.mkdir(parents=True, exist_ok=True)
final_output_dir.mkdir(parents=True, exist_ok=True)

# Parameters
# Optional: include Rt_serv_ch raster for comparison runs
include_change = os.environ.get("COASTAL_INCLUDE_CH", "0") == "1"
clean_outputs = os.environ.get("COASTAL_CLEAN", "0") == "1"
columns_env = os.environ.get("COASTAL_COLUMNS")
tile_size = 2000  # Adjust based on available memory

# Function to generate tile boundaries
def tile_bounds(width, height, tile_size):
    for y in range(0, height, tile_size):
        for x in range(0, width, tile_size):
            w = min(tile_size, width - x)
            h = min(tile_size, height - y)
            yield x, y, w, h

# Optional cleanup for re-runs
if clean_outputs:
    for fp in output_dir.glob("*.tif"):
        fp.unlink(missing_ok=True)
    for fp in final_output_dir.glob("*.tif"):
        fp.unlink(missing_ok=True)

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

if gdf.crs != crs:
    print(f"Reprojecting points to {crs} ...")
    gdf = gdf.to_crs(crs)

# NOTE: Rt_ratio_1992/2020 are expected to already exist in the input layer.
if columns_env:
    columns = [c.strip() for c in columns_env.split(",") if c.strip()]
else:
    numeric_cols = gdf.select_dtypes(include=["number"]).columns.tolist()
    exclude = {"fid", "id"}
    if not include_change:
        exclude.add("Rt_serv_ch")
    columns = [c for c in numeric_cols if c not in exclude]

if not columns:
    raise RuntimeError("No numeric columns found to rasterize.")

# Rasterize each variable by tile
for column in columns:
    print(f"\nRasterizing: {column}")
    tile_files = []

    for x, y, w, h in tqdm(tile_bounds(width, height, tile_size)):
        tile_fp = output_dir / f"{column}_x{x}_y{y}.tif"
        if tile_fp.exists():
            continue  # Skip already processed tile

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

        tile_meta = meta.copy()
        tile_meta.update({"height": h, "width": w, "transform": rasterio.windows.transform(window, transform)})

        with rasterio.open(tile_fp, "w", **tile_meta) as dst:
            dst.write(mean_r, 1)

        tile_files.append(tile_fp)

    print(f"✓ Finished rasterizing {column}, {len(tile_files)} tiles saved")

    # Mosaic the tiles into a single raster
    print(f"\nMosaicking tiles for {column}...")
    tile_rasters = [rasterio.open(fp) for fp in output_dir.glob(f"{column}_x*_y*.tif")]
    mosaic, out_trans = merge(tile_rasters)

    out_meta = meta.copy()
    out_meta.update({
        "height": mosaic.shape[1],
        "width": mosaic.shape[2],
        "transform": out_trans
    })

    final_fp = final_output_dir / f"{column}.tif"
    with rasterio.open(final_fp, "w", **out_meta) as dest:
        dest.write(mosaic)

    for r in tile_rasters:
        r.close()

    print(f"✓ Final raster saved to: {final_fp}")

# Optional cleanup: remove intermediate tiles to save space
cleanup_tiles = os.environ.get("COASTAL_CLEAN_TILES", "1") == "1"
if cleanup_tiles:
    for fp in output_dir.glob("*.tif"):
        fp.unlink()
    print(f"✓ Removed intermediate tiles in {output_dir}")
