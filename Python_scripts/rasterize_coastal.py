import geopandas as gpd
from pathlib import Path
import rasterio
from rasterio.features import rasterize
import numpy as np
# Load the point vector file (assuming it's a shapefile or GeoPackage)
gdf = gpd.read_file("/Users/rodriguez/OneDrive - World Wildlife Fund, Inc/global_NCP/data/Spring/Inspring/coastal_protection_Wchange.shp")

# Open the raster template
with rasterio.open("/Users/rodriguez/OneDrive - World Wildlife Fund, Inc/global_NCP/data/input_rasters/LandCovers/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_md5_2ed6285e6f8ec1e7e0b75309cc6d6f9f.tif") as src:
    meta = src.meta.copy()
    transform = src.transform
    out_shape = (src.height, src.width)
    crs = src.crs

def rasterize_column(gdf, value_col, transform, out_shape, agg_func=np.mean):
    # Create a generator of ((x, y), value)
    shapes = ((geom, value) for geom, value in zip(gdf.geometry, gdf[value_col]))
    
    # Create an array to count the number of overlapping points
    count = np.zeros(out_shape, dtype=np.uint16)
    values = np.zeros(out_shape, dtype=np.float32)

    # Rasterize by iterating and aggregating manually
    for geom, val in shapes:
        mask = rasterize(
            [(geom, 1)],
            out_shape=out_shape,
            transform=transform,
            fill=0,
            all_touched=False,
            dtype=np.uint8
        )
        count += mask
        values += mask * val

    # Avoid division by zero
    with np.errstate(divide='ignore', invalid='ignore'):
        out = np.where(count > 0, values / count, np.nan)

    return out

# Adjust these to match your column names
col1 = "Rt_1992"
col2 = "Rt_2020"
col_diff = "Rt_serv_ch"

r1 = rasterize_column(gdf, col1, transform, out_shape)
r2 = rasterize_column(gdf, col2, transform, out_shape)
rd = rasterize_column(gdf, col_diff, transform, out_shape)

meta.update(dtype=r1.dtype, count=1, nodata=np.nan)

output_dir = Path("/Users/rodriguez/Library/CloudStorage/OneDrive-WorldWildlifeFund,Inc/global_NCP/data/Spring/coastal_protection_rast")
output_dir.mkdir(exist_ok=True)  # make the folder if it doesn't exist


# Create output directory
output_dir = Path("/Users/rodriguez/Library/CloudStorage/OneDrive-WorldWildlifeFund,Inc/global_NCP/data/Spring/coastal_protection_rast")
output_dir.mkdir(parents=True, exist_ok=True)  # `parents=True` handles nested folders

# Export rasters using full path
with rasterio.open(output_dir / "Rt_1992_coastal_protection.tif", "w", **meta) as dst:
    dst.write(r1, 1)

with rasterio.open(output_dir / "Rt_2020_coastal_protection.tif", "w", **meta) as dst:
    dst.write(r2, 1)

with rasterio.open(output_dir / "Rt_ch_coastal_protection.tif", "w", **meta) as dst:
    dst.write(rd, 1)


