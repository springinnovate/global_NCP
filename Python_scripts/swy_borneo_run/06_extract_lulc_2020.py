"""Extract the Borneo AOI from the real 2020 Copernicus C3S land cover product.

The 300m global C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc file is not stored in this repo's
data/ (2.3GB) — it lives on the user's WWF OneDrive, at the path below. Update ONEDRIVE_NC_PATH
if that location changes. Real note (2026-09-10): a local data/raw/LandCovers/landcover_gl_1992.tif
that this project previously relied on turned out to be an empty/corrupted stub (zero valid
pixels found anywhere on Earth, not just Borneo) — the user had removed the real data for
storage reasons at some point. Kept as a cautionary example: verify raster content, not just
file existence, before trusting a "this input already exists" assumption.

Output: data/borneo_lulc/lulc_borneo_2020.tif, clipped to the exact AOI polygon (not just its
bounding box) with nodata=255 outside it, using the ESA CCI / C3S LCCS class codes (`lccs_class`
variable) that this project's CN table (gcn250_esa_lc_cn_table.csv) is already keyed to.
"""
import os

import geopandas as gpd
import numpy as np
import rasterio
from rasterio.crs import CRS
from rasterio.mask import mask
from rasterio.windows import from_bounds

ONEDRIVE_NC_PATH = (
    r"C:\Users\JerónimoRodríguezEsc\OneDrive - World Wildlife Fund, Inc\PROJECTS\Global_NCP"
    r"\data\Raw\6e774a4e4a552fd229bcb88b4d3c02e6\C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"
)
AOI_PATH = "data/swy_shared_package/borneo_aoi.gpkg"
OUT_DIR = "data/borneo_lulc"
OUT_PATH = os.path.join(OUT_DIR, "lulc_borneo_2020.tif")
NODATA = 255


def main():
    aoi = gpd.read_file(AOI_PATH).to_crs(4326)
    minx, miny, maxx, maxy = aoi.total_bounds
    geoms = [g.__geo_interface__ for g in aoi.geometry]

    subdataset = f'NETCDF:"{ONEDRIVE_NC_PATH}":lccs_class'
    with rasterio.open(subdataset) as src:
        # Windowed read first (a 0.1deg pad) — the global 300m raster is far too large to
        # load in full; only pull the Borneo-adjacent window before doing the exact-polygon mask.
        window = from_bounds(minx - 0.1, miny - 0.1, maxx + 0.1, maxy + 0.1, transform=src.transform)
        data = src.read(1, window=window)
        win_transform = src.window_transform(window)

    os.makedirs(OUT_DIR, exist_ok=True)
    tmp_path = os.path.join(OUT_DIR, "_tmp_windowed_2020.tif")
    profile = {
        "driver": "GTiff", "height": data.shape[0], "width": data.shape[1], "count": 1,
        "dtype": data.dtype, "crs": CRS.from_epsg(4326), "transform": win_transform,
        "compress": "lzw",
    }
    with rasterio.open(tmp_path, "w", **profile) as dst:
        dst.write(data, 1)

    with rasterio.open(tmp_path) as src:
        out_img, out_transform = mask(src, geoms, crop=True, nodata=NODATA)
        out_meta = src.meta.copy()
        out_meta.update({
            "height": out_img.shape[1], "width": out_img.shape[2],
            "transform": out_transform, "nodata": NODATA, "compress": "lzw",
        })
    with rasterio.open(OUT_PATH, "w", **out_meta) as dst:
        dst.write(out_img)
    os.remove(tmp_path)

    valid = out_img[out_img != NODATA]
    print(f"shape: {out_img.shape}, valid fraction: {100 * valid.size / out_img.size:.1f}%")
    vals, counts = np.unique(valid, return_counts=True)
    for v, c in sorted(zip(vals, counts), key=lambda x: -x[1])[:10]:
        print(f"  class {v}: {100 * c / valid.size:.1f}%")
    print(f"\nWritten to {OUT_PATH}")


if __name__ == "__main__":
    main()
