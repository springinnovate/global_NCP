"""Fetch and clip 2020 monthly reference ET0 (TerraClimate) for Borneo.

No authentication needed — TerraClimate is served publicly, unauthenticated, via UI Boise's
THREDDS server. This is a source found and vetted for the first time in this project on
2026-09-10 (CHIRPS is precipitation-only; ET0 needs its own source) — see docs/swy/swy_methods.qmd
for why this specific product (the `pet` variable = "Reference Evapotranspiration," ASCE
Penman-Monteith corrected for CO2) is the right fit for InVEST's `et0_dir` input.

Downloads one global netCDF per year (~110MB, all 12 months bundled), extracts and clips each
month to the Borneo AOI, applying the file's own scale_factor (0.1) to get real mm.
"""
import os

import geopandas as gpd
import numpy as np
import rasterio
import requests
from rasterio.crs import CRS
from rasterio.mask import mask

AOI_PATH = "data/swy_shared_package/borneo_aoi.gpkg"
RAW_DIR = "data/terraclimate_borneo_2020/global_raw"
OUT_DIR = "data/swy_shared_package/et0_terraclimate_2020"
YEAR = 2020
URL = f"http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_pet_{YEAR}.nc"
SCALE_FACTOR = 0.1
NODATA = -9999.0


def main():
    os.makedirs(RAW_DIR, exist_ok=True)
    os.makedirs(OUT_DIR, exist_ok=True)
    nc_path = os.path.join(RAW_DIR, f"TerraClimate_pet_{YEAR}.nc")
    if not os.path.exists(nc_path):
        r = requests.get(URL)
        r.raise_for_status()
        with open(nc_path, "wb") as f:
            f.write(r.content)

    aoi = gpd.read_file(AOI_PATH).to_crs(4326)
    geoms = [g.__geo_interface__ for g in aoi.geometry]

    subdataset = f'NETCDF:"{nc_path}":pet'
    with rasterio.open(subdataset) as src:
        base_transform, height, width = src.transform, src.height, src.width
        for month in range(1, 13):
            print(f"month {month:02d}...")
            band = src.read(month, masked=True)
            arr = band.filled(NODATA).astype("float32")
            arr = np.where(arr == NODATA, NODATA, arr * SCALE_FACTOR)

            tmp_path = os.path.join(OUT_DIR, f"_tmp_band_{month:02d}.tif")
            profile = {
                "driver": "GTiff", "height": height, "width": width, "count": 1,
                "dtype": "float32", "crs": CRS.from_epsg(4326), "transform": base_transform,
                "nodata": NODATA,
            }
            with rasterio.open(tmp_path, "w", **profile) as tmp:
                tmp.write(arr, 1)

            with rasterio.open(tmp_path) as tmpsrc:
                out_img, out_transform = mask(tmpsrc, geoms, crop=True, nodata=NODATA)
                out_meta = tmpsrc.meta.copy()
                out_meta.update({
                    "height": out_img.shape[1], "width": out_img.shape[2],
                    "transform": out_transform, "compress": "lzw",
                })
            out_path = os.path.join(OUT_DIR, f"terraclimate_et0_borneo_{YEAR}_{month:02d}.tif")
            with rasterio.open(out_path, "w", **out_meta) as dst:
                dst.write(out_img)
            os.remove(tmp_path)

    print(f"done — see {OUT_DIR}")


if __name__ == "__main__":
    main()
