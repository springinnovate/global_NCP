"""Fetch and clip 2020 monthly precipitation (CHIRPS) for Borneo.

No authentication needed — CHIRPS is served publicly, unauthenticated, from UCSB's Climate
Hazards Center (not NASA-distributed, so not reachable via AppEEARS). Downloads global monthly
GeoTIFFs (~14.5MB/month compressed), clips each to the Borneo AOI locally, and cleans up the
large intermediate global files afterward (keeps only the small .tif.gz originals for
provenance).
"""
import gzip
import os
import shutil

import geopandas as gpd
import rasterio
import requests
from rasterio.mask import mask

AOI_PATH = "data/swy_shared_package/borneo_aoi.gpkg"
RAW_DIR = "data/chirps_borneo_2020/global_raw"
OUT_DIR = "data/swy_shared_package/precip_chirps_2020"
YEAR = 2020
URL_TEMPLATE = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/tifs/chirps-v2.0.{year}.{month:02d}.tif.gz"


def download_and_decompress(month):
    gz_path = os.path.join(RAW_DIR, f"chirps-v2.0.{YEAR}.{month:02d}.tif.gz")
    tif_path = gz_path[:-3]
    if not os.path.exists(gz_path):
        r = requests.get(URL_TEMPLATE.format(year=YEAR, month=month))
        r.raise_for_status()
        with open(gz_path, "wb") as f:
            f.write(r.content)
    with gzip.open(gz_path, "rb") as f_in, open(tif_path, "wb") as f_out:
        shutil.copyfileobj(f_in, f_out)
    return tif_path


def clip_to_aoi(global_tif_path, geoms, out_path):
    with rasterio.open(global_tif_path) as src:
        out_img, out_transform = mask(src, geoms, crop=True)
        out_meta = src.meta.copy()
        out_meta.update({
            "height": out_img.shape[1], "width": out_img.shape[2],
            "transform": out_transform, "compress": "lzw",
        })
    with rasterio.open(out_path, "w", **out_meta) as dst:
        dst.write(out_img)


def main():
    os.makedirs(RAW_DIR, exist_ok=True)
    os.makedirs(OUT_DIR, exist_ok=True)
    aoi = gpd.read_file(AOI_PATH).to_crs(4326)
    geoms = [g.__geo_interface__ for g in aoi.geometry]

    for month in range(1, 13):
        print(f"month {month:02d}...")
        global_tif = download_and_decompress(month)
        out_path = os.path.join(OUT_DIR, f"chirps_precip_borneo_{YEAR}_{month:02d}.tif")
        clip_to_aoi(global_tif, geoms, out_path)
        os.remove(global_tif)  # keep the .gz for provenance, drop the large uncompressed copy

    print(f"done — see {OUT_DIR}")


if __name__ == "__main__":
    main()
