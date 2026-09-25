"""Download Sacks et al. (2010) global crop planting/harvest calendar (SAGE, UW-Madison,
5-arcmin, "filled"/spatially-extrapolated netCDF), clip to the Philippines AOI, for the
two dominant Annual Crop types by area found in 06b_fetch_mapspam_dominant_crop.py: rice
(50.6% of classified cropland) and maize (15.5%). This is the calendar half (WHEN a crop
is planted/harvested) of the cropland Kc fix -- 06b already gave the crop-TYPE half
(WHAT is grown where). Coconut (16.9%, the #2 crop by area) is NOT in this dataset --
it's a perennial tree crop with no annual planting/harvest cycle, and falls under the
WWF-SIPA "Perennial Crop" LULC class, not "Annual Crop" (the class this whole fix
targets, 5.82x B distortion, docs/swy/cn_kc_biome_coverage_registry.md bucket 2), so
its absence here doesn't block the Annual Crop fix. Neither this raw data nor the
eventual per-pixel Kc build is wired into the biophysical table yet -- this script only
fetches and clips the calendar itself.

Source: Sacks, W.J., D. Deryng, J.A. Foley, and N. Ramankutty (2010). Crop planting
dates: An analysis of global patterns. Global Ecology and Biogeography, 19, 607-620.
Data: SAGE (Center for Sustainability and the Global Environment, UW-Madison),
https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset/ ,
files served from https://sage-public-files.s3.amazonaws.com/crop-calendar-dataset/
netcdf5min/ (confirmed live 2026-09-23; the dataset's own front-end pages have moved
domains at least once -- sage.wisc.edu -> nelson.wisc.edu -- so prefer this direct S3
path over re-deriving it from whatever page is current). "Filled" variant used (spatial
extrapolation beyond the original station observations) since a regional raster fix
needs full AOI coverage, not just the original sparse survey points; each crop has a
main-season file and a "2" (second season) file for double-cropping regions -- both
fetched since Philippine rice is commonly double- or triple-cropped.

Per-file netCDF subdatasets used here: `plant`, `harvest`, `tot.days` (day-of-year
planting/harvest dates and growing-season length; the dataset's own `.start`/`.end`/
`.range` variants describe observed variability, not needed for a single Kc-calendar
value per pixel).

Output: data/swy/philippines/inputs/sacks_crop_calendar_ph/{crop}_{season}_{var}_ph.tif
    crop in {rice, maize}, season in {main, second}, var in {plant, harvest, tot_days}
"""
import gzip
import os
import shutil

import numpy as np
import rasterio
import requests
from rasterio.mask import mask
import geopandas as gpd

RAW_DIR = "data/swy/shared/sacks_crop_calendar/raw_downloads"
AOI_PATH = "data/swy/philippines/inputs/ph_aoi.gpkg"
OUT_DIR = "data/swy/philippines/inputs/sacks_crop_calendar_ph"

BASE_URL = "https://sage-public-files.s3.amazonaws.com/crop-calendar-dataset/netcdf5min"

# (output crop name, SAGE file prefix, output season name, SAGE season suffix)
CROP_FILES = [
    ("rice", "Rice", "main", ""),
    ("rice", "Rice", "second", ".2"),
    ("maize", "Maize", "main", ""),
    ("maize", "Maize", "second", ".2"),
]
VARS = ["plant", "harvest", "tot.days"]


def fetch_and_decompress(sage_prefix, season_suffix):
    fname_base = f"{sage_prefix}{season_suffix}.crop.calendar.fill.nc"
    gz_path = os.path.join(RAW_DIR, f"{fname_base}.gz")
    nc_path = os.path.join(RAW_DIR, fname_base)
    if os.path.exists(nc_path):
        return nc_path

    os.makedirs(RAW_DIR, exist_ok=True)
    if not os.path.exists(gz_path):
        url = f"{BASE_URL}/{fname_base}.gz"
        print(f"downloading {url} ...")
        r = requests.get(url, timeout=300, stream=True)
        r.raise_for_status()
        with open(gz_path, "wb") as f:
            shutil.copyfileobj(r.raw, f)

    print(f"decompressing {gz_path} ...")
    with gzip.open(gz_path, "rb") as f_in, open(nc_path, "wb") as f_out:
        shutil.copyfileobj(f_in, f_out)
    return nc_path


def clip_subdataset(nc_path, var, aoi_geoms, out_path):
    src_path = f'NETCDF:"{nc_path}":{var}'
    with rasterio.open(src_path) as src:
        out_img, out_transform = mask(src, aoi_geoms, crop=True, filled=True, nodata=np.nan)
        out_meta = src.meta.copy()
        nodata_val = src.nodata

    arr = out_img[0].astype("float32")
    if nodata_val is not None:
        arr = np.where(np.isclose(arr, nodata_val), np.nan, arr)

    out_meta.update(
        driver="GTiff", dtype="float32", nodata=np.nan,
        height=arr.shape[0], width=arr.shape[1], transform=out_transform,
        compress="lzw", count=1,
    )
    with rasterio.open(out_path, "w", **out_meta) as dst:
        dst.write(arr, 1)

    valid = np.isfinite(arr)
    if valid.sum() > 0:
        print(f"  {os.path.basename(out_path)}: n_valid={valid.sum()}, mean={arr[valid].mean():.1f}")
    else:
        print(f"  {os.path.basename(out_path)}: no valid pixels in AOI (crop/season not grown here)")


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    aoi = gpd.read_file(AOI_PATH).to_crs(4326)
    geoms = [g.__geo_interface__ for g in aoi.geometry]

    for crop_name, sage_prefix, season_name, season_suffix in CROP_FILES:
        print(f"\n{crop_name} ({season_name} season) ...")
        nc_path = fetch_and_decompress(sage_prefix, season_suffix)
        for var in VARS:
            var_out = var.replace(".", "_")
            out_path = os.path.join(OUT_DIR, f"{crop_name}_{season_name}_{var_out}_ph.tif")
            clip_subdataset(nc_path, var, geoms, out_path)

    print(f"\ndone -- see {OUT_DIR}")


if __name__ == "__main__":
    main()
