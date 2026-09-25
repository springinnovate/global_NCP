"""Download SPAM2020 v2.2 physical-area rasters (all 46 crops) from FAO's public GCS mirror
(no guestbook/registration gate, unlike the Harvard Dataverse original -- confirmed live: the
Dataverse download API returned a guestbook-required error, this bucket doesn't), clip each to
the Philippines AOI, and determine the dominant crop per pixel by physical area.

This is the crop-TYPE half of the cropland Kc fix (docs/swy/cn_kc_biome_coverage_registry.md,
bucket 2) -- tells us WHAT is grown where. The calendar half (WHEN it's planted/harvested,
Sacks et al. 2010) is a separate, not-yet-done step. Neither is wired into the biophysical table
yet -- this script only produces the dominant-crop layer.

Source: FAO's AFIRM GCS bucket, gs://fao-gismgr-afirm-data/DATA/AFIRM/MAPSET/SPAM2020-PHYSICAL-AREA/
(mirrors Harvard Dataverse doi:10.7910/DVN/SWPENT, CC-BY-4.0, confirmed via
https://data.fao.org/catalog/iso/3757ea29-7c21-419e-a9d0-46e0b268049e). Each file is one crop's
global physical-area raster (hectares, 5 arcmin), "ALL" production system (irrigated+rainfed
combined) -- 46 files, ~1MB each, no auth needed.

Crop codes (SPAM standard, see Readme_SPAM2020V2r2.txt if fetched): RICE, MAIZ, CASS=cassava,
CNUT=coconut, SUGC=sugarcane, BANA=banana, PLNT=plantain, COCO=cocoa, COFF=coffee, RUBB=rubber,
TOBA=tobacco, VEGE=vegetables (aggregate), TROF=other tropical fruit, etc. -- full list of 46
populated at runtime from the bucket listing, not hardcoded, so this doesn't silently drift if
FAO adds/renames crops.

Output:
    data/swy/shared/mapspam/raw_downloads/{CROP}.tif        (global, cached, reused across regions)
    data/swy/philippines/inputs/mapspam_dominant_crop_2020/dominant_crop_ph.tif  (crop code as a
        categorical raster -- see dominant_crop_ph_codes.json for the code->index mapping)
    data/swy/philippines/inputs/mapspam_dominant_crop_2020/dominant_crop_area_ph.tif  (the winning
        crop's own physical-area value, hectares -- useful to gauge confidence/margin)
"""
import json
import os

import numpy as np
import rasterio
import requests
from rasterio.crs import CRS
from rasterio.mask import mask
import geopandas as gpd

BUCKET_LIST_URL = (
    "https://storage.googleapis.com/storage/v1/b/fao-gismgr-afirm-data/o"
    "?prefix=DATA/AFIRM/MAPSET/SPAM2020-PHYSICAL-AREA&maxResults=1000"
    "&fields=items(name,mediaLink)"
)
RAW_DIR = "data/swy/shared/mapspam/raw_downloads"
AOI_PATH = "data/swy/philippines/inputs/ph_aoi.gpkg"
OUT_DIR = "data/swy/philippines/inputs/mapspam_dominant_crop_2020"


def list_crop_files():
    r = requests.get(BUCKET_LIST_URL, timeout=60)
    r.raise_for_status()
    items = r.json().get("items", [])
    crops = {}
    for item in items:
        name = item["name"]
        if not name.endswith(".ALL.tif"):
            continue
        # DATA/AFIRM/MAPSET/SPAM2020-PHYSICAL-AREA/AFIRM.SPAM2020-PHYSICAL-AREA.<CROP>.ALL.tif
        code = name.split(".")[-3]
        crops[code] = item["mediaLink"]
    return crops


def fetch_crop(code, media_link):
    os.makedirs(RAW_DIR, exist_ok=True)
    out_path = os.path.join(RAW_DIR, f"{code}.tif")
    if os.path.exists(out_path):
        return out_path
    r = requests.get(media_link, timeout=120)
    r.raise_for_status()
    with open(out_path, "wb") as f:
        f.write(r.content)
    return out_path


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    crops = list_crop_files()
    print(f"found {len(crops)} crops in the bucket listing")

    aoi = gpd.read_file(AOI_PATH).to_crs(4326)
    geoms = [g.__geo_interface__ for g in aoi.geometry]

    codes = sorted(crops.keys())
    stack = None
    ref_transform = ref_crs = None

    for i, code in enumerate(codes):
        print(f"[{i+1}/{len(codes)}] {code}...")
        global_path = fetch_crop(code, crops[code])
        with rasterio.open(global_path) as src:
            out_img, out_transform = mask(src, geoms, crop=True, nodata=np.nan, filled=True)
            out_meta = src.meta.copy()
        arr = out_img[0].astype("float32")
        arr = np.where(arr < 0, np.nan, arr)  # SPAM uses negative sentinels for nodata in places

        if stack is None:
            stack = np.full((len(codes),) + arr.shape, np.nan, dtype="float32")
            ref_transform = out_transform
            ref_crs = src.crs
        stack[i] = arr

    # dominant crop = highest physical area per pixel, ignoring all-NaN pixels (no cropland there)
    all_nan = np.all(np.isnan(stack), axis=0)
    stack_filled = np.where(np.isnan(stack), -1, stack)
    dominant_idx = np.argmax(stack_filled, axis=0).astype("int16")
    dominant_area = np.max(stack_filled, axis=0).astype("float32")
    dominant_idx = np.where(all_nan, -1, dominant_idx)
    dominant_area = np.where(all_nan, np.nan, dominant_area)

    height, width = dominant_idx.shape
    profile = {
        "driver": "GTiff", "height": height, "width": width, "count": 1,
        "dtype": "int16", "crs": ref_crs or CRS.from_epsg(4326), "transform": ref_transform,
        "nodata": -1, "compress": "lzw",
    }
    with rasterio.open(os.path.join(OUT_DIR, "dominant_crop_ph.tif"), "w", **profile) as dst:
        dst.write(dominant_idx, 1)

    profile.update(dtype="float32", nodata=np.nan)
    with rasterio.open(os.path.join(OUT_DIR, "dominant_crop_area_ph.tif"), "w", **profile) as dst:
        dst.write(dominant_area, 1)

    with open(os.path.join(OUT_DIR, "dominant_crop_ph_codes.json"), "w") as f:
        json.dump({str(i): c for i, c in enumerate(codes)}, f, indent=2)

    valid = dominant_idx[dominant_idx >= 0]
    vals, counts = np.unique(valid, return_counts=True)
    order = np.argsort(-counts)
    print("\nDominant crop, share of classified pixels:")
    for idx in order[:15]:
        code = codes[vals[idx]]
        pct = 100 * counts[idx] / valid.size
        print(f"  {code:6s} {pct:5.1f}%  (n={counts[idx]})")

    print(f"\ndone -- see {OUT_DIR}")


if __name__ == "__main__":
    main()
