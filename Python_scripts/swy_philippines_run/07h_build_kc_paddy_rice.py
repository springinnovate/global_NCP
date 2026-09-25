"""Paddy-rice Kc for Annual Crop, on top of the current per-pixel Kc (07g_build_kc_ndvi_perpixel.py).

Annual Crop is the class with by far the largest remaining B gap against WWF-SIPA's parameters
(2.28x vs the 09h reference, 2026-09-25), and it is mostly paddy rice (MapSPAM SPAM2020: rice
50.6% of Philippine cropland by area). A Kamble NDVI fit from US dryland crops doesn't describe
a flooded paddy. This replaces Annual Crop's Kc with an FAO-56 rice curve, placed in time by the
Sacks et al. (2010) crop calendar and weighted by how much of each pixel's annual cropland is rice:

    Kc_annual_crop = f_rice * Kc_paddy + (1 - f_rice) * Kc_ndvi

- f_rice: MapSPAM RICE physical area / total annual-crop physical area in the 5-arcmin cell
  (tree and perennial crops excluded, they belong to Perennial Crop), bilinear onto the 1km grid.
- Kc_paddy: FAO-56 (Allen et al. 1998, Table 12) rice Kc_ini 1.05 (flooded), Kc_mid 1.20,
  Kc_end 0.75 (midpoint of FAO's 0.90-0.60), linear through development and late season. Stage
  lengths 20/20/40/20% of the season (FAO-56 Table 11 tropical rice, 30/30/60/30 of 150 days),
  scaled to each pixel's Sacks season length. Days outside the season keep that month's NDVI Kc
  (fallow or whatever is growing, as seen by MODIS).
- Season: Sacks rice_main plant DOY and length, per pixel (wraps across New Year). Sacks has no
  second rice season for this AOI, although irrigated Philippine paddies are often double-cropped;
  the NDVI Kc outside the main season partly covers a dry-season crop, but this is a real
  limitation.

Monthly Kc = mean over the month's days. Only Annual Crop (lulc_id 1) changes; every other pixel
is copied from 07g, so a run on this output differs from 09l only in Annual Crop Kc.

Inputs:
    data/swy/philippines/inputs/kc_ndvi_perpixel_2020/kc_MM.tif           (07g)
    data/swy/shared/mapspam/raw_downloads/{CROP}.tif                      (06b)
    data/swy/philippines/inputs/sacks_crop_calendar_ph/rice_main_{plant,tot_days}_ph.tif  (06c)
    data/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif

Outputs:
    data/swy/philippines/inputs/kc_paddy_2020/kc_MM.tif
    data/swy/philippines/inputs/kc_paddy_2020/annual_crop_kc_summary.csv
"""
import calendar
import glob
import os

import numpy as np
import pandas as pd
import rasterio
from rasterio.crs import CRS
from rasterio.enums import Resampling
from rasterio.warp import reproject

LULC_PATH = "data/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif"
BASE_KC_DIR = "data/swy/philippines/inputs/kc_ndvi_perpixel_2020"
MAPSPAM_DIR = "data/swy/shared/mapspam/raw_downloads"
SACKS_DIR = "data/swy/philippines/inputs/sacks_crop_calendar_ph"
OUT_DIR = "data/swy/philippines/inputs/kc_paddy_2020"

ANNUAL_CROP_ID = 1
YEAR = 2020
# tree / perennial crops, excluded from the annual-crop denominator
PERENNIAL_CROPS = {"BANA", "PLNT", "CITR", "CNUT", "COCO", "COFF", "RCOF", "OILP", "RUBB", "TEAS",
                   "TEMF", "TROF"}
KC_INI, KC_MID, KC_END = 1.05, 1.20, 0.75
STAGE_FRACTIONS = (0.2, 0.2, 0.4, 0.2)  # initial, development, mid, late
DEFAULT_PLANT_DOY, DEFAULT_SEASON_DAYS = 130, 171.5  # Sacks' AOI-wide median, fills gaps


def _to_ref(src_path, ref, resampling):
    out = np.full(ref["shape"], np.nan, dtype="float32")
    with rasterio.open(src_path) as src:
        reproject(
            source=rasterio.band(src, 1), destination=out,
            src_transform=src.transform, src_crs=src.crs or CRS.from_epsg(4326),
            dst_transform=ref["transform"], dst_crs=ref["crs"],
            resampling=resampling, src_nodata=src.nodata, dst_nodata=np.nan,
        )
    return out


def rice_fraction():
    """Rice share of annual-crop physical area, on MapSPAM's own grid (5 arcmin)."""
    rice, total, profile = None, None, None
    for path in sorted(glob.glob(f"{MAPSPAM_DIR}/*.tif")):
        crop = os.path.basename(path)[:-4]
        if crop in PERENNIAL_CROPS:
            continue
        with rasterio.open(path) as src:
            if profile is None:
                profile = src.profile
            arr = src.read(1).astype("float64")
            if src.nodata is not None:
                arr[arr == src.nodata] = 0
            arr[~np.isfinite(arr) | (arr < 0)] = 0
        total = arr if total is None else total + arr
        if crop == "RICE":
            rice = arr
    with np.errstate(invalid="ignore", divide="ignore"):
        frac = np.where(total > 0, rice / total, np.nan).astype("float32")
    return frac, profile


def paddy_day_kc(day_in_season, season_days):
    """FAO-56 rice Kc for a day index within the season (0-based), vectorised."""
    l_ini, l_dev, l_mid, _ = (f * season_days for f in STAGE_FRACTIONS)
    t1, t2, t3 = l_ini, l_ini + l_dev, l_ini + l_dev + l_mid
    kc = np.where(day_in_season < t1, KC_INI, 0.0)
    kc = np.where((day_in_season >= t1) & (day_in_season < t2),
                  KC_INI + (KC_MID - KC_INI) * (day_in_season - t1) / l_dev, kc)
    kc = np.where((day_in_season >= t2) & (day_in_season < t3), KC_MID, kc)
    kc = np.where(day_in_season >= t3,
                  KC_MID + (KC_END - KC_MID) * (day_in_season - t3) / (season_days - t3), kc)
    return kc


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    with rasterio.open(os.path.join(BASE_KC_DIR, "kc_01.tif")) as ref_src:
        ref = {"shape": ref_src.shape, "transform": ref_src.transform, "crs": ref_src.crs}
        profile = ref_src.profile

    lulc = _to_ref(LULC_PATH, ref, Resampling.mode)
    crop_mask = np.round(lulc) == ANNUAL_CROP_ID
    print(f"Annual Crop pixels: {crop_mask.sum()}")

    # rice fraction: computed on MapSPAM's grid, written to a temp file, then bilinear to 1km
    frac, mp_profile = rice_fraction()
    frac_path = os.path.join(OUT_DIR, "rice_fraction_mapspam.tif")
    mp_profile.update(dtype="float32", count=1, nodata=np.nan, compress="lzw")
    with rasterio.open(frac_path, "w", **mp_profile) as dst:
        dst.write(frac, 1)
    f_rice = _to_ref(frac_path, ref, Resampling.bilinear)
    f_rice = np.where(np.isfinite(f_rice), f_rice, 0.0)  # no MapSPAM cropland -> no rice weight

    plant = _to_ref(os.path.join(SACKS_DIR, "rice_main_plant_ph.tif"), ref, Resampling.nearest)
    season = _to_ref(os.path.join(SACKS_DIR, "rice_main_tot_days_ph.tif"), ref, Resampling.nearest)
    n_filled = int((crop_mask & ~np.isfinite(plant)).sum())
    plant = np.where(np.isfinite(plant), plant, DEFAULT_PLANT_DOY)
    season = np.where(np.isfinite(season), season, DEFAULT_SEASON_DAYS)
    print(f"Annual Crop pixels with no Sacks rice calendar (filled with AOI median): {n_filled}")

    # work only on Annual Crop pixels
    idx = np.where(crop_mask)
    f_c, plant_c, season_c = f_rice[idx], plant[idx], season[idx]
    print(f"rice fraction on Annual Crop: mean {f_c.mean():.2f}, "
          f"share of pixels > 0.5: {(f_c > 0.5).mean():.2f}")

    summary = []
    doy0 = 0
    for month in range(1, 13):
        days = calendar.monthrange(YEAR, month)[1]
        with rasterio.open(os.path.join(BASE_KC_DIR, f"kc_{month:02d}.tif")) as src:
            kc_out = src.read(1).astype("float32")
        kc_ndvi = kc_out[idx].astype("float64")

        paddy_sum = np.zeros_like(kc_ndvi)
        in_season_days = np.zeros_like(kc_ndvi)
        for d in range(days):
            doy = doy0 + d + 1
            day_in_season = (doy - plant_c) % 365
            in_season = day_in_season < season_c
            kc_day = np.where(in_season, paddy_day_kc(day_in_season, season_c), kc_ndvi)
            paddy_sum += kc_day
            in_season_days += in_season
        kc_paddy = paddy_sum / days
        kc_new = f_c * kc_paddy + (1 - f_c) * kc_ndvi
        kc_out[idx] = kc_new.astype("float32")
        doy0 += days

        summary.append({
            "month": month, "kc_ndvi_mean": kc_ndvi.mean(), "kc_paddy_mean": kc_paddy.mean(),
            "kc_new_mean": kc_new.mean(), "kc_new_p05": np.percentile(kc_new, 5),
            "kc_new_p95": np.percentile(kc_new, 95), "mean_in_season_share": (in_season_days / days).mean(),
        })

        out_profile = profile.copy()
        out_profile.update(dtype="float32", count=1, nodata=np.nan, compress="lzw",
                           crs=ref["crs"] or CRS.from_epsg(4326))
        with rasterio.open(os.path.join(OUT_DIR, f"kc_{month:02d}.tif"), "w", **out_profile) as dst:
            dst.write(kc_out, 1)

    df = pd.DataFrame(summary).round(3)
    df.to_csv(os.path.join(OUT_DIR, "annual_crop_kc_summary.csv"), index=False)
    print("\n== Annual Crop Kc by month: NDVI (07g) vs paddy curve vs blended ==")
    print(df.to_string(index=False))
    print(f"\nannual mean: NDVI {df.kc_ndvi_mean.mean():.3f} -> blended {df.kc_new_mean.mean():.3f}")
    print(f"done -- see {OUT_DIR}")


if __name__ == "__main__":
    main()
