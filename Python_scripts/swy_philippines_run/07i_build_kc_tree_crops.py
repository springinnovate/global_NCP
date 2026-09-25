"""Tree-crop Kc for Perennial Crop, on top of the paddy Kc (07h_build_kc_paddy_rice.py).

Perennial Crop (lulc_id 12) has so far used the Kamble et al. (2013) NDVI regression, fit on US
High Plains row crops (temperate, semi-arid). Philippine perennial cropland is mostly coconut, plus
banana, other tropical fruit, sugarcane and some coffee/cacao/rubber: tall evergreen canopies in a
humid tropical climate, outside Kamble's calibration domain. This replaces it with FAO-56 Table 12
(Allen et al. 1998) single crop coefficients, weighted by each pixel's MapSPAM 2020 crop mix:

    Kc_perennial = sum_i share_i * Kc_i      (share over the perennial crops in the 5-arcmin cell)

Per-crop Kc is the mean of FAO-56's Kc_ini / Kc_mid / Kc_end (these crops are evergreen or
year-round, so the three values are close and there is no calendar to place). Coconut has no
FAO-56 row; it takes "Palm trees", the standard stand-in. Values used, FAO-56 Table 12 rows
(ini/mid/end), confirmed from https://www.fao.org/4/x0490e/x0490e0b.htm on 2026-09-25:

    CNUT coconut, OILP oil palm -> Palm trees 0.95/1.00/1.00
    BANA banana, PLNT plantain  -> Banana 2nd year 1.00/1.20/1.10
    COCO cacao                  -> Cacao 1.00/1.05/1.05
    COFF, RCOF coffee           -> Coffee, bare ground cover 0.90/0.95/0.95
    RUBB rubber                 -> Rubber trees 0.95/1.00/1.00
    TEAS tea                    -> Tea, non-shaded 0.95/1.00/1.00
    SUGC sugarcane              -> Sugar cane 0.40/1.25/0.75, time-weighted over FAO's ratoon stage
                                   lengths instead of a plain mean (stages differ a lot)
    TROF other tropical fruit, CITR citrus, TEMF temperate fruit -> no single FAO row; assumed 0.95
                                   (a generic evergreen fruit-tree value; flagged, small share)

FAO-56 values assume full canopy cover; no cover adjustment is applied, so pixels with sparse or
young plantations are likely overestimated. Pixels whose cell has no perennial MapSPAM area keep
their Kamble NDVI value. Only Perennial Crop changes; everything else is copied from 07h, so a run
on this output would differ from 09n only in Perennial Crop Kc.

Result 2026-09-25: annual-mean Perennial Crop Kc 0.990 (Kamble) -> 0.985 (tree crops), mean
per-pixel change 0.065. Too small to move baseflow meaningfully, so no model run was made; the
09n results stand. Two independent methods agree on ~1.0, against WWF-SIPA's flat 0.70.

Outputs:
    data/swy/philippines/inputs/kc_tree_crops_2020/kc_MM.tif
    data/swy/philippines/inputs/kc_tree_crops_2020/perennial_kc_summary.csv
"""
import os

import numpy as np
import pandas as pd
import rasterio
from rasterio.crs import CRS
from rasterio.enums import Resampling
from rasterio.warp import reproject

LULC_PATH = "data/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif"
BASE_KC_DIR = "data/swy/philippines/inputs/kc_paddy_2020"
MAPSPAM_DIR = "data/swy/shared/mapspam/raw_downloads"
OUT_DIR = "data/swy/philippines/inputs/kc_tree_crops_2020"
PERENNIAL_CROP_ID = 12


def _mean(*v):
    return sum(v) / len(v)


def _sugarcane():
    # FAO-56 Table 11 sugarcane ratoon stages (days): initial 25, development 70, mid 135, late 50;
    # Kc constant at ini, linear ini->mid over development, constant at mid, linear mid->end late
    ini, mid, end = 0.40, 1.25, 0.75
    days = {"ini": 25, "dev": 70, "mid": 135, "late": 50}
    total = sum(days.values())
    return (ini * days["ini"] + _mean(ini, mid) * days["dev"] + mid * days["mid"]
            + _mean(mid, end) * days["late"]) / total


CROP_KC = {
    "CNUT": _mean(0.95, 1.00, 1.00), "OILP": _mean(0.95, 1.00, 1.00),
    "BANA": _mean(1.00, 1.20, 1.10), "PLNT": _mean(1.00, 1.20, 1.10),
    "COCO": _mean(1.00, 1.05, 1.05),
    "COFF": _mean(0.90, 0.95, 0.95), "RCOF": _mean(0.90, 0.95, 0.95),
    "RUBB": _mean(0.95, 1.00, 1.00), "TEAS": _mean(0.95, 1.00, 1.00),
    "SUGC": _sugarcane(),
    "TROF": 0.95, "CITR": 0.95, "TEMF": 0.95,  # assumed, see docstring
}


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


def tree_kc_mapspam():
    """Area-weighted Kc over perennial crops, on MapSPAM's grid."""
    weighted, total, profile = None, None, None
    for crop, kc in CROP_KC.items():
        with rasterio.open(os.path.join(MAPSPAM_DIR, f"{crop}.tif")) as src:
            if profile is None:
                profile = src.profile
            a = src.read(1).astype("float64")
        a[~np.isfinite(a) | (a < 0)] = 0
        weighted = a * kc if weighted is None else weighted + a * kc
        total = a if total is None else total + a
    with np.errstate(invalid="ignore", divide="ignore"):
        kc = np.where(total > 0, weighted / total, np.nan).astype("float32")
    return kc, profile


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    print("per-crop Kc used:", {k: round(v, 3) for k, v in CROP_KC.items()})

    with rasterio.open(os.path.join(BASE_KC_DIR, "kc_01.tif")) as ref_src:
        ref = {"shape": ref_src.shape, "transform": ref_src.transform, "crs": ref_src.crs}
        profile = ref_src.profile

    lulc = _to_ref(LULC_PATH, ref, Resampling.mode)
    mask = np.round(lulc) == PERENNIAL_CROP_ID
    print(f"Perennial Crop pixels: {mask.sum()}")

    kc_mp, mp_profile = tree_kc_mapspam()
    kc_path = os.path.join(OUT_DIR, "tree_crop_kc_mapspam.tif")
    mp_profile.update(dtype="float32", count=1, nodata=np.nan, compress="lzw")
    with rasterio.open(kc_path, "w", **mp_profile) as dst:
        dst.write(kc_mp, 1)
    kc_tree = _to_ref(kc_path, ref, Resampling.bilinear)
    has_tree = mask & np.isfinite(kc_tree)
    print(f"Perennial Crop pixels with MapSPAM perennial area: {has_tree.sum()} "
          f"({has_tree.sum() / mask.sum():.0%}); the rest keep Kamble NDVI")

    summary = []
    for month in range(1, 13):
        with rasterio.open(os.path.join(BASE_KC_DIR, f"kc_{month:02d}.tif")) as src:
            kc_out = src.read(1).astype("float32")
        old = kc_out[mask].copy()
        kc_out[has_tree] = kc_tree[has_tree]
        new = kc_out[mask]
        summary.append({"month": month, "kc_kamble_mean": float(np.nanmean(old)),
                        "kc_tree_mean": float(np.nanmean(new)),
                        "kc_tree_p05": float(np.nanpercentile(new, 5)),
                        "kc_tree_p95": float(np.nanpercentile(new, 95)),
                        "mean_abs_change": float(np.nanmean(np.abs(new - old)))})
        out_profile = profile.copy()
        out_profile.update(dtype="float32", count=1, nodata=np.nan, compress="lzw",
                           crs=ref["crs"] or CRS.from_epsg(4326))
        with rasterio.open(os.path.join(OUT_DIR, f"kc_{month:02d}.tif"), "w", **out_profile) as dst:
            dst.write(kc_out, 1)

    df = pd.DataFrame(summary).round(3)
    df.to_csv(os.path.join(OUT_DIR, "perennial_kc_summary.csv"), index=False)
    print("\n== Perennial Crop Kc by month: Kamble NDVI (current) vs FAO-56 tree crops ==")
    print(df.to_string(index=False))
    print(f"\nannual mean: Kamble {df.kc_kamble_mean.mean():.3f} -> tree crops {df.kc_tree_mean.mean():.3f}; "
          f"mean absolute per-pixel change {df.mean_abs_change.mean():.3f} (WWF-SIPA: 0.70)")


if __name__ == "__main__":
    main()
