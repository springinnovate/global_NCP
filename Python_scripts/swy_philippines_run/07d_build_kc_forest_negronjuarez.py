"""Build 12 monthly per-pixel Kc rasters, using Negron Juarez et al. (2008)'s tropical-forest
ET model for forest pixels (Closed Forest = lulc_id 4, Open Forest = lulc_id 10 -- Becky's own
WWF-SIPA typology, matching the "Tropical & Subtropical Moist Broadleaf Forests" biome row in
docs/swy/cn_kc_biome_coverage_registry.md) and falling back to the existing table-based Kc
(07b_build_biophysical_table_becky_inputs.py's Kamble/EVI regression) everywhere else.

This is the real, un-squashed model, not a linear Kc=a*EVI+b proxy:
    ET_forest = C1 + C2 * EVI^C3 * (Rn - C4)                          [mm/day]
    Kc_forest = ET_forest / ETo_daily
with C1=2.7, C2=0.05, C3=1.75, C4=140 (W/m^2) -- confirmed directly from the paper text
(docs/swy/literature_pdfs/negronjuarez2008.pdf, eq. 2 and its surrounding text: "RnISCCP
(measured in Wm^-2)... ET_site, measured in mm day^-1"). These are the Amazon-fitted constants,
used here UNCHANGED -- this is a transfer test, not a recalibration. See
docs/swy/cn_kc_biome_coverage_registry.md for why: there's no Philippine/SE-Asian flux-tower
dataset to recalibrate against, so the honest move is to run the untuned transfer and let the
comparison against WWF-SIPA's own baseline say whether it holds up, the same way every other
CN/Kc question in this investigation has been resolved.

Net radiation input (`05b_fetch_net_radiation_power.py`'s output) is coarse (~0.5-1deg, NASA
POWER's native grid) relative to EVI (1km) -- resampled here with bilinear onto the EVI grid,
this pipeline's established rule for continuous fields (see the 2026-09-17 rain-events
checkerboard bug). Reference net-radiation range across the AOI (all 12 months, already checked):
77-217 W/m^2, meaning real portions of the domain (Oct-Feb) sit below C4=140 -- the model's
"vegetation stops helping ET below the radiation threshold" behavior genuinely engages here, not
just a theoretical edge case (see this session's conversation for the full reasoning; no pixel
gets close to ET=0 given realistic Philippine forest EVI, since that would need EVI>~0.92).

ETo (TerraClimate) is a MONTHLY TOTAL in mm, confirmed by inspection (July mean ~112mm, clearly
not a daily value) -- divided by days-in-month here to get the daily value Kc's ratio needs.

Non-forest classes keep whatever Kc this project's biophysical table already has for them
(Kamble/EVI regression, or fixed values for water/urban) -- this script only touches forest.

Inputs:
    data/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif
    data/swy/philippines/inputs/mod13a3_evi_2020/*_EVI_*.tif, *_VI_Quality_*.tif
    data/swy/philippines/inputs/net_radiation_power_2020/net_radiation_ph_2020_MM_wm2.tif
    data/swy/philippines/inputs/et0_terraclimate_2020/terraclimate_et0_ph_2020_MM.tif
    data/swy/philippines/INPUTS_SP/ph_biophysical_table_ncp_kc_cn.csv  (non-forest Kc fallback)

Output:
    data/swy/philippines/inputs/kc_forest_negronjuarez_2020/kc_MM.tif  (one band each, EVI's 1km
    grid, WGS84 -- inspring's own _reclassify_or_clip() bilinear-warps any raster override onto
    the actual model grid at run time, so no pre-alignment to the DEM grid is needed here).
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

INPUTS_SP_DIR = "data/swy/philippines/INPUTS_SP"
LULC_PATH = os.path.join(INPUTS_SP_DIR, "ph_baseline_lulc_md5_7f29da.tif")
BIOPHYSICAL_TABLE_PATH = os.path.join(INPUTS_SP_DIR, "ph_biophysical_table_ncp_kc_cn.csv")
EVI_DIR = "data/swy/philippines/inputs/mod13a3_evi_2020"
RN_DIR = "data/swy/philippines/inputs/net_radiation_power_2020"
ETO_DIR = "data/swy/philippines/inputs/et0_terraclimate_2020"
OUT_DIR = "data/swy/philippines/inputs/kc_forest_negronjuarez_2020"

FOREST_LULC_IDS = {4, 10}  # Closed Forest, Open Forest (Becky's WWF-SIPA typology)
YEAR = 2020
QA_GOOD_MAX = 0b01

C1, C2, C3, C4 = 2.7, 0.05, 1.75, 140.0  # Negron Juarez et al. (2008), unmodified Amazon fit
KC_FLOOR = 0.05  # matches 07b's own Kamble-regression floor, applied here for consistency


def _resample_to_ref(src_path, ref_shape, ref_transform, resampling):
    with rasterio.open(src_path) as src:
        out = np.empty(ref_shape, dtype="float32")
        reproject(
            source=rasterio.band(src, 1), destination=out,
            src_transform=src.transform, src_crs=src.crs or CRS.from_epsg(4326),
            dst_transform=ref_transform, dst_crs=CRS.from_epsg(4326),
            resampling=resampling,
            src_nodata=src.nodata, dst_nodata=np.nan,
        )
        return out


def main():
    os.makedirs(OUT_DIR, exist_ok=True)

    evi_files = sorted(glob.glob(f"{EVI_DIR}/MOD13A3.061__1_km_monthly_EVI_doy{YEAR}*.tif"))
    qa_files = sorted(glob.glob(f"{EVI_DIR}/MOD13A3.061__1_km_monthly_VI_Quality_doy{YEAR}*.tif"))
    assert len(evi_files) == 12 and len(qa_files) == 12, (
        f"expected 12 EVI + 12 QA files, got {len(evi_files)}/{len(qa_files)}"
    )

    with rasterio.open(evi_files[0]) as ref:
        ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape
        ref_profile = ref.profile

    lulc_ref = _resample_to_ref(LULC_PATH, ref_shape, ref_transform, Resampling.mode)
    forest_mask = np.isin(np.round(lulc_ref), list(FOREST_LULC_IDS))
    print(f"forest pixels: {forest_mask.sum()} / {forest_mask.size}")

    table = pd.read_csv(BIOPHYSICAL_TABLE_PATH).set_index("lulc_id")

    for month in range(1, 13):
        with rasterio.open(evi_files[month - 1]) as esrc:
            evi = esrc.read(1).astype("float32") * 0.0001
            evi_nodata = esrc.nodata
        with rasterio.open(qa_files[month - 1]) as qsrc:
            qa = qsrc.read(1)
        evi_good = ((qa & 0b11) <= QA_GOOD_MAX)
        if evi_nodata is not None:
            evi_good &= (evi != evi_nodata * 0.0001)

        rn_path = os.path.join(RN_DIR, f"net_radiation_ph_{YEAR}_{month:02d}_wm2.tif")
        eto_path = os.path.join(ETO_DIR, f"terraclimate_et0_ph_{YEAR}_{month:02d}.tif")
        rn = _resample_to_ref(rn_path, ref_shape, ref_transform, Resampling.bilinear)
        eto_monthly = _resample_to_ref(eto_path, ref_shape, ref_transform, Resampling.bilinear)

        days = calendar.monthrange(YEAR, month)[1]
        eto_daily = eto_monthly / days

        evi_clipped = np.clip(evi, 0.0, None)  # EVI^C3 undefined for negative EVI
        et_forest = C1 + C2 * (evi_clipped**C3) * (rn - C4)
        with np.errstate(invalid="ignore", divide="ignore"):
            kc_forest = et_forest / eto_daily
        kc_forest = np.clip(kc_forest, KC_FLOOR, None)

        # start from the existing table's flat per-class Kc, rasterized from LULC
        kc_out = np.full(ref_shape, np.nan, dtype="float32")
        classes_present = [c for c in np.unique(np.round(lulc_ref)) if not np.isnan(c)]
        for lulc_id in classes_present:
            lulc_id_int = int(lulc_id)
            if lulc_id_int not in table.index:
                continue
            class_mask = np.round(lulc_ref) == lulc_id
            kc_out[class_mask] = table.loc[lulc_id_int, f"Kc_{month}"]

        # overwrite forest pixels with the Negron Juarez value where EVI data is usable this month
        apply_mask = forest_mask & evi_good & np.isfinite(kc_forest)
        kc_out[apply_mask] = kc_forest[apply_mask]

        n_below_threshold = int(((rn < C4) & forest_mask)[np.isfinite(rn)].sum())
        print(
            f"month {month:02d}: forest Kc mean={np.nanmean(kc_out[forest_mask]):.3f}, "
            f"forest pixels below Rn threshold={n_below_threshold}"
        )

        out_path = os.path.join(OUT_DIR, f"kc_{month:02d}.tif")
        profile = ref_profile.copy()
        profile.update(dtype="float32", count=1, nodata=np.nan, compress="lzw", crs=ref_crs or CRS.from_epsg(4326))
        with rasterio.open(out_path, "w", **profile) as dst:
            dst.write(kc_out, 1)

    print(f"done -- see {OUT_DIR}")


if __name__ == "__main__":
    main()
