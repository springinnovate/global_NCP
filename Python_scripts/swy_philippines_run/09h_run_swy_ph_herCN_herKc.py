"""Factorial cell 4 of 4 (the direct replication check): her CN and her Kc together, run through
our own pipeline at 90m (grid-nested to her grid) instead of her native 30m. Mirrors `09f`/`09g` --
only difference is `biophysical_table_path` points at her own, completely unmodified
`biophysical_template_PH_revised.csv` (no hybrid swap needed, she already has both CN_A-D and
Kc_1-12 in it -- the raster CN overrides in her `.ini` were themselves built by reclassifying this
same table onto the LULC, so this is equivalent to her raster-override path, just via the table
route).

Purpose: this is the direct test of "are we faithfully reproducing her calculation, separate from
the CN/Kc choice itself" -- if QF/B here closely track her actual baseline (allowing for the
resolution difference), that's strong evidence our own pipeline/DEM/grid work is sound and every
remaining gap in the our-CN/our-Kc run traces to the deliberate CN/Kc choice, not a hidden bug.
Already have strong partial evidence of this from 09g (her CN alone reproduces her QF at r=0.92) --
this cell confirms it with both her CN and her Kc together, not split.

    MSYS_NO_PATHCONV=1 docker run --rm -w /scripts_root -e SWY_DATA_ROOT=/scripts_root/data \\
        --mount type=bind,source="$(pwd)",target=/scripts_root swy_borneo_run:rainfix4 \\
        micromamba run -n geopy311 python \\
        /scripts_root/Python_scripts/swy_philippines_run/09h_run_swy_ph_herCN_herKc.py
"""
import os

from osgeo import gdal

DATA_ROOT = os.environ.get("SWY_DATA_ROOT", "/data")


def _reproject_lulc_to_wgs84(src_path, dst_path, dem_path):
    if os.path.exists(dst_path):
        return dst_path
    dem_gt = gdal.Info(dem_path, format="json")["geoTransform"]
    x_res, y_res = abs(dem_gt[1]), abs(dem_gt[5])
    gdal.Warp(
        dst_path, src_path, dstSRS="EPSG:4326", xRes=x_res, yRes=y_res,
        resampleAlg="mode", srcNodata=128, dstNodata=128,
        creationOptions=["COMPRESS=LZW"],
    )
    return dst_path


def _normalize_et0_dir(src_dir, staging_dir):
    import glob
    import re
    import shutil

    os.makedirs(staging_dir, exist_ok=True)
    src_files = glob.glob(os.path.join(src_dir, "*.tif"))
    month_re = re.compile(r"(\d{2})\.tif$")
    found_months = set()
    for src_path in src_files:
        m = month_re.search(os.path.basename(src_path))
        if not m:
            continue
        month = int(m.group(1))
        found_months.add(month)
        dst_path = os.path.join(staging_dir, f"et0_v3_{month:02d}.tif")
        if not os.path.exists(dst_path):
            shutil.copyfile(src_path, dst_path)
    assert found_months == set(range(1, 13)), (
        f"expected months 1-12 in {src_dir}, found {sorted(found_months)}"
    )
    return staging_dir


def main():
    from inspring.seasonal_water_yield import seasonal_water_yield

    inputs_sp = os.path.join(DATA_ROOT, "swy/philippines/INPUTS_SP")
    workspace = os.path.join(DATA_ROOT, "swy/philippines/workspace_becky_inputs_90m_herCN_herKc")
    os.makedirs(workspace, exist_ok=True)

    et0_staging = _normalize_et0_dir(
        os.path.join(inputs_sp, "Global-ET0_v3_monthly_tifs"),
        os.path.join(workspace, "et0_v3_normalized"),
    )

    dem_path = os.path.join(DATA_ROOT, "swy/philippines/inputs/dem_srtmgl3_ph_snapped.tif")
    lulc_wgs84_path = _reproject_lulc_to_wgs84(
        os.path.join(inputs_sp, "ph_baseline_lulc_md5_7f29da.tif"),
        os.path.join(workspace, "ph_baseline_lulc_wgs84.tif"),
        dem_path,
    )

    args = {
        "workspace_dir": workspace,
        "results_suffix": "ph_becky_inputs_90m_herCN_herKc",
        "aoi_path": os.path.join(DATA_ROOT, "swy/philippines/inputs/ph_aoi.gpkg"),
        "dem_raster_path": dem_path,
        "lulc_raster_path": lulc_wgs84_path,
        "lucode_field": "lulc_id",
        "soil_group_path": os.path.join(
            DATA_ROOT, "swy/philippines/inputs/soil_hydrologic_group_hysogs250m_raw_ph.tif"
        ),
        "precip_dir": os.path.join(inputs_sp, "precip_PH_historical_climate_50"),
        "et0_dir": et0_staging,
        # Her own table, completely unmodified -- her CN and her Kc together.
        "biophysical_table_path": os.path.join(inputs_sp, "biophysical_template_PH_revised.csv"),
        "user_defined_rain_events_dir": os.path.join(
            inputs_sp, "n_events_PH_historical_climate_50"
        ),
        "rain_events_table_path": os.path.join(
            DATA_ROOT, "swy/philippines/lulc/rain_events_table.csv"
        ),
        "threshold_flow_accumulation": 1000,
        "alpha_m": 0.018,
        "beta_i": 1,
        "gamma": 1,
        "monthly_alpha": False,
        "user_defined_local_recharge": False,
        "user_defined_climate_zones": False,
        "n_workers": -1,
    }

    print("Calling inspring.seasonal_water_yield.execute() with:")
    for k, v in args.items():
        print(f"  {k}: {v}")

    seasonal_water_yield.execute(args)
    print(f"\nDONE — outputs in {workspace}")


if __name__ == "__main__":
    main()
