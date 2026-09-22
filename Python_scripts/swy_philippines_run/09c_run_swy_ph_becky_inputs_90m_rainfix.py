"""Isolated test: does WWF-SIPA's real spatially-distributed rain-events product change the
comparison, independent of the DEM-resolution fix being tested separately (in parallel, in the
main `workspace_becky_inputs` run)?

Deliberately uses the OLD 90m SRTMGL3 DEM (not the new 30m SRTMGL1), so this variable — rain
events on vs. off — is isolated from the resolution variable, and so this run is fast (this
project has spare CPU/memory headroom to run it alongside the 30m job rather than waiting).
Everything else is identical to `09b_run_swy_ph_becky_inputs.py`. Own workspace/results_suffix so
this never touches the other run's files.

Requires the image built from the 2026-09-16 Dockerfile (`docker build -t swy_borneo_run:rainfix
-f Python_scripts/swy_borneo_run/Dockerfile Python_scripts/swy_borneo_run`), which patches both
real upstream `inspring` bugs in the `user_defined_rain_events_dir` code path — see that
Dockerfile's own comments for the full story, including why the second bug (previously left
unpatched over a data-loss concern that didn't hold up on re-examination) is now fixed too.

    MSYS_NO_PATHCONV=1 docker run --rm -v "$(pwd)/data:/data" -v "$(pwd)/Python_scripts:/scripts" \\
        swy_borneo_run:rainfix micromamba run -n geopy311 python \\
        /scripts/swy_philippines_run/09c_run_swy_ph_becky_inputs_90m_rainfix.py
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
        dst_path,
        src_path,
        dstSRS="EPSG:4326",
        xRes=x_res,
        yRes=y_res,
        resampleAlg="mode",
        srcNodata=128,
        dstNodata=128,
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
    workspace = os.path.join(DATA_ROOT, "swy/philippines/workspace_becky_inputs_90m_rainfix")
    os.makedirs(workspace, exist_ok=True)

    et0_staging = _normalize_et0_dir(
        os.path.join(inputs_sp, "Global-ET0_v3_monthly_tifs"),
        os.path.join(workspace, "et0_v3_normalized"),
    )

    # Deliberately the OLD 90m DEM -- this run tests the rain-events fix in isolation, not
    # resolution. The 30m comparison is running separately, in its own workspace.
    dem_path = os.path.join(DATA_ROOT, "swy/philippines/inputs/dem_srtmgl3/srtmgl3_dem_ph.tif")
    lulc_wgs84_path = _reproject_lulc_to_wgs84(
        os.path.join(inputs_sp, "ph_baseline_lulc_md5_7f29da.tif"),
        os.path.join(workspace, "ph_baseline_lulc_wgs84.tif"),
        dem_path,
    )

    args = {
        "workspace_dir": workspace,
        "results_suffix": "ph_becky_inputs_90m_rainfix",
        "aoi_path": os.path.join(DATA_ROOT, "swy/philippines/inputs/ph_aoi.gpkg"),
        "dem_raster_path": dem_path,
        "lulc_raster_path": lulc_wgs84_path,
        "lucode_field": "lulc_id",
        # Raw ORNL DAAC HYSOGs250m.tif (Ross et al. 2018), not NatCap's own "reclassified"
        # Data Hub copy -- the latter showed a real, regular checkerboard artifact even in its
        # own native-resolution pixels (confirmed 2026-09-16 by direct inspection), while this raw
        # source shows genuine fine-scale texture. Also matches what Becky's own .ini references
        # (HYSOGs250m_md5_517bfa.tif) far more closely -- same encoding (1-4 base classes, 11-14
        # dual classes for shallow-water-table soils), collapsed to base classes the same way her
        # own SOIL_HYDROLOGIC_MAP does. Clipped to the Philippines AOI (+0.05deg buffer) from the
        # global file, dual codes collapsed, via a one-off script, not part of the numbered pipeline.
        "soil_group_path": os.path.join(
            DATA_ROOT, "swy/philippines/inputs/soil_hydrologic_group_hysogs250m_raw_ph.tif"
        ),
        "precip_dir": os.path.join(inputs_sp, "precip_PH_historical_climate_50"),
        "et0_dir": et0_staging,
        "biophysical_table_path": os.path.join(inputs_sp, "ph_biophysical_table_ncp_kc_cn.csv"),
        # The actual variable under test: WWF-SIPA's real spatially-distributed rain events,
        # instead of the flat 18-events/month placeholder every other run in this project uses.
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
