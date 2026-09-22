"""Isolated test: does using GCN250 directly as a per-pixel CN raster, instead of this project's
own hand-built lucode-indexed CN table, change the Philippines comparison?

Real background (see docs/reports/swy/swy_status_report.qmd's "Curve number" section and
docs/swy/research_notes.md's 2026-09-17 entry for the full story): `seasonal_water_yield.execute()`
was long assumed to have no raster-override path for CN — that assumption was wrong. Tracing
`_reclassify_or_clip()`'s actual runtime logic (not its docstring, which never mentions this) shows
it checks `args['cn_a_path']`/`args['cn_b_path']`/`args['cn_c_path']`/`args['cn_d_path']` before
falling back to the CSV table, and uses that raster directly (warped onto the model grid via
bilinear) if set. GCN250 doesn't map onto the four soil-group slots directly, though — it's
delivered as three rasters by antecedent moisture condition (dry/average/wet = ARC-I/II/III), not
by soil group, because GCN250 already bakes soil group into its own values (crosswalped against
HYSOGs250m during its own construction, Jaafar et al. 2019). Correct usage: point all four of
cn_a_path-cn_d_path at the same ARC-II ("average") raster -- whichever nominal soil-group slot the
model's own soil_group_path-driven logic selects per pixel, the value is identical anyway.

Kc and root_depth still come from this project's own biophysical table (unaffected by this test --
CN is the only variable being swapped here). Everything else matches
09c_run_swy_ph_becky_inputs_90m_rainfix.py exactly (90m DEM, real rain events, real soil-group
raster, real WWF-SIPA precip/ET0) -- own workspace/results_suffix so this never touches that run's
already-reported output.

    MSYS_NO_PATHCONV=1 docker run --rm -v "$(pwd)/data:/data" -v "$(pwd)/Python_scripts:/scripts" \\
        swy_borneo_run:rainfix4 micromamba run -n geopy311 python \\
        /scripts/swy_philippines_run/09d_run_swy_ph_gcn250_direct.py
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
    workspace = os.path.join(DATA_ROOT, "swy/philippines/workspace_becky_inputs_90m_gcn250")
    os.makedirs(workspace, exist_ok=True)

    et0_staging = _normalize_et0_dir(
        os.path.join(inputs_sp, "Global-ET0_v3_monthly_tifs"),
        os.path.join(workspace, "et0_v3_normalized"),
    )

    dem_path = os.path.join(DATA_ROOT, "swy/philippines/inputs/dem_srtmgl3/srtmgl3_dem_ph.tif")
    lulc_wgs84_path = _reproject_lulc_to_wgs84(
        os.path.join(inputs_sp, "ph_baseline_lulc_md5_7f29da.tif"),
        os.path.join(workspace, "ph_baseline_lulc_wgs84.tif"),
        dem_path,
    )

    # GCN250 ARC-II ("average" antecedent moisture), clipped to the Philippines AOI + 0.05deg
    # buffer, nodata-filled (nearest valid neighbor). Same file for all four soil-group slots --
    # see module docstring for why that's the correct usage, not a shortcut.
    #
    # Nodata-filled, not just clipped: `geoprocessing.warp_raster` (called inside
    # `_reclassify_or_clip` for this raster-override path) takes no nodata parameter at all and
    # doesn't propagate the source's own nodata (255) to its output -- confirmed by inspecting its
    # signature directly. A first attempt with the plain clipped file left 1.17% of valid model
    # pixels reading a literal CN of 255 (GCN250's own coastal no-data gaps, where its coverage
    # doesn't quite match this project's DEM/LULC-derived land mask), concentrated in a fringe
    # around every island -- confirmed by direct inspection, not assumed. Filling those gaps with
    # `scipy.ndimage.distance_transform_edt`'s nearest-valid-neighbor fill before this run means
    # there's no literal 255 left in the file for the broken warp to mishandle.
    gcn250_path = os.path.join(DATA_ROOT, "swy/philippines/inputs/gcn250_arcii_ph_filled.tif")

    args = {
        "workspace_dir": workspace,
        "results_suffix": "ph_gcn250_direct",
        "aoi_path": os.path.join(DATA_ROOT, "swy/philippines/inputs/ph_aoi.gpkg"),
        "dem_raster_path": dem_path,
        "lulc_raster_path": lulc_wgs84_path,
        "lucode_field": "lulc_id",
        "soil_group_path": os.path.join(
            DATA_ROOT, "swy/philippines/inputs/soil_hydrologic_group_hysogs250m_raw_ph.tif"
        ),
        "precip_dir": os.path.join(inputs_sp, "precip_PH_historical_climate_50"),
        "et0_dir": et0_staging,
        # Still needed: Kc and root_depth have no {factor}_path override set below, so
        # _reclassify_or_clip() falls back to this table for those factors, same as every other
        # run. CN_A/B/C/D columns in this table are ignored -- overridden by the four args below.
        "biophysical_table_path": os.path.join(inputs_sp, "ph_biophysical_table_ncp_kc_cn.csv"),
        # The actual variable under test: GCN250 direct, bypassing the CN columns in the table
        # above entirely. All four point at the same file -- see module docstring.
        "cn_a_path": gcn250_path,
        "cn_b_path": gcn250_path,
        "cn_c_path": gcn250_path,
        "cn_d_path": gcn250_path,
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
