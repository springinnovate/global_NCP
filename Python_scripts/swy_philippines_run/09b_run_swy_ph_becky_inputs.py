"""Run inspring.seasonal_water_yield on Becky's Philippines inputs, with this project's own Kc/CN.

The isolated-variable comparison Becky asked for (2026-09-14): her DEM [not delivered, see below],
her LULC, her real 30-year CMIP6-derived climate stack (precip, ET0, rain events) — but Kc and CN
swapped for this project's own EVI-regression/ESA-crosswalk methodology (built by
`07b_build_biophysical_table_becky_inputs.py`).

Run inside the same Docker image as the other SWY runs — no changes needed there, `inspring` has
no region-specific code:

    docker build -t swy_borneo_run -f Python_scripts/swy_borneo_run/Dockerfile \\
        Python_scripts/swy_borneo_run
    MSYS_NO_PATHCONV=1 docker run --rm -v "$(pwd)/data:/data" -v "$(pwd)/Python_scripts:/scripts" \\
        swy_borneo_run micromamba run -n geopy311 python /scripts/swy_philippines_run/09b_run_swy_ph_becky_inputs.py

Real deviations from a pure "just her inputs" run, each a deliberate, documented choice:
  - **DEM**: her `INPUTS_SP/` folder and her `.ini` both omit a DEM path entirely. **Corrected
    2026-09-15 night**: the first attempt at this run reused this project's already-fetched
    SRTMGL3 (90m) DEM from the Borneo pipeline, out of convenience — but her own `.ini` sets
    `TARGET_PIXEL_SIZE = 30` / `GLOBAL_PIXEL_SIZE_DEG = 0.0002777777777777` (exactly 1 arc-second),
    meaning the 90m DEM introduced a *second*, unintended confound (resolution) on top of the
    intended one (Kc/CN) — the whole point of this run was to isolate Kc/CN alone. Fixed by
    fetching SRTMGL1 (30m, exactly 1 arc-second — the actual resolution her value describes) and
    pointing `dem_raster_path` at it instead. **`TARGET_PIXEL_SIZE` itself is not a real parameter
    of this build's `execute()`** — checked directly against `inspring`'s source
    (`seasonal_water_yield.py:279-280`): resolution is always derived from
    `args['dem_raster_path']`'s own native pixel size, no override kwarg exists. Same "her `.ini`
    key doesn't map to a real parameter here" pattern already found for the CN raster-override
    claim — swapping the DEM file itself is the only lever that actually exists, and it happens to
    be sufficient here since SRTMGL1's native resolution already matches her stated value exactly.
  - **CN delivery mechanism**: her `.ini` passes CN as raster overrides (`CN_A_PATH` etc.), a
    parameter shape this project's actual installed `inspring` (confirmed by reading its
    `seasonal_water_yield.execute()` source directly) does not support at all — CN only ever comes
    from `cn_a`/`cn_b`/`cn_c`/`cn_d` columns in the biophysical table, joined via `lucode_field`.
    Her config is therefore for a different wrapper/fork, not a stricter version of this one. No
    functional loss: the biophysical-table path is this codebase's only path, not a fallback.
  - **alpha_m**: her `.ini`'s `MONTHLY_ALPHA = 0.018` almost certainly corresponds to this
    codebase's `alpha_m` (a flat monthly value, `monthly_alpha=False`), not the same-named
    `monthly_alpha` *boolean* toggle this codebase actually has — the value doesn't fit that slot
    (0.018 isn't True/False). Read as her calibrated flat alpha_m and used as such here, replacing
    this project's own placeholder default of 0.083333 (=1/12) used in the plain `09_run_swy_ph.py`
    run — this is real calibration information worth adopting, not a discretionary pick.
  - **Rain events: attempted, reverted.** Her real spatially-distributed monthly rain-event
    rasters were meant to feed `user_defined_rain_events_dir` (this codebase does have that
    parameter). Two real, separate upstream bugs in that ~15-line code block, found in this order:
    (1) `interpolate_list` is sized before the rain-events extension — patched in the Dockerfile,
    see above; (2) after that fix, `n_events_path_list` is built via bare `os.listdir()` (no
    directory prefix) instead of full paths like the precip/ET0 lists two lines above it, AND that
    same list is reused as both the alignment *input* and *output* target — meaning a naive path
    fix would have `align_and_resize_raster_stack` write reprojected rasters back out **on top of
    Becky's original delivered files**. Not patched today — fixing it correctly means giving the
    aligned copies genuinely separate output paths, real feature-completion work that deserves its
    own careful pass, not a rushed mid-run patch with a real data-loss risk if wrong. **Reverted to
    this project's own existing flat 18-events/month placeholder table** (built by
    `08_build_rain_events_table.py`, same file `09_run_swy_ph.py` uses) for this run instead — the
    real per-month climatology upgrade stays a documented, open follow-up.
  - **ET0 filename bug worked around, not upstream**: her `Global-ET0_v3_monthly_tifs/` mixes
    `et0_v3_0X.tif` and `et0_V3_0X.tif` casing (months 05-09 capitalized, others not).
    `seasonal_water_yield.execute()` sorts this directory's filenames as plain strings to assign
    calendar months — ASCII sorts capital `V` before lowercase `v`, so a raw pass-through would
    silently reorder ET0 to May-Sept-then-Jan-Apr-Oct-Dec instead of Jan-Dec. Normalized into a
    lowercase-consistent staging copy before the run (see `_normalize_et0_dir` below) rather than
    editing Becky's delivered files in place.

  - **CRS mismatch, found and fixed**: her LULC raster is natively EPSG:32651 (UTM 51N, meters),
    while the DEM/precip/ET0/soil-group inputs are all WGS84 (degrees).
    `geoprocessing.align_and_resize_raster_stack` (called internally by `execute()`) does not
    reproject mismatched CRSs itself — it intersects each input's raw bounding box as reported in
    its own native CRS, which is meaningless across a meters-vs-degrees mismatch and fails outright
    ("Bounding boxes do not intersect"). Worked around by reprojecting her LULC to EPSG:4326 first
    (`_reproject_lulc_to_wgs84` below, GDAL Warp, mode resampling to preserve categorical values,
    output resolution matched to the DEM's own pixel size since the alignment step resamples to
    that regardless) — a real, necessary preprocessing step, not an inspring bug.

Known, deliberate compromises carried over unchanged from the ESA/own-LULC Philippines run:
  - `root_depth` is a placeholder (1000mm) — confirmed dead code in this inspring fork.
  - AOI is this project's own `ph_aoi.gpkg` (400+-part archipelago multipolygon derived from
    Rich's output footprint) — needed to bound the run; her config doesn't define one at all.
"""
import glob
import os
import re
import shutil

from osgeo import gdal

DATA_ROOT = os.environ.get("SWY_DATA_ROOT", "/data")


def _reproject_lulc_to_wgs84(src_path, dst_path, dem_path):
    """Reproject Becky's UTM-51N LULC raster to WGS84, matching the DEM's own pixel size.

    See this script's module docstring for why this is required, not optional: `execute()`'s own
    alignment step does not reproject across CRSs on its own.
    """
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
    """Copy the 12 ET0 monthly tifs into a consistently-cased, zero-padded staging dir.

    See this script's module docstring for why: `execute()` sorts filenames as plain strings to
    infer calendar order, and the delivered files mix `et0_v3_0X.tif`/`et0_V3_0X.tif` casing.
    """
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
    workspace = os.path.join(DATA_ROOT, "swy/philippines/workspace_becky_inputs_30m")
    os.makedirs(workspace, exist_ok=True)

    et0_staging = _normalize_et0_dir(
        os.path.join(inputs_sp, "Global-ET0_v3_monthly_tifs"),
        os.path.join(workspace, "et0_v3_normalized"),
    )

    dem_path = os.path.join(
        DATA_ROOT, "swy/philippines/inputs/dem_srtmgl1_becky_inputs/srtmgl1_dem_ph_merged.tif"
    )
    lulc_wgs84_path = _reproject_lulc_to_wgs84(
        os.path.join(inputs_sp, "ph_baseline_lulc_md5_7f29da.tif"),
        os.path.join(workspace, "ph_baseline_lulc_wgs84.tif"),
        dem_path,
    )

    args = {
        "workspace_dir": workspace,
        "results_suffix": "ph_becky_inputs_30m",
        "aoi_path": os.path.join(DATA_ROOT, "swy/philippines/inputs/ph_aoi.gpkg"),
        "dem_raster_path": dem_path,
        "lulc_raster_path": lulc_wgs84_path,
        "lucode_field": "lulc_id",
        # Raw ORNL DAAC HYSOGs250m, not NatCap's reclassified copy -- see 09c's own comment and
        # the 2026-09-16 status report for why (a real, regular artifact in NatCap's version,
        # confirmed absent from the raw source and from Becky's own referenced file family).
        "soil_group_path": os.path.join(
            DATA_ROOT, "swy/philippines/inputs/soil_hydrologic_group_hysogs250m_raw_ph.tif"
        ),
        "precip_dir": os.path.join(inputs_sp, "precip_PH_historical_climate_50"),
        "et0_dir": et0_staging,
        "biophysical_table_path": os.path.join(inputs_sp, "ph_biophysical_table_ncp_kc_cn.csv"),
        # Real WWF-SIPA rain events, not the flat placeholder -- the three inspring bugs blocking
        # this are now patched (see 09c / Findings for Rich), same image as the 90m rainfix runs.
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
