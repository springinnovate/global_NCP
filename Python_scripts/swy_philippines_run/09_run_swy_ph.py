"""Attempt the actual Philippines SWY run via inspring.seasonal_water_yield.execute().

Mirrors `swy_borneo_run/09_run_swy_borneo.py` exactly in mechanics — run inside the same Docker
image (build from `swy_borneo_run/Dockerfile`, no changes needed there; it's region-agnostic).
From the repo root:

    docker build -t swy_borneo_run -f Python_scripts/swy_borneo_run/Dockerfile \\
        Python_scripts/swy_borneo_run
    MSYS_NO_PATHCONV=1 docker run --rm -v "$(pwd)/data:/data" -v "$(pwd)/Python_scripts:/scripts" \\
        swy_borneo_run micromamba run -n geopy311 python /scripts/swy_philippines_run/09_run_swy_ph.py

(the image name/Dockerfile are still called "swy_borneo_run" — no need for a separate image,
`inspring` itself has no region-specific code)

Uses the CSV biophysical-table path, same pragmatic-for-now choice as Borneo — see
docs/swy/swy_methods.qmd for why the raster-override path is architecturally preferable
long-term.

Known, deliberate compromises in this run — same caveats as Borneo's, plus one Philippines-
specific weakening (see 08_build_rain_events_table.py's docstring):
  - Rain events table is a placeholder (18/month, flat) — WEAKER justification here than for
    Borneo, given the Philippines' real monsoon/typhoon seasonality.
  - `root_depth` is a placeholder (1000mm) — confirmed dead code in this inspring fork.
  - Kc uses the NDVI regression uniformly across all vegetated classes, including cropland.
  - AOI is a 400+-part archipelago multipolygon derived from Rich's own output footprint (see
    01_build_aoi_from_rich_mask.py) — not independently verified against a political boundary.
"""
import os

DATA_ROOT = os.environ.get("SWY_DATA_ROOT", "/data")


def main():
    from inspring.seasonal_water_yield import seasonal_water_yield

    workspace = os.path.join(DATA_ROOT, "swy/philippines/workspace")
    os.makedirs(workspace, exist_ok=True)

    args = {
        "workspace_dir": workspace,
        "results_suffix": "ph_2020_test",
        "aoi_path": os.path.join(DATA_ROOT, "swy/philippines/inputs/ph_aoi.gpkg"),
        "dem_raster_path": os.path.join(DATA_ROOT, "swy/philippines/inputs/dem_srtmgl3/srtmgl3_dem_ph.tif"),
        "lulc_raster_path": os.path.join(DATA_ROOT, "swy/philippines/lulc/lulc_ph_2020.tif"),
        "lucode_field": "lucode",
        "soil_group_path": os.path.join(
            DATA_ROOT, "swy/shared/soil_hydrologic_group/HYSOGs250m_Soil_Groups_reclassified.tif"
        ),
        "precip_dir": os.path.join(DATA_ROOT, "swy/philippines/inputs/precip_chirps_2020"),
        "et0_dir": os.path.join(DATA_ROOT, "swy/philippines/inputs/et0_terraclimate_2020"),
        "biophysical_table_path": os.path.join(DATA_ROOT, "swy/philippines/lulc/ph_biophysical_table_2020.csv"),
        "rain_events_table_path": os.path.join(DATA_ROOT, "swy/philippines/lulc/rain_events_table.csv"),
        "threshold_flow_accumulation": 1000,
        "alpha_m": 0.083333,
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
