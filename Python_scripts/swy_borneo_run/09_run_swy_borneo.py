"""Attempt the actual Borneo SWY run via inspring.seasonal_water_yield.execute().

Run inside the Docker image built from this directory's Dockerfile — inspring and its
dependencies (ecoshard's bundled geoprocessing/taskgraph) are not installed in the project's
plain .venv. From the repo root:

    docker build -t swy_borneo_run -f Python_scripts/swy_borneo_run/Dockerfile \
        Python_scripts/swy_borneo_run
    docker run --rm -v "$(pwd)/data:/data" swy_borneo_run \
        micromamba run -n geopy311 python /data/../Python_scripts/swy_borneo_run/run_swy_borneo.py

(or mount the whole repo and run it from there — whatever's convenient; the only hard
requirement is that DATA_ROOT below resolves to this repo's data/ directory inside the
container).

Uses the CSV biophysical-table path (lucode + CN_A-D + Kc_1-12), not inspring's raster-override
args — simpler and already fully built; see docs/swy/swy_methods.qmd for why the raster-override
path is architecturally preferable long-term, and docs/swy/research_notes.md for why the CSV
path was the pragmatic choice for this specific attempt.

Known, deliberate compromises in this run — read before interpreting output as more than a
mechanical smoke test:
  - Rain events table is a placeholder (18/month, not derived from real daily CHIRPS) —
    see build_rain_events_table.py's docstring.
  - `root_depth` is a placeholder (1000mm) — confirmed dead code in this inspring fork.
  - Kc uses the NDVI regression uniformly across all vegetated classes, including cropland,
    rather than FAO-56 for cropland (no region-correct crop calendar available yet).
"""
import os

DATA_ROOT = os.environ.get("SWY_DATA_ROOT", "/data")


def main():
    from inspring.seasonal_water_yield import seasonal_water_yield

    workspace = os.path.join(DATA_ROOT, "swy_borneo_workspace")
    os.makedirs(workspace, exist_ok=True)

    args = {
        "workspace_dir": workspace,
        "results_suffix": "borneo_2020_test",
        "aoi_path": os.path.join(DATA_ROOT, "swy_shared_package/borneo_aoi.gpkg"),
        "dem_raster_path": os.path.join(DATA_ROOT, "swy_shared_package/dem_srtmgl3/srtmgl3_dem_borneo.tif"),
        "lulc_raster_path": os.path.join(DATA_ROOT, "borneo_lulc/lulc_borneo_2020.tif"),
        "lucode_field": "lucode",
        "soil_group_path": os.path.join(
            DATA_ROOT, "swy_shared_package/soil_hydrologic_group/HYSOGs250m_Soil_Groups_reclassified.tif"
        ),
        "precip_dir": os.path.join(DATA_ROOT, "swy_shared_package/precip_chirps_2020"),
        "et0_dir": os.path.join(DATA_ROOT, "swy_shared_package/et0_terraclimate_2020"),
        "biophysical_table_path": os.path.join(DATA_ROOT, "borneo_lulc/borneo_biophysical_table_2020.csv"),
        "rain_events_table_path": os.path.join(DATA_ROOT, "borneo_lulc/rain_events_table.csv"),
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
