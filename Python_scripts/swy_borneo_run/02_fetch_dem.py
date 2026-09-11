"""Fetch the Borneo DEM (SRTMGL3, 90m) via NASA AppEEARS.

Requires `.netrc` set up per appeears_common.py's docstring (NASA Earthdata account with the
"LP DAAC Data Pool" app authorized — see docs/HANDOFF.md for the one-time account walkthrough).

Product/layers: SRTMGL3_NC.003 (elevation) + SRTMGL3_NUMNC.003 (source-tracking layer). Static
product — the date range is arbitrary and doesn't affect the (single, time-invariant) result.
"""
import os

from appeears_common import aoi_to_geojson, download_bundle, login, submit_area_task, wait_for_task

AOI_PATH = "data/swy_shared_package/borneo_aoi.gpkg"
OUT_DIR = "data/swy_shared_package/dem_srtmgl3"

LAYERS = [
    {"product": "SRTMGL3_NC.003", "layer": "SRTMGL3_DEM"},
    {"product": "SRTMGL3_NUMNC.003", "layer": "SRTMGL3_NUM"},
]


def main():
    token = login()
    task_id = submit_area_task(
        token,
        task_name="swy_borneo_srtmgl3_dem",
        layers=LAYERS,
        start_date="01-01-2000",
        end_date="12-31-2000",
        aoi_geojson=aoi_to_geojson(AOI_PATH),
    )
    print(f"submitted task_id: {task_id}")
    wait_for_task(token, task_id)
    files = download_bundle(token, task_id, OUT_DIR, name_filter=lambda f: f.endswith(".tif"))

    # Rename to the stable filenames the rest of the pipeline expects.
    for f in files:
        if "SRTMGL3_DEM" in f:
            os.replace(f, os.path.join(OUT_DIR, "srtmgl3_dem_borneo.tif"))
        elif "SRTMGL3_NUM" in f:
            os.replace(f, os.path.join(OUT_DIR, "srtmgl3_num_borneo.tif"))
    print(f"done — see {OUT_DIR}")


if __name__ == "__main__":
    main()
