"""Fetch Philippines EVI (MOD13A3, 1km monthly) via NASA AppEEARS.

Sibling to `03_fetch_ndvi.py` — same product, different layer. Uses the AOI's bounding box for
the AppEEARS submission, same reason as `02_fetch_dem.py`/`03_fetch_ndvi.py`: the precise
multi-island AOI is too complex for AppEEARS' API to accept.

Run this directly (not through Claude Code) if the AppEEARS API is blocking Claude Code's own
requests but not yours — same account, different execution path, worth testing:

    .venv\\Scripts\\python.exe Python_scripts\\swy_philippines_run\\03b_fetch_evi.py
"""
import os
import sys

import geopandas as gpd
from shapely.geometry import box

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "swy_borneo_run"))
from appeears_common import download_bundle, login, submit_area_task, wait_for_task

AOI_PATH = "data/swy/philippines/inputs/ph_aoi.gpkg"
OUT_DIR = "data/swy/philippines/inputs/mod13a3_evi_2020"

LAYERS = [{"product": "MOD13A3.061", "layer": "_1_km_monthly_EVI"}]


def bbox_geojson(aoi_path):
    aoi = gpd.read_file(aoi_path).to_crs(4326)
    bbox = gpd.GeoDataFrame(geometry=[box(*aoi.total_bounds)], crs=4326)
    return bbox.__geo_interface__


def main():
    token = login()
    task_id = submit_area_task(
        token, task_name="swy_ph_mod13a3_evi_2020", layers=LAYERS,
        start_date="01-01-2020", end_date="12-31-2020",
        aoi_geojson=bbox_geojson(AOI_PATH),
    )
    print(f"submitted task_id: {task_id}")
    wait_for_task(token, task_id, timeout_seconds=10800)
    download_bundle(token, task_id, OUT_DIR, name_filter=lambda f: f.endswith((".tif", ".csv")))
    print(f"done — see {OUT_DIR}")


if __name__ == "__main__":
    main()
