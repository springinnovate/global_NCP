"""Fetch Philippines NDVI (MOD13A3, 1km monthly) via NASA AppEEARS.

Mirrors `swy_borneo_run/03_fetch_ndvi.py`. Requires `.netrc` set up per appeears_common.py's
docstring.

Usage:
    python 03_fetch_ndvi.py            # 2020 only — matches the LULC anchor year and Becky's
                                        # shared PH workspace's assumed year (unconfirmed, see
                                        # docs/swy/research_notes.md)
    python 03_fetch_ndvi.py --full     # full 2000-present record — kept local only

Product/layers: MOD13A3.061's NDVI layer + its own QA layer (used to mask low-quality pixels
before the Kc regression — see 07_build_biophysical_table.py).

Note on the AOI submitted to AppEEARS: same fix as `02_fetch_dem.py` — the precise `ph_aoi.gpkg`
multipolygon (~115 parts, ~96,000 vertices) produced a payload AppEEARS' API rejected outright
(403, a size/complexity limit, not a real permissions problem). Submits the AOI's bounding box
instead; `06_extract_lulc_2020.py`/`07_build_biophysical_table.py` still clip to the precise
polygon locally afterward.
"""
import argparse
import os
import sys

import geopandas as gpd
from shapely.geometry import box

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "swy_borneo_run"))
from appeears_common import download_bundle, login, submit_area_task, wait_for_task

AOI_PATH = "data/swy/philippines/inputs/ph_aoi.gpkg"


def bbox_geojson(aoi_path):
    aoi = gpd.read_file(aoi_path).to_crs(4326)
    bbox = gpd.GeoDataFrame(geometry=[box(*aoi.total_bounds)], crs=4326)
    return bbox.__geo_interface__

LAYERS = [
    {"product": "MOD13A3.061", "layer": "_1_km_monthly_NDVI"},
    {"product": "MOD13A3.061", "layer": "_1_km_monthly_VI_Quality"},
]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--full", action="store_true", help="fetch the full 2000-present record")
    args = parser.parse_args()

    if args.full:
        start_date, end_date = "01-01-2000", "12-31-2026"
        out_dir = "data/swy/philippines/raw_downloads/mod13a3_ph_full_record"
        task_name = "swy_ph_mod13a3_ndvi_full_record"
    else:
        start_date, end_date = "01-01-2020", "12-31-2020"
        out_dir = "data/swy/philippines/inputs/mod13a3_ndvi_2020"
        task_name = "swy_ph_mod13a3_ndvi_2020"

    token = login()
    task_id = submit_area_task(
        token, task_name=task_name, layers=LAYERS,
        start_date=start_date, end_date=end_date,
        aoi_geojson=bbox_geojson(AOI_PATH),
    )
    print(f"submitted task_id: {task_id}")
    wait_for_task(token, task_id, timeout_seconds=10800)
    download_bundle(token, task_id, out_dir, name_filter=lambda f: f.endswith((".tif", ".csv")))
    print(f"done — see {out_dir}")


if __name__ == "__main__":
    main()
