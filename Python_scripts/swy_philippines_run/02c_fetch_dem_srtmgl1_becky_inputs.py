"""Fetch the Philippines DEM at SRTMGL1 (30m), for the Becky-inputs NCP Kc/CN comparison run,
via NASA AppEEARS.

Why this exists, 2026-09-15 night: the original Becky-inputs run reused this project's
already-fetched SRTMGL3 (90m) DEM from the Borneo pipeline, since neither her `INPUTS_SP/` folder
nor her `.ini` includes a DEM path — a convenience decision (research_notes.md:1003-1005), not one
that weighed resolution against the run's explicit isolated-Kc/CN-variable design goal. The user
caught this: WWF-SIPA's own baseline run sets `TARGET_PIXEL_SIZE=30` explicitly, so reusing a 90m
DEM introduced a second, unintended confound (resolution) on top of the intended one (Kc/CN).
Fetching SRTMGL1 (30m) instead — confirmed available for this AOI via AppEEARS' public product
endpoint (`SRTMGL1_NC.003` / `SRTMGL1_NUMNC.003`, same layer-naming convention as SRTMGL3) — and
setting `TARGET_PIXEL_SIZE` to match hers removes that confound.

Mirrors `02_fetch_dem.py`'s bbox workaround exactly (same AOI, same 403-on-precise-multipolygon
issue — see that script's docstring for the full story) — only the product/output differ.

Product/layers: SRTMGL1_NC.003 (elevation) + SRTMGL1_NUMNC.003 (source-tracking layer). Static
product — the date range is arbitrary and doesn't affect the (single, time-invariant) result.
"""
import os
import sys

import geopandas as gpd
from shapely.geometry import box

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "swy_borneo_run"))
from appeears_common import download_bundle, login, submit_area_task, wait_for_task

AOI_PATH = "data/swy/philippines/inputs/ph_aoi.gpkg"
OUT_DIR = "data/swy/philippines/inputs/dem_srtmgl1_becky_inputs"

LAYERS = [
    {"product": "SRTMGL1_NC.003", "layer": "SRTMGL1_DEM"},
    {"product": "SRTMGL1_NUMNC.003", "layer": "SRTMGL1_NUM"},
]


def bbox_geojson(aoi_path):
    aoi = gpd.read_file(aoi_path).to_crs(4326)
    bbox = gpd.GeoDataFrame(geometry=[box(*aoi.total_bounds)], crs=4326)
    return bbox.__geo_interface__


def main():
    token = login()
    task_id = submit_area_task(
        token,
        task_name="swy_ph_srtmgl1_dem_becky_inputs",
        layers=LAYERS,
        start_date="01-01-2000",
        end_date="12-31-2000",
        aoi_geojson=bbox_geojson(AOI_PATH),
    )
    print(f"submitted task_id: {task_id}")
    wait_for_task(token, task_id, timeout_seconds=10800)
    files = download_bundle(token, task_id, OUT_DIR, name_filter=lambda f: f.endswith(".tif"))

    for f in files:
        if "SRTMGL1_DEM" in f:
            os.replace(f, os.path.join(OUT_DIR, "srtmgl1_dem_ph.tif"))
        elif "SRTMGL1_NUM" in f:
            os.replace(f, os.path.join(OUT_DIR, "srtmgl1_num_ph.tif"))
    print(f"done — see {OUT_DIR}")


if __name__ == "__main__":
    main()
