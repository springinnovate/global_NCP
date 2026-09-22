"""Fetch the Philippines DEM (SRTMGL3, 90m) via NASA AppEEARS.

Mirrors `swy_borneo_run/02_fetch_dem.py` exactly (same product, same AppEEARS mechanics) —
only the AOI and output location differ. Requires `.netrc` set up per `appeears_common.py`'s
docstring (see docs/HANDOFF.md for the one-time account walkthrough).

Product/layers: SRTMGL3_NC.003 (elevation) + SRTMGL3_NUMNC.003 (source-tracking layer). Static
product — the date range is arbitrary and doesn't affect the (single, time-invariant) result.

Note on the AOI submitted to AppEEARS: `ph_aoi.gpkg` is a multi-island archipelago polygon (even
after cleanup, ~115 parts / ~96,000 vertices — see `01_build_aoi_from_rich_mask.py`'s docstring)
and, submitted as-is, produced a multi-MB GeoJSON payload that AppEEARS' API rejected outright
(403 — confirmed by checking payload size directly, not a real permissions problem; simplify()
barely reduced vertex count even at aggressive tolerances, which itself is a real oddity worth
someone eventually digging into rather than just working around). **Fix used here: submit the
AOI's bounding box instead of the precise multipolygon.** This fetches a rectangular DEM covering
some extra open-water area beyond the exact archipelago footprint — harmless, since
`06_extract_lulc_2020.py` and `07_build_biophysical_table.py` both still clip/mask to the precise
`ph_aoi.gpkg` polygon locally afterward. Trades a bit of extra download volume for a working,
simple fetch.
"""
import os
import sys

import geopandas as gpd
from shapely.geometry import box

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "swy_borneo_run"))
from appeears_common import download_bundle, login, submit_area_task, wait_for_task

AOI_PATH = "data/swy/philippines/inputs/ph_aoi.gpkg"
OUT_DIR = "data/swy/philippines/inputs/dem_srtmgl3"


def bbox_geojson(aoi_path):
    aoi = gpd.read_file(aoi_path).to_crs(4326)
    bbox = gpd.GeoDataFrame(geometry=[box(*aoi.total_bounds)], crs=4326)
    return bbox.__geo_interface__

LAYERS = [
    {"product": "SRTMGL3_NC.003", "layer": "SRTMGL3_DEM"},
    {"product": "SRTMGL3_NUMNC.003", "layer": "SRTMGL3_NUM"},
]


def main():
    token = login()
    task_id = submit_area_task(
        token,
        task_name="swy_ph_srtmgl3_dem",
        layers=LAYERS,
        start_date="01-01-2000",
        end_date="12-31-2000",
        aoi_geojson=bbox_geojson(AOI_PATH),
    )
    print(f"submitted task_id: {task_id}")
    wait_for_task(token, task_id, timeout_seconds=10800)
    files = download_bundle(token, task_id, OUT_DIR, name_filter=lambda f: f.endswith(".tif"))

    # Rename to the stable filenames the rest of the pipeline expects.
    for f in files:
        if "SRTMGL3_DEM" in f:
            os.replace(f, os.path.join(OUT_DIR, "srtmgl3_dem_ph.tif"))
        elif "SRTMGL3_NUM" in f:
            os.replace(f, os.path.join(OUT_DIR, "srtmgl3_num_ph.tif"))
    print(f"done — see {OUT_DIR}")


if __name__ == "__main__":
    main()
