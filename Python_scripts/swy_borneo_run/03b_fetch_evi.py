"""Fetch Borneo EVI (MOD13A3, 1km monthly) via NASA AppEEARS.

Sibling to `03_fetch_ndvi.py` — same product, different layer, added 2026-09-11/14 once the
Kc/NDVI-saturation question came up (see docs/swy/swy_methods.qmd). Requires `.netrc` set up per
appeears_common.py's docstring.

Run this directly (not through Claude Code) if the AppEEARS API is blocking Claude Code's own
requests but not yours — same account, different execution path, worth testing:

    .venv\\Scripts\\python.exe Python_scripts\\swy_borneo_run\\03b_fetch_evi.py
"""
from appeears_common import aoi_to_geojson, download_bundle, login, submit_area_task, wait_for_task

AOI_PATH = "data/swy/borneo/inputs/borneo_aoi.gpkg"
OUT_DIR = "data/swy/borneo/inputs/mod13a3_evi_2020"

LAYERS = [{"product": "MOD13A3.061", "layer": "_1_km_monthly_EVI"}]


def main():
    token = login()
    task_id = submit_area_task(
        token, task_name="swy_borneo_mod13a3_evi_2020", layers=LAYERS,
        start_date="01-01-2020", end_date="12-31-2020",
        aoi_geojson=aoi_to_geojson(AOI_PATH),
    )
    print(f"submitted task_id: {task_id}")
    wait_for_task(token, task_id, timeout_seconds=10800)
    download_bundle(token, task_id, OUT_DIR, name_filter=lambda f: f.endswith((".tif", ".csv")))
    print(f"done — see {OUT_DIR}")


if __name__ == "__main__":
    main()
