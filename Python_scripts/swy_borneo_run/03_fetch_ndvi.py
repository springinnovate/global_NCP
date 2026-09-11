"""Fetch Borneo NDVI (MOD13A3, 1km monthly) via NASA AppEEARS.

Requires `.netrc` set up per appeears_common.py's docstring.

Usage:
    python 03_fetch_ndvi.py            # 2020 only (~40MB) — what actually ships in the
                                        # shared Drive package, matches the LULC anchor year
    python 03_fetch_ndvi.py --full     # full 2000-present record (~1GB) — kept local only,
                                        # deliberately not shipped in the Drive package to keep
                                        # it manageable; useful if testing a different year later

Product/layers: MOD13A3.061's NDVI layer + its own QA layer (used to mask low-quality pixels
before the Kc regression — see 07_build_biophysical_table.py).
"""
import argparse

from appeears_common import aoi_to_geojson, download_bundle, login, submit_area_task, wait_for_task

AOI_PATH = "data/swy_shared_package/borneo_aoi.gpkg"

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
        out_dir = "data/mod13a3_borneo_full_record"
        task_name = "swy_borneo_mod13a3_ndvi_full_record"
    else:
        start_date, end_date = "01-01-2020", "12-31-2020"
        out_dir = "data/swy_shared_package/mod13a3_ndvi_2020"
        task_name = "swy_borneo_mod13a3_ndvi_2020"

    token = login()
    task_id = submit_area_task(
        token, task_name=task_name, layers=LAYERS,
        start_date=start_date, end_date=end_date,
        aoi_geojson=aoi_to_geojson(AOI_PATH),
    )
    print(f"submitted task_id: {task_id}")
    wait_for_task(token, task_id, timeout_seconds=7200)
    download_bundle(token, task_id, out_dir, name_filter=lambda f: f.endswith((".tif", ".csv")))
    print(f"done — see {out_dir}")


if __name__ == "__main__":
    main()
