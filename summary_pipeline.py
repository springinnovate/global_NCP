"""Summarize reference data across dynamic data."""

import logging
import os
import sys
from datetime import datetime

from ecoshard.geoprocessing import zonal_statistics
from ecoshard import taskgraph
from tqdm import tqdm
import pandas as pd

logging.basicConfig(
    level=logging.DEBUG,
    stream=sys.stdout,
    format=(
        "%(asctime)s (%(relativeCreated)d) %(levelname)s %(name)s"
        " [%(funcName)s:%(lineno)d] %(message)s"
    ),
)

REFERENCE_SUMMARY_VECTOR_PATHS = {
    "hydrosheds_lv6_synth": "./data/reference/hydrosheds_lv6_synth.gpkg"
}

# Tag format is a tuple ([description], [YYYY])
REFERNCE_LANDCOVER_RASTER_PATHS = {
    "landcover_gl_1995": "./data/reference/landcover_gl_1995.tif",
    "landcover_gl_1992": "./data/reference/landcover_gl_1992.tif",
}

# Tag format is a tuple ([description], [YYYY])
ANALYSIS_DATA = {
    (
        "GHS_BUILT_S_E2020_GLOBE_R2023A_4326_3ss_V1_0",
        2020,
    ): "./data/analysis/GHS_BUILT_S_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif",
    (
        "GHS_BUILT_S_E1990_GLOBE_R2023A_4326_3ss_V1_0",
        1990,
    ): "./data/analysis/GHS_BUILT_S_E1990_GLOBE_R2023A_4326_3ss_V1_0.tif",
    (
        "GHS_BUILT_S_E1995_GLOBE_R2023A_4326_3ss_V1_0",
        1995,
    ): "./data/analysis/GHS_BUILT_S_E1995_GLOBE_R2023A_4326_3ss_V1_0.tif",
    (
        "GHS_BUILT_S_E2000_GLOBE_R2023A_4326_3ss_V1_0",
        2000,
    ): "./data/analysis/GHS_BUILT_S_E2000_GLOBE_R2023A_4326_3ss_V1_0.tif",
    (
        "GHS_BUILT_S_E2005_GLOBE_R2023A_4326_3ss_V1_0",
        2005,
    ): "./data/analysis/GHS_BUILT_S_E2005_GLOBE_R2023A_4326_3ss_V1_0.tif",
    (
        "GHS_BUILT_S_E2010_GLOBE_R2023A_4326_3ss_V1_0",
        2010,
    ): "./data/analysis/GHS_BUILT_S_E2010_GLOBE_R2023A_4326_3ss_V1_0.tif",
    (
        "GHS_BUILT_S_E2015_GLOBE_R2023A_4326_3ss_V1_0",
        2015,
    ): "./data/analysis/GHS_BUILT_S_E2015_GLOBE_R2023A_4326_3ss_V1_0.tif",
}

# percent change of any ecosystem service
# don't look at landcover change

WORKSPACE_DIR = "./summary_pipeline_workspace"
os.makedirs(WORKSPACE_DIR, exist_ok=True)


def main():
    """Entry point."""
    task_graph = taskgraph.TaskGraph(
        WORKSPACE_DIR, n_workers=-1, reporting_interval=15.0
    )
    zonal_stats_task_list = []
    for (description, year), raster_path in ANALYSIS_DATA.items():
        zonal_stats_task = task_graph.add_task(
            func=zonal_statistics,
            args=(
                (raster_path, 1),
                REFERENCE_SUMMARY_VECTOR_PATHS["hydrosheds_lv6_synth"],
            ),
            kwargs={
                "polygons_might_overlap": False,
                "working_dir": WORKSPACE_DIR,
            },
            store_result=True,
            task_name=f"stats for {description}",
        )
        zonal_stats_task_list.append((description, year, zonal_stats_task))
    task_graph.join()
    row_list = []
    for description, year, zonal_task in tqdm(zonal_stats_task_list):
        zonal_stats = zonal_task.get()
        for fid, stats in zonal_stats.items():
            # mean approximated by dividing by number of valid pixels
            mean_value = (
                stats["sum"] / stats["count"] if stats["count"] > 0 else None
            )

            row_list.append(
                {
                    "raster id": description,
                    "year": year,
                    "fid": fid,
                    "min": stats["min"],
                    "max": stats["max"],
                    "sum": stats["sum"],
                    "count": stats["count"],
                    "nodata_count": stats["nodata_count"],
                    "mean": mean_value,
                }
            )

    df = pd.DataFrame(row_list)
    timestamp = datetime.now().strftime("%Y_%m_%d_%H_%M_%S")
    filename = f"zonal_stats_{timestamp}.csv"
    df.to_csv(filename, index=False)


if __name__ == "__main__":
    main()
