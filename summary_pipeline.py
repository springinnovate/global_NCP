"""Summarize analysis raster data across reference vector data."""

from pathlib import Path
from datetime import datetime
import time
import logging
import os
import sys

from geopandas import gpd
from exactextract import exact_extract
import psutil
from ecoshard import taskgraph

logging.basicConfig(
    level=logging.DEBUG,
    stream=sys.stdout,
    format=(
        "%(asctime)s (%(relativeCreated)d) %(levelname)s %(name)s"
        " [%(funcName)s:%(lineno)d] %(message)s"
    ),
)

LOGGER = logging.getLogger(__name__)
logging.getLogger("ecoshard.taskgraph").setLevel(logging.INFO)

REFERENCE_SUMMARY_VECTOR_PATHS = {
    "hydrosheds_lv6_synth": "./data/reference/hydrosheds_lv6_synth.gpkg"
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


WORKSPACE_DIR = "./summary_pipeline_workspace"
os.makedirs(WORKSPACE_DIR, exist_ok=True)
REPORTING_INTERVAL = 10.0
ZONAL_OPS = ["mean", "max", "min"]


def create_progress_logger(update_rate, task_id):
    """Create a logging function to report progress of a long-running task.

    This function returns a nested logger function that logs the progress of
    a task based on a specified update interval. Each logged message includes
    the task identifier, the completion percentage, and the elapsed time since
    the task started.

    Args:
        update_rate (float): Minimum time interval (in seconds) between
            consecutive log updates.
        task_id (str): Identifier for the task, included in each log message.

    Returns:
        callable: A logger function that accepts two arguments:
            fraction (float): Progress of the task as a float between 0 and 1.
            message (str): Additional message describing the current task state.
    """
    start_time = time.time()
    last_time = start_time

    def _process_logger(fraction, message):
        nonlocal last_time
        if time.time() - last_time > update_rate:
            LOGGER.info(
                f"{task_id} is {100*fraction:.2f}% complete running for "
                f"{time.time()-start_time:.2f}s"
            )
            last_time = time.time()

    return _process_logger


def zonal_stats(raster_path, vector_path):
    """Calculate zonal statistics for vector file over a given raster.

    This function reads polygon geometries from a vector file, assigns a
    unique identifier (FID) to each geometry, and computes zonal statistics
    such as mean, sum, count, min, and max of the raster values intersecting
    each polygon. Statistics calculation uses exact extraction methods,
    ensuring accuracy particularly at polygon boundaries.

    Args:
        raster_path (str): Path to the input raster file.
        vector_path (str): Path to the vector file containing polygon
            geometries.

    Returns:
        pandas.DataFrame: DataFrame containing zonal statistics with one row
        per polygon, identified by 'fid' and columns for each calculated
        statistic named from `ZONAL_OPS`.
    """
    gdf = gpd.read_file(vector_path)

    # need to get the FID column in there so we can join results
    gdf = gdf[["geometry"]].copy()
    gdf["fid"] = gdf.index.astype("int32")

    stem = Path(raster_path).stem
    stats_df = exact_extract(
        rast=raster_path,
        vec=gdf,
        ops=ZONAL_OPS,
        include_cols=["fid"],
        output="pandas",
        strategy="raster-sequential",
        progress=create_progress_logger(REPORTING_INTERVAL, stem),
    )
    return stats_df


def main():
    """Entry point."""
    physical_cores = psutil.cpu_count(logical=False)
    task_graph = taskgraph.TaskGraph(
        WORKSPACE_DIR,
        n_workers=min(len(ANALYSIS_DATA), physical_cores),
        reporting_interval=REPORTING_INTERVAL,
    )
    zonal_stats_task_list = []

    for vector_id, vector_path in REFERENCE_SUMMARY_VECTOR_PATHS.items():
        for (raster_id, year), raster_path in ANALYSIS_DATA.items():
            stats_task = task_graph.add_task(
                func=zonal_stats,
                args=(raster_path, vector_path),
                store_result=True,
                task_name=f"zonal stats for {raster_id} on {vector_id}",
            )
            zonal_stats_task_list.append((raster_id, stats_task))

        # process zonal results
        gdf = gpd.read_file(vector_path)

        # need to get the FID column in there so we can join results
        gdf = gdf[["geometry"]].copy()
        gdf["fid"] = gdf.index.astype("int32")
        for raster_id, stats_task in zonal_stats_task_list:
            stats_df = stats_task.get()
            rename_map = {op: f"{raster_id}_{op}" for op in ZONAL_OPS}
            stats_df.rename(columns=rename_map, inplace=True)
            gdf = gdf.merge(stats_task.get(), on="fid")
        timestamp = datetime.now().strftime("%Y_%m_%d_%H_%M_%S")
        out_vector_path = os.path.join(
            WORKSPACE_DIR, f"{vector_id}_synth_zonal_{timestamp}.gpkg"
        )
        gdf.to_file(out_vector_path, driver="GPKG")
        print(f"output written to {out_vector_path}")


if __name__ == "__main__":
    main()
