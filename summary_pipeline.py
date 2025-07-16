"""Summarize analysis raster data across reference vector data."""

from datetime import datetime
from datetime import timedelta
from pathlib import Path
import argparse
import logging
import os
import sys
import time
import textwrap
import yaml

from ecoshard import taskgraph
from exactextract import exact_extract
from exactextract.raster import GDALRasterSource
from geopandas import gpd
import psutil
import rasterio

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
logging.getLogger("rasterio").setLevel(logging.INFO)

YAML_EXAMPLE = textwrap.dedent(
    """\
    Example YAML format:

    workspace:
        path: "summary_pipeline_workspace"

    vector_zones:
      hydrosheds_lv6_synth:
        path: "./data/reference/hydrosheds_lv6_synth.gpkg"

    raster_layers:
      rast_gdpTot_1990_2020_30arcsec_1990:
        path: "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif"
        band: 1

      rast_gdpTot_1990_2020_30arcsec_1995:
        path: "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif"
        band: 2

      rast_gdpTot_1990_2020_30arcsec_2000:
        path: "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif"
        band: 3

    ops_stats:
      - mean
      - max
      - min
    """
)


REPORTING_INTERVAL = 10.0


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
            elapsed_time = time.time() - start_time
            estimated_time_remaining = elapsed_time / fraction - elapsed_time
            elapsed_str = str(timedelta(seconds=int(elapsed_time)))
            etc_str = str(timedelta(seconds=int(estimated_time_remaining)))
            LOGGER.info(
                f"{task_id} is {100*fraction:.2f}% complete running for "
                f"{elapsed_str}, estimate {etc_str} remaining"
            )
            last_time = time.time()

    return _process_logger


def zonal_stats(raster_path_band_dict, op_stats, vector_path):
    """Calculate zonal statistics for vector file over a given raster.

    This function reads polygon geometries from a vector file, assigns a
    unique identifier (FID) to each geometry, and computes zonal statistics
    such as mean, sum, count, min, and max of the raster values intersecting
    each polygon. Statistics calculation uses exact extraction methods,
    ensuring accuracy particularly at polygon boundaries.

    Args:
        raster_path_band_dict (dict): dictionary containing 'path' and 'band'
            for the raster to process.
        op_stats (list): list of zonal ops that can be passed to
            exact_extract defined here:
            https://isciences.github.io/exactextract/operations.html
        vector_path (str): Path to the vector file containing polygon
            geometries.

    Returns:
        pandas.DataFrame: DataFrame containing zonal statistics with one row
        per polygon, identified by 'fid' and columns for each calculated
        statistic named from `op_stats`.
    """
    gdf = gpd.read_file(vector_path)

    # reproject if necessary
    with rasterio.open(raster_path_band_dict["path"]) as src:
        raster_crs = src.crs
    if gdf.crs != raster_crs:
        gdf = gdf.to_crs(raster_crs)

    # need to get the FID column in there so we can join results
    gdf = gdf[["geometry"]].copy()
    gdf["fid"] = gdf.index.astype("int32")

    stem = Path(raster_path_band_dict["path"]).stem
    stats_df = exact_extract(
        rast=GDALRasterSource(
            raster_path_band_dict["path"],
            band_idx=raster_path_band_dict["band"],
        ),
        vec=gdf,
        ops=op_stats,
        include_cols=["fid"],
        output="pandas",
        strategy="raster-sequential",
        # i found a *2 and *4 to make a nearly twofold improvment, but didn't
        # see gains at *8
        max_cells_in_memory=30000000 * 4,
        progress=create_progress_logger(REPORTING_INTERVAL, stem),
    )
    return stats_df


def load_config(config_path):
    """Load a YAML configuration and resolve relative data paths.

    The function parses the YAML file, then rewrites each
    ``vector_zones.*.path`` and ``raster_layers.*.path`` entry so that
    any relative path (starting with ``.`` or lacking a drive/root) is
    interpreted relative to the directory containing the YAML file.
    The resulting paths are returned as absolute, expanded
    ``pathlib.Path`` objects.

    Args:
        config_path (Path): Path to the YAML configuration file.

    Returns:
        dict: Parsed configuration dictionary with all data-path values
        converted to absolute ``Path`` objects.
    """
    with config_path.open("r") as f:
        cfg = yaml.safe_load(f)

    base = config_path.parent

    for v in cfg["vector_zones"].values():
        v["path"] = (base / v["path"]).expanduser().resolve()

    for r in cfg["raster_layers"].values():
        r["path"] = (base / r["path"]).expanduser().resolve()

    return cfg


def main():
    """Entry point."""
    parser = argparse.ArgumentParser(description="Zonal stats pipeline.")
    parser.add_argument(
        "config_yaml_path",
        type=Path,
        help="Path to YAML configuration example:\n\n" + YAML_EXAMPLE,
    )
    args = parser.parse_args()
    pipeline_config = load_config(args.config_yaml_path)

    required_fields = {
        "workspace_dir",
        "vector_zones",
        "raster_layers",
        "op_stats",
    }
    missing = required_fields - pipeline_config.keys()
    if missing:
        raise ValueError(
            f'Missing fields {", ".join(sorted(missing))} in {args.config_yaml_path}'
        )

    workspace_dir = pipeline_config["workspace_dir"]["path"]
    os.makedirs(workspace_dir, exist_ok=True)

    op_stats = pipeline_config["op_stats"]

    start_time = time.time()
    raster_layers = pipeline_config["raster_layers"]
    physical_cores = psutil.cpu_count(logical=False)
    task_graph = taskgraph.TaskGraph(
        workspace_dir,
        n_workers=min(len(raster_layers), physical_cores),
        reporting_interval=REPORTING_INTERVAL,
    )
    zonal_stats_task_list = []

    vector_zones = pipeline_config["vector_zones"]
    output_vector_list = []
    for vector_id, vector_config in vector_zones.items():
        vector_path = vector_config["path"]
        for raster_id, raster_path_band_dict in raster_layers.items():
            stats_task = task_graph.add_task(
                func=zonal_stats,
                args=(raster_path_band_dict, op_stats, vector_path),
                store_result=True,
                task_name=(
                    f"zonal stats for {raster_id}:"
                    f"{raster_path_band_dict['band']} on {vector_id}"
                ),
            )
            zonal_stats_task_list.append(
                (raster_id, raster_path_band_dict["band"], stats_task)
            )

        # copy original vector and join to zonal stats via 'fid'
        gdf = gpd.read_file(vector_path)
        gdf = gdf[["geometry"]].copy()
        gdf["fid"] = gdf.index.astype("int32")
        for raster_id, band_idx, stats_task in zonal_stats_task_list:
            stats_df = stats_task.get()
            # renames the stat to be the raster id provided in the
            # config file with a _operation at the end so we can
            # differentiate the operation applied to that raster
            rename_map = {op: f"{raster_id}_{op}" for op in op_stats}
            stats_df.rename(columns=rename_map, inplace=True)
            gdf = gdf.merge(stats_task.get(), on="fid")
        timestamp = datetime.now().strftime("%Y_%m_%d_%H_%M_%S")
        out_vector_path = os.path.join(
            workspace_dir, f"{vector_id}_synth_zonal_{timestamp}.gpkg"
        )
        output_vector_list.append(out_vector_path)
        gdf.to_file(out_vector_path, driver="GPKG")
    task_graph.join()
    task_graph.close()
    print(
        f"done in {time.time()-start_time:.2f}s, output(s) written to "
        + ",".join(output_vector_list)
    )


if __name__ == "__main__":
    main()
"""Summarize analysis raster data across reference vector data."""

from datetime import datetime
from datetime import timedelta
from pathlib import Path
import argparse
import logging
import os
import sys
import time
import textwrap
import yaml

from ecoshard import taskgraph
from exactextract import exact_extract
from exactextract.raster import GDALRasterSource
from geopandas import gpd
import psutil
import rasterio

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
logging.getLogger("rasterio").setLevel(logging.INFO)

YAML_EXAMPLE = textwrap.dedent(
    """\
    Example YAML format:

    workspace:
        path: "summary_pipeline_workspace"

    vector_zones:
      hydrosheds_lv6_synth:
        path: "./data/reference/hydrosheds_lv6_synth.gpkg"

    raster_layers:
      rast_gdpTot_1990_2020_30arcsec_1990:
        path: "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif"
        band: 1

      rast_gdpTot_1990_2020_30arcsec_1995:
        path: "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif"
        band: 2

      rast_gdpTot_1990_2020_30arcsec_2000:
        path: "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif"
        band: 3

    ops_stats:
      - mean
      - max
      - min
    """
)


REPORTING_INTERVAL = 10.0


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
            elapsed_time = time.time() - start_time
            estimated_time_remaining = elapsed_time / fraction - elapsed_time
            elapsed_str = str(timedelta(seconds=int(elapsed_time)))
            etc_str = str(timedelta(seconds=int(estimated_time_remaining)))
            LOGGER.info(
                f"{task_id} is {100*fraction:.2f}% complete running for "
                f"{elapsed_str}, estimate {etc_str} remaining"
            )
            last_time = time.time()

    return _process_logger


def zonal_stats(raster_path_band_dict, op_stats, vector_path):
    """Calculate zonal statistics for vector file over a given raster.

    This function reads polygon geometries from a vector file, assigns a
    unique identifier (FID) to each geometry, and computes zonal statistics
    such as mean, sum, count, min, and max of the raster values intersecting
    each polygon. Statistics calculation uses exact extraction methods,
    ensuring accuracy particularly at polygon boundaries.

    Args:
        raster_path_band_dict (dict): dictionary containing 'path' and 'band'
            for the raster to process.
        op_stats (list): list of zonal ops that can be passed to
            exact_extract defined here:
            https://isciences.github.io/exactextract/operations.html
        vector_path (str): Path to the vector file containing polygon
            geometries.

    Returns:
        pandas.DataFrame: DataFrame containing zonal statistics with one row
        per polygon, identified by 'fid' and columns for each calculated
        statistic named from `op_stats`.
    """
    gdf = gpd.read_file(vector_path)

    # reproject if necessary
    with rasterio.open(raster_path_band_dict["path"]) as src:
        raster_crs = src.crs
    if gdf.crs != raster_crs:
        gdf = gdf.to_crs(raster_crs)

    # need to get the FID column in there so we can join results
    gdf = gdf[["geometry"]].copy()
    gdf["fid"] = gdf.index.astype("int32")

    stem = Path(raster_path_band_dict["path"]).stem
    stats_df = exact_extract(
        rast=GDALRasterSource(
            raster_path_band_dict["path"],
            band_idx=raster_path_band_dict["band"],
        ),
        vec=gdf,
        ops=op_stats,
        include_cols=["fid"],
        output="pandas",
        strategy="raster-sequential",
        # i found a *2 and *4 to make a nearly twofold improvment, but didn't
        # see gains at *8
        max_cells_in_memory=30000000 * 4,
        progress=create_progress_logger(REPORTING_INTERVAL, stem),
    )
    return stats_df


def load_config(config_path):
    """Load a YAML configuration and resolve relative data paths.

    The function parses the YAML file, then rewrites each
    ``vector_zones.*.path`` and ``raster_layers.*.path`` entry so that
    any relative path (starting with ``.`` or lacking a drive/root) is
    interpreted relative to the directory containing the YAML file.
    The resulting paths are returned as absolute, expanded
    ``pathlib.Path`` objects.

    Args:
        config_path (Path): Path to the YAML configuration file.

    Returns:
        dict: Parsed configuration dictionary with all data-path values
        converted to absolute ``Path`` objects.
    """
    with config_path.open("r") as f:
        cfg = yaml.safe_load(f)

    base = config_path.parent

    for v in cfg["vector_zones"].values():
        v["path"] = (base / v["path"]).expanduser().resolve()

    for r in cfg["raster_layers"].values():
        r["path"] = (base / r["path"]).expanduser().resolve()

    return cfg


def main():
    """Entry point."""
    parser = argparse.ArgumentParser(description="Zonal stats pipeline.")
    parser.add_argument(
        "config_yaml_path",
        type=Path,
        help="Path to YAML configuration example:\n\n" + YAML_EXAMPLE,
    )
    args = parser.parse_args()
    pipeline_config = load_config(args.config_yaml_path)

    required_fields = {
        "workspace_dir",
        "vector_zones",
        "raster_layers",
        "op_stats",
    }
    missing = required_fields - pipeline_config.keys()
    if missing:
        raise ValueError(
            f'Missing fields {", ".join(sorted(missing))} in {args.config_yaml_path}'
        )

    workspace_dir = pipeline_config["workspace_dir"]["path"]
    os.makedirs(workspace_dir, exist_ok=True)

    op_stats = pipeline_config["op_stats"]

    start_time = time.time()
    raster_layers = pipeline_config["raster_layers"]
    physical_cores = psutil.cpu_count(logical=False)
    task_graph = taskgraph.TaskGraph(
        workspace_dir,
        n_workers=min(len(raster_layers), physical_cores),
        reporting_interval=REPORTING_INTERVAL,
    )
    zonal_stats_task_list = []

    vector_zones = pipeline_config["vector_zones"]
    output_vector_list = []
    for vector_id, vector_config in vector_zones.items():
        vector_path = vector_config["path"]
        for raster_id, raster_path_band_dict in raster_layers.items():
            stats_task = task_graph.add_task(
                func=zonal_stats,
                args=(raster_path_band_dict, op_stats, vector_path),
                store_result=True,
                task_name=(
                    f"zonal stats for {raster_id}:"
                    f"{raster_path_band_dict['band']} on {vector_id}"
                ),
            )
            zonal_stats_task_list.append(
                (raster_id, raster_path_band_dict["band"], stats_task)
            )

        # copy original vector and join to zonal stats via 'fid'
        gdf = gpd.read_file(vector_path)
        gdf = gdf[["geometry"]].copy()
        gdf["fid"] = gdf.index.astype("int32")
        for raster_id, band_idx, stats_task in zonal_stats_task_list:
            stats_df = stats_task.get()
            # renames the stat to be the raster id provided in the
            # config file with a _operation at the end so we can
            # differentiate the operation applied to that raster
            rename_map = {op: f"{raster_id}_{op}" for op in op_stats}
            stats_df.rename(columns=rename_map, inplace=True)
            gdf = gdf.merge(stats_task.get(), on="fid")
        timestamp = datetime.now().strftime("%Y_%m_%d_%H_%M_%S")
        out_vector_path = os.path.join(
            workspace_dir, f"{vector_id}_synth_zonal_{timestamp}.gpkg"
        )
        output_vector_list.append(out_vector_path)
        gdf.to_file(out_vector_path, driver="GPKG")
    task_graph.join()
    task_graph.close()
    print(
        f"done in {time.time()-start_time:.2f}s, output(s) written to "
        + ",".join(output_vector_list)
    )


if __name__ == "__main__":
    main()