"""Summarize analysis raster data across reference vector data."""

from dataclasses import dataclass
from datetime import datetime
from datetime import timedelta
from pathlib import Path
from typing import Callable
import logging
import os
import re
import sys
import time

from ecoshard import taskgraph
from exactextract import exact_extract
from exactextract.raster import GDALRasterSource
from geopandas import gpd
import pandas as pd
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

REFERENCE_SUMMARY_VECTOR_PATHS = {
    "hydrosheds_lv6_synth": "./data/reference/hydrosheds_lv6_synth.gpkg"
}

# Tag format is a tuple ([description], [YYYY]) key
# with a "path" -> str and "band" -> int dictionary value
ANALYSIS_DATA = {
    ("rast_gdpTot_1990_2020_30arcsec", 1990): {
        "path": "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif",
        "band": 1,
    },
    ("rast_gdpTot_1990_2020_30arcsec", 1995): {
        "path": "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif",
        "band": 2,
    },
    ("rast_gdpTot_1990_2020_30arcsec", 2000): {
        "path": "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif",
        "band": 3,
    },
    ("rast_gdpTot_1990_2020_30arcsec", 2005): {
        "path": "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif",
        "band": 4,
    },
    ("rast_gdpTot_1990_2020_30arcsec", 2010): {
        "path": "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif",
        "band": 5,
    },
    ("rast_gdpTot_1990_2020_30arcsec", 2015): {
        "path": "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif",
        "band": 6,
    },
    ("rast_gdpTot_1990_2020_30arcsec", 2020): {
        "path": "./data/analysis/Gridded_GDP/rast_gdpTot_1990_2020_30arcsec.tif",
        "band": 7,
    },
}

"""
The format for the zonal ops is as below:
<stat>                         # plain
<stat>@<alias>                 # same as plain, but give it a name
<stat>/<vector_alias>.<field>  # divide by a vector field
<stat>*<vector_alias>.<field>  # multiply by a vector field
<expr>@<alias>                 # any Python expr using ${stat} + row
"""
ZONAL_OPS = ["mean", "max", "min"]


WORKSPACE_DIR = "./summary_pipeline_workspace"
os.makedirs(WORKSPACE_DIR, exist_ok=True)
REPORTING_INTERVAL = 10.0


# Define the regular expressions to extract out the metrics and the ops
METRIC_RE = re.compile(
    r"""
    (?P<body>[^@]+?)           # stat or expr
    (?:@(?P<alias>\w+))?       # optional alias
    $""",
    re.VERBOSE,
)

OP_RE = re.compile(
    r"^(?P<stat>\w+)(?:(?P<op>[*/])(?P<v_alias>\w+)\.(?P<v_field>\w+))?$"
)


@dataclass
class ZonalMetric:
    name: str
    func: Callable[[pd.Series], float]  # row → value


def build_metrics(metric_specs):
    metrics = []

    for spec in metric_specs:
        m = METRIC_RE.match(spec)
        if not m:
            raise ValueError(f"bad metric spec: {spec}")
        body, alias = m.group("body", "alias")

        # plain op or op with */field
        mo = OP_RE.match(body)
        if mo:
            stat = mo.group("stat")
            op, v_alias, v_field = mo.group("op", "v_alias", "v_field")
            name = (
                alias or f'{v_alias or "hydro6"}_{stat}'
                if not op
                else (
                    alias or f'{stat}_{v_field}_{ "div" if op=="/" else "mul"}'
                )
            )

            def _make(stat, op, v_alias, v_field):
                return lambda row: (
                    (
                        row[stat] / row[f"{v_alias}.{v_field}"]
                        if op == "/"
                        else row[stat] * row[f"{v_alias}.{v_field}"]
                    )
                    if op
                    else row[stat]
                )

            metrics.append(
                ZonalMetric(name, _make(stat, op, v_alias or "hydro6", v_field))
            )
        else:
            # arbitrary python expression (use ${stat} placeholders)
            expr = body.replace("${", 'row["').replace("}", '"]')
            name = alias or "expr_" + str(abs(hash(expr)))[:6]
            metrics.append(ZonalMetric(name, lambda row, e=expr: eval(e)))
    return metrics


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


def zonal_stats(raster_path_band_dict, zonal_ops, vector_path):
    """Calculate zonal statistics for vector file over a given raster.

    This function reads polygon geometries from a vector file, assigns a
    unique identifier (FID) to each geometry, and computes zonal statistics
    such as mean, sum, count, min, and max of the raster values intersecting
    each polygon. Statistics calculation uses exact extraction methods,
    ensuring accuracy particularly at polygon boundaries.

    Args:
        raster_path_band_dict (dict): dictionary containing 'path' and 'band'
            for the raster to process.
        vector_path (str): Path to the vector file containing polygon
            geometries.

    Returns:
        pandas.DataFrame: DataFrame containing zonal statistics with one row
        per polygon, identified by 'fid' and columns for each calculated
        statistic named from `ZONAL_OPS`.
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
        ops=zonal_ops,
        include_cols=["fid"],
        output="pandas",
        strategy="raster-sequential",
        # i found a *2 and *4 to make a nearly twofold improvment, but didn't
        # see gains at *8
        max_cells_in_memory=30000000 * 4,
        progress=create_progress_logger(REPORTING_INTERVAL, stem),
    )
    return stats_df


def main():
    """Entry point."""
    start_time = time.time()
    physical_cores = psutil.cpu_count(logical=False)
    task_graph = taskgraph.TaskGraph(
        WORKSPACE_DIR,
        n_workers=min(len(ANALYSIS_DATA), physical_cores),
        reporting_interval=REPORTING_INTERVAL,
    )
    zonal_stats_task_list = []

    for vector_id, vector_path in REFERENCE_SUMMARY_VECTOR_PATHS.items():
        for (raster_id, year), raster_path_band_dict in ANALYSIS_DATA.items():
            stats_task = task_graph.add_task(
                func=zonal_stats,
                args=(raster_path_band_dict, ZONAL_OPS, vector_path),
                store_result=True,
                task_name=(
                    f"zonal stats for {raster_id}:"
                    f"{raster_path_band_dict['band']} on {vector_id}"
                ),
            )
            zonal_stats_task_list.append(
                (raster_id, raster_path_band_dict["band"], stats_task)
            )

        # process zonal results
        gdf = gpd.read_file(vector_path)

        # need to get the FID column in there so we can join results
        gdf = gdf[["geometry"]].copy()
        gdf["fid"] = gdf.index.astype("int32")
        for raster_id, band_idx, stats_task in zonal_stats_task_list:
            stats_df = stats_task.get()
            rename_map = {
                op: f"{raster_id}:{band_idx}_{op}" for op in ZONAL_OPS
            }
            stats_df.rename(columns=rename_map, inplace=True)
            gdf = gdf.merge(stats_task.get(), on="fid")
        timestamp = datetime.now().strftime("%Y_%m_%d_%H_%M_%S")
        out_vector_path = os.path.join(
            WORKSPACE_DIR, f"{vector_id}_synth_zonal_{timestamp}.gpkg"
        )
        gdf.to_file(out_vector_path, driver="GPKG")
        print(
            f"done in {time.time()-start_time:.2f}s, output written to {out_vector_path}"
        )


if __name__ == "__main__":
    main()
