"""Summarize analysis raster data across a raster of zones."""
# Simplified version of summary_pipeline_landgrid.py
# This version uses a zone RASTER instead of a vector to avoid geometry issues.

from datetime import datetime
import argparse
import logging
import os
import sys
import time
import yaml
from pathlib import Path

from ecoshard import taskgraph
from exactextract import exact_extract
import pandas as pd

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stdout,
    format=(
        "%(asctime)s (%(relativeCreated)d) %(levelname)s %(name)s"
        " [%(funcName)s:%(lineno)d] %(message)s"
    ),
)
LOGGER = logging.getLogger(__name__)

def zonal_stats_raster(value_raster_path, zone_raster_path, op_stats):
    """Calculate zonal stats using a value raster and a zone raster."""
    LOGGER.info(f"Starting zonal stats for {Path(value_raster_path).name} using zones from {Path(zone_raster_path).name}")

    # exact_extract can take a raster as the 'vec' input.
    # The values of the zone raster are used as the feature IDs.
    stats_df = exact_extract(
        value_raster_path,
        zone_raster_path,
        ops=op_stats,
        output="pandas",
        include_cols=["value"] # 'value' is the default name for the zone raster pixel values
    )

    # Rename the 'value' column to 'fid' to match our project's ID system
    stats_df.rename(columns={"value": "fid"}, inplace=True)

    return stats_df

def main():
    parser = argparse.ArgumentParser(description="Raster-based zonal stats pipeline.")
    parser.add_argument("config_yaml_path", type=Path)
    parser.add_argument("--data-root", type=Path, default=os.getenv("GLOBAL_NCP_DATA", ""))
    args = parser.parse_args()

    with args.config_yaml_path.open("r") as f:
        pipeline_config = yaml.safe_load(f)

    workspace_dir = Path(pipeline_config["workspace_dir"]["path"])
    os.makedirs(workspace_dir, exist_ok=True)

    # The path to the single zone raster we will use for all calculations
    zone_raster_path = Path(os.path.expandvars(pipeline_config["zone_raster"]["path"])).resolve()
    if not zone_raster_path.exists():
        raise FileNotFoundError(f"Zone raster not found at: {zone_raster_path}")

    op_stats = pipeline_config["op_stats"]

    task_graph = taskgraph.TaskGraph(workspace_dir, n_workers=len(pipeline_config["raster_layers"]))

    results = []
    for raster_id, raster_info in pipeline_config["raster_layers"].items():
        value_raster_path = Path(os.path.expandvars(raster_info["path"])).resolve()

        stats_task = task_graph.add_task(
            func=zonal_stats_raster,
            args=(str(value_raster_path), str(zone_raster_path), op_stats),
            store_result=True,
            task_name=f"zonal_stats:{raster_id}"
        )
        results.append((raster_id, stats_task))

    task_graph.join()

    # Consolidate results into a single dataframe
    LOGGER.info("Consolidating results...")
    final_df = None
    for raster_id, stats_task in results:
        stats_df = stats_task.get()

        # Rename stat columns to be specific to the raster layer
        rename_map = {op: f"{raster_id}_{op}" for op in op_stats}
        stats_df.rename(columns=rename_map, inplace=True)

        if final_df is None:
            final_df = stats_df
        else:
            # Merge results based on the 'fid' from the zone raster
            final_df = pd.merge(final_df, stats_df, on="fid", how="outer")

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_csv_path = workspace_dir / f"raster_zonal_summary_{timestamp}.csv"
    final_df.to_csv(output_csv_path, index=False)

    task_graph.close()
    LOGGER.info(f"Processing complete. Output written to {output_csv_path}")

if __name__ == "__main__":
    main()
