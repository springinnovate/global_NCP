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
import pandas as pd
import numpy as np
import rasterio

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
    LOGGER.info(f"Starting block-by-block zonal stats for {Path(value_raster_path).name}")

    fid_stats = {}

    with rasterio.open(zone_raster_path) as z_src, rasterio.open(value_raster_path) as v_src:
        if z_src.shape != v_src.shape:
            LOGGER.warning(f"Shape mismatch: Zones {z_src.shape} vs Values {v_src.shape}")

        z_nodata = z_src.nodata
        v_nodata = v_src.nodata

        # Iterate through raster blocks to avoid loading massive global grids entirely into RAM
        for _, z_window in z_src.block_windows(1):
            zones = z_src.read(1, window=z_window)

            # Dynamically compute the corresponding window in the value raster
            # by getting the spatial bounding box of the current zone block.
            # This perfectly aligns datasets with different extents/crops.
            bounds = z_src.window_bounds(z_window)
            v_window = v_src.window(*bounds)

            # boundless=True safely pads areas outside the value raster's extent with nodata.
            # out_shape forces the returned array to exactly match the zone block's shape, preventing broadcast errors.
            vals = v_src.read(1, window=v_window, boundless=True, out_shape=zones.shape)

            mask = np.ones(zones.shape, dtype=bool)
            
            if z_nodata is not None:
                if np.isnan(z_nodata): mask &= ~np.isnan(zones)
                else: mask &= (zones != z_nodata)
                
            if np.issubdtype(vals.dtype, np.floating):
                mask &= ~np.isnan(vals)
                
            if v_nodata is not None and not np.isnan(v_nodata):
                mask &= (vals != v_nodata)

            valid_zones = zones[mask].astype(int)
            valid_vals = vals[mask]

            if valid_zones.size == 0:
                continue

            # Grouping via pandas is fast and clean
            df = pd.DataFrame({'fid': valid_zones, 'value': valid_vals})
            chunk_agg = df.groupby('fid')['value'].agg(['sum', 'count', 'max', 'min'])

            for fid, row in chunk_agg.iterrows():
                if fid not in fid_stats:
                    fid_stats[fid] = {'sum': 0.0, 'count': 0, 'max': float('-inf'), 'min': float('inf')}
                
                stat = fid_stats[fid]
                stat['sum'] += row['sum']
                stat['count'] += row['count']
                stat['max'] = max(stat['max'], row['max'])
                stat['min'] = min(stat['min'], row['min'])

    results = []
    for fid, stat in fid_stats.items():
        row = {'fid': fid}
        if 'mean' in op_stats:
            row['mean'] = stat['sum'] / stat['count'] if stat['count'] > 0 else np.nan
        if 'sum' in op_stats:
            row['sum'] = stat['sum']
        if 'max' in op_stats:
            row['max'] = stat['max']
        if 'min' in op_stats:
            row['min'] = stat['min']
        results.append(row)

    return pd.DataFrame(results)

def main():
    parser = argparse.ArgumentParser(description="Raster-based zonal stats pipeline.")
    parser.add_argument("config_yaml_path", type=Path)
    parser.add_argument("--data-root", type=Path, default=os.getenv("GLOBAL_NCP_DATA", ""))
    args = parser.parse_args()

    if str(args.data_root):
        os.environ["GLOBAL_NCP_DATA"] = str(args.data_root)

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
