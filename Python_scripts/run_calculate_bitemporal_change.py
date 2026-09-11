"""Entry point for the bitemporal-change GPKG step. Orchestration only — see
calculate_bitemporal_change_tasks.py for the actual work.
"""

import argparse

from calculate_bitemporal_change_tasks import calculate_change


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Create a new GeoPackage with bi-temporal changes, dropping original years.")
    parser.add_argument("--input-gpkg", default="/data/interim/10k_grid_services_base.gpkg",
                         help="Path to the input GPKG")
    parser.add_argument("--output-gpkg", default="/data/interim/10k_grid_services_change.gpkg",
                         help="Path to the new output GPKG")
    parser.add_argument("--base-year", default="1992", help="The baseline year (e.g., 1992)")
    parser.add_argument("--target-year", default="2020",
                         help="The target year to compare against (e.g., 2020)")
    parser.add_argument("--metric", default=None,
                         help="Optional metric to filter by (e.g., 'mean' to only process means)")
    args = parser.parse_args()

    calculate_change(args.input_gpkg, args.output_gpkg, args.base_year, args.target_year, args.metric)
