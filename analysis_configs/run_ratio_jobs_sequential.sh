#!/bin/bash
# Run the country / region_wb / income_grp jobs of path_a_n_ret_ratio_rerun.ini one at a time
# (2026-10-01). Running all four jobs x three rasters in parallel left the three jobs on the
# cartographic vector with empty results; biome (already written) is skipped.
# Run inside the project Docker image (see Python_scripts/rerun_n_ret_ratio_path_a.py).
set -e
INI=/projects/global_NCP/analysis_configs/path_a_n_ret_ratio_rerun.ini
for job in country region_wb income_grp; do
  mkdir -p /tmp/${job}; tmp=/tmp/${job}/path_a_n_ret_ratio_rerun.ini  # runner requires [project].name == file stem
  awk -v keep="job:${job}" '
    /^\[/ { section = substr($0, 2, length($0) - 2) }
    section == "project" || section == keep { print }
  ' "$INI" | sed "s#global_work_dir = .*#global_work_dir = /projects/global_NCP/data/raw/n_ret_ratio_aligned/workdir_${job}#" > "$tmp"
  echo "[$(date +%H:%M:%S)] job ${job}"
  (cd /projects/zonal_stats_toolkit && python runner.py "$tmp")
done
echo "[$(date +%H:%M:%S)] all done"
