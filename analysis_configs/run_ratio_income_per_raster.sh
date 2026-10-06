#!/bin/bash
# income_grp job of path_a_n_ret_ratio_rerun.ini, one raster at a time (2026-10-02). With all three
# ratio rasters in one job, TaskGraph aborted on "memory usage exceeds 95%" and returned empty
# results. Each run writes its own income_grp_<timestamp>.csv; merge them on income_grp afterwards.
set -e
INI=/projects/global_NCP/analysis_configs/path_a_n_ret_ratio_rerun.ini
for r in ${RATIO_RASTERS:-n_retention_ratio_1992 n_retention_ratio_2020 n_retention_ratio_diff_1992_2020}; do
  mkdir -p /tmp/$r; tmp=/tmp/$r/path_a_n_ret_ratio_rerun.ini  # runner requires [project].name == file stem
  awk '/^\[/ { section = substr($0, 2, length($0) - 2) } section == "project" || section == "job:income_grp" { print }' "$INI" \
    | sed -e "s#global_work_dir = .*#global_work_dir = /projects/global_NCP/data/raw/n_ret_ratio_aligned/workdir_income_${r}#" \
          -e "s#base_raster_pattern = .*#base_raster_pattern = /projects/global_NCP/data/raw/n_ret_ratio_aligned/${r}.tif#" > "$tmp"
  echo "[$(date +%H:%M:%S)] income_grp ${r}"
  (cd /projects/zonal_stats_toolkit && python runner.py "$tmp")
  sleep 2
done
echo "[$(date +%H:%M:%S)] all done"
