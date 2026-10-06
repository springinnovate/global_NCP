"""Rerun Path A for nitrogen export only, reusing the original pipeline functions unchanged.

Steps (same as the original Path A, only paths differ):
  1. 2020 - 1992 difference  -> Python_scripts/batch_raster_diff.calculate_difference
  2. divide by pixel area    -> zonal_stats_toolkit/convert_to_ha.process_single_raster
  3. regional zonal stats    -> zonal_stats_toolkit/runner.py analysis_configs/path_a_n_export_rerun.ini

Run inside the project Docker image (2026-10-01):
  MSYS_NO_PATHCONV=1 docker run --rm -v "C:/projects:/projects" \
    -v "C:/Users/JerónimoRodríguezEsc/data/global_ncp:/gncp" \
    therealspring/global_ncp-computational-environment:latest bash -c \
    "micromamba run -n geopy311 pip install -q datasketches tqdm fiona && \
     micromamba run -n geopy311 python /projects/global_NCP/Python_scripts/rerun_n_export_path_a.py"
(the image's geopy311 env lacks datasketches/tqdm/fiona, which runner.py and the diff script import).
"""

import subprocess
import sys
import time
from pathlib import Path

GNCP = Path("/projects/global_NCP")
TOOLKIT = Path("/projects/zonal_stats_toolkit")
BASE = GNCP / "data/raw/base_years"
F1992 = BASE / "global_n_export_tnc_esa1992_compressed_md5_728edc-004.tif"
F2020 = BASE / "global_n_export_tnc_esa2020_compressed_md5_1d3c17-007.tif"
CHG_DIR = GNCP / "data/raw/2020_1992_chg"
HA_DIR = GNCP / "data/raw/2020_1992_ch_ha"
AREA = Path("/gncp/vector_basedata/esa_pixel_area_ha_md5_1dd3298a7c4d25c891a11e01868b5db6.tif")
INI = GNCP / "analysis_configs/path_a_n_export_rerun.ini"

sys.path[:0] = [str(GNCP / "Python_scripts"), str(TOOLKIT)]
import batch_raster_diff  # noqa: E402
import convert_to_ha      # noqa: E402


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def main():
    CHG_DIR.mkdir(parents=True, exist_ok=True)
    HA_DIR.mkdir(parents=True, exist_ok=True)
    diff = CHG_DIR / "n_export_diff_1992_2020.tif"

    if not diff.exists():
        log(f"step 1: difference {F2020.name} - {F1992.name}")
        batch_raster_diff.calculate_difference(F1992, F2020, diff, num_threads=1)  # 1: concurrent reads on one GDAL dataset fail
    else:
        log(f"step 1: exists, skipping ({diff})")

    ha = HA_DIR / diff.name
    if not ha.exists():
        log("step 2: per-hectare conversion")
        convert_to_ha.AREA_RASTER_PATH = AREA
        convert_to_ha.OUTPUT_DIR = HA_DIR
        convert_to_ha.process_single_raster(diff)
    else:
        log(f"step 2: exists, skipping ({ha})")
    if not ha.exists():
        sys.exit("per-ha raster was not written; stopping before zonal stats")

    log("step 3: regional zonal statistics")
    subprocess.run([sys.executable, str(TOOLKIT / "runner.py"), str(INI)], check=True, cwd=str(TOOLKIT))
    log("done")


if __name__ == "__main__":
    main()
