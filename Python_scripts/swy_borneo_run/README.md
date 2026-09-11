# SWY Borneo test — consolidated, reproducible pipeline

Written 2026-09-10/11, consolidating what had until then been ad-hoc interactive work (bash/
Python commands run directly in a Claude Code session, never saved) into real, re-runnable
scripts — both so this can be repeated without a session at hand, and so the whole process is
traceable and reviewable. See `docs/swy/research_notes.md`'s 2026-09-10/11 entries for the
narrative version of this same work (gotchas, dead ends, reasoning); this README and the scripts
are the executable version.

## Pipeline, in order

| # | Script | What it does | Needs |
|---|---|---|---|
| 1 | `01_build_aoi.py` | Dissolve HydroBASINS AU/Oceania level-6 units into the Borneo AOI polygon | HydroBASINS shapefiles already in `data/swy_shared_package/hydrobasins/` |
| 2 | `02_fetch_dem.py` | Download SRTMGL3 DEM (90m) via AppEEARS | NASA Earthdata `.netrc` (see below) |
| 3 | `03_fetch_ndvi.py` | Download MOD13A3 NDVI + QA (2020, or `--full` for 2000-present) via AppEEARS | same `.netrc` |
| 4 | `04_fetch_precip_chirps.py` | Download + clip 2020 monthly CHIRPS precipitation | none (public, unauthenticated) |
| 5 | `05_fetch_et0_terraclimate.py` | Download + clip 2020 monthly TerraClimate reference ET0 | none (public, unauthenticated) |
| 6 | `06_extract_lulc_2020.py` | Extract Borneo from the real 2020 C3S land cover netCDF | the netCDF on the user's WWF OneDrive — path is hardcoded near the top of the script, update if it moves |
| 7 | `07_build_biophysical_table.py` | Build the CN_A-D + Kc_1-12 + root_depth table InVEST needs, per lucode | outputs of steps 3 and 6 |
| 8 | `08_build_rain_events_table.py` | Write a **placeholder** rain-events table | none — see the script's own docstring for why it's a placeholder, not derived data |
| 9 | `09_run_swy_borneo.py` | Call `inspring.seasonal_water_yield.execute()` with everything assembled above | run inside the Docker image built from `Dockerfile` (see below) — inspring is not installed in the plain project `.venv` |

Steps 1-8 run fine in the project's own `.venv` (`geopandas`/`rasterio`/`requests` — the
Windows-native environment already used elsewhere in this repo). Step 9 needs `inspring`, which
does not install cleanly on Windows (no C++ toolchain in this environment) — it runs inside
Docker instead.

## NASA Earthdata `.netrc` (needed for steps 2 and 3)

```
machine urs.earthdata.nasa.gov
login <username>
password <password>

machine appeears.earthdatacloud.nasa.gov
login <username>
password <password>
```

Both blocks are required (different hostnames). Full one-time account walkthrough (registration,
authorizing the "LP DAAC Data Pool" app) is in `docs/HANDOFF.md`.

## Running the model (step 9)

```bash
docker build -t swy_borneo_run -f Python_scripts/swy_borneo_run/Dockerfile Python_scripts/swy_borneo_run
MSYS_NO_PATHCONV=1 docker run --rm -v "$(pwd)/data:/data" -v "$(pwd)/Python_scripts:/scripts" swy_borneo_run \
    micromamba run -n geopy311 python /scripts/swy_borneo_run/09_run_swy_borneo.py
```

`MSYS_NO_PATHCONV=1` matters on Windows Git Bash — without it, Git Bash silently rewrites the
`/scripts/...` and `/data/...` container paths into nonsense Windows paths before Docker ever
sees them.

The `Dockerfile` builds on this project's own existing image
(`therealspring/global_ncp-computational-environment`) rather than `inspring`'s own Dockerfile,
which is broken as of 2026-09-10 (see "Findings for Rich" below) — the project's own image
already had everything `inspring` actually needs (gcc/g++/cython/GDAL, a fresh unpinned
`ecoshard`).

## Known, deliberate compromises in this specific run

Documented here so nobody mistakes this for a finished, validated result — see
`docs/swy/swy_methods.qmd` for the full methodology this is a first test of:

- **Rain events table is a placeholder** (uniform 18 events/month), not derived from real daily
  CHIRPS data. The documented derivation method (`calculate_average_monthly_events.py`, "Earth
  Engine + CHIRPS daily") needs GEE access this environment doesn't have configured, and the
  script file itself wasn't found in this repo.
- **Kc uses the NDVI regression for every vegetated class, including cropland** — not switched to
  FAO-56 for cropland, since region-correct crop-calendar timing isn't built yet.
- **LULC is 2020** (the real Copernicus C3S product, sourced from the user's OneDrive after the
  local copy turned out to be an empty/corrupted stub — see step 6's docstring) but the AOI-level
  2020 vs. 1992 question that motivated this project's dual LULC epochs elsewhere doesn't apply
  here; this is the correct, matching year.
- **`root_depth` is a placeholder** (1000mm) — confirmed dead code in this `inspring` fork, so
  its actual value doesn't affect output.
- Uses the CSV **biophysical-table** path (`biophysical_table_path`), not `inspring`'s raster-
  override args (`cn_a/b/c/d_path`, `kc_1...12_path`) — simpler and already fully built for this
  attempt. The raster-override path remains architecturally preferable long-term (see
  `swy_methods.qmd`) and is worth attempting in a follow-up round.

## Findings for Rich (inspring / its Dockerfile)

Concrete, reproducible — not "it didn't work":

1. **`inspring`'s own `Dockerfile` fails immediately**: it runs `pip3 install -r requirements.txt`
   against a repo state where `requirements.txt` was deleted in 2022 and never replaced.
   Confirmed by actually running the build, not just reading the file.
2. **`setup.py`'s `packages` list omits `inspring.seasonal_water_yield`** (and possibly other
   subpackages worth checking) — a normal `pip install .` compiles the Cython extension but the
   Python module itself isn't importable afterward. Workaround used here: `python setup.py
   build_ext --inplace` + `PYTHONPATH` pointed at the source tree directly, bypassing the
   installed package entirely.
3. **Good news, not just bugs**: the *current* upstream `ecoshard` (`springinnovate/ecoshard`,
   not the old pinned `therealspring/ecoshard@b9b4580` fork commit) works fine — its bundled
   `ecoshard.geoprocessing` (with `routing`) and `ecoshard.taskgraph` submodules have the API
   `inspring` expects. This resolves an open question from earlier research (whether the old
   fork was still required) — it isn't.
4. Both (1) and (2) are small, well-scoped, non-scientific fixes — real candidates for a PR back
   to `springinnovate/inspring` once there's time to do it properly (fork, fix, PR) rather than
   just working around them locally as done here.
