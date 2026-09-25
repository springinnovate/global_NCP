# SWY Philippines test — consolidated, reproducible pipeline

## Current state (2026-09-25) — read this first

This folder compares WWF-SIPA's Philippines SWY run against the same model driven by CN and Kc
built from global data. Everything below this section is the original 2026-09-11 setup log,
kept for history; most of its "status" column is long out of date.

**Reference run:** WWF-SIPA's CN + Kc through this pipeline on the 90m DEM (`09h`). It reproduces
WWF-SIPA's own 30m output (r=0.92 QF and B) on the same grid as our runs, so all current
comparisons are against it. Run and figure dictionary: `docs/reports/swy/FIGURES.md`. Status
report: `docs/reports/swy/swy_status_report.qmd`. Methods: `docs/swy/swy_methods.qmd`.

**Scripts in the current chain** (everything else here is superseded or a one-off check):

| Step | Script | What it does |
|---|---|---|
| inputs | `02e`, `03`/`03b`, `05b`, `06b`, `06c` | snapped 90m DEM; MOD13A3 NDVI/EVI; NASA POWER net radiation; MapSPAM crops; Sacks crop calendar |
| CN | `07b_build_biophysical_table_becky_inputs.py` | our CN (GCN250 via ESA crosswalk) in the biophysical table; its Kc columns are overridden by the rasters below |
| Kc forest | `07d_build_kc_forest_negronjuarez.py` | Negrón Juárez (2008) EVI + net-radiation Kc, forest pixels |
| Kc grass/shrub | `07e_build_kc_grassland_oliveira.py` | Oliveira (2015) EVI Kc |
| Kc combine | `07f_build_kc_combined_forest_grassland.py` | merges 07d + 07e |
| Kc other classes | `07g_build_kc_ndvi_perpixel.py` | Kamble (2013) on NDVI, per pixel, for crops/mangrove/marsh/barren (fixes the EVI-into-Kamble error) |
| Kc paddy (test) | `07h_build_kc_paddy_rice.py` | FAO-56 rice curve on the Sacks calendar, weighted by MapSPAM rice share, Annual Crop only |
| runs | `09h` (reference), `09l`, `09m`, `09f`, `09n` | see FIGURES.md for the CN/Kc of each |
| compare | `20_cn_kc_comparison.py` | every run vs `09h`, per class; figures + CSVs |
| map | `10c`, `10e`, `11` | interactive map: QF/B (09h vs 09l), land cover, shaded DEM |

Runs need Docker (`swy_borneo_run:rainfix4`, see each `09*` docstring for the command; start
Docker Desktop first). Everything else runs in the project `.venv`.

**Where it stands:** with our CN + per-pixel Kc, baseflow is 1.06x the reference overall, but the
gap is concentrated in Annual Crop (2.28x), mostly paddy rice. Swapping in WWF-SIPA's CN alone
brings Annual Crop to 1.25x, so cropland CN is the main open problem; no global CN source
describes bunded paddy fields. How to parameterize cropland globally is the open question with
WWF-SIPA.

---

## Original setup notes (2026-09-11)

Written 2026-09-11, mirroring `swy_borneo_run/` (see that folder's own README for the pipeline
that actually completed end-to-end — this one is the Philippines equivalent, in progress). Goal:
run this project's own from-scratch SWY pipeline for the Philippines, using Rich/Becky's own
shared model-run output as the AOI template, so the result is genuinely comparable to what they
already have — not a second, differently-scoped Philippines run.

**Status as of this writing: input downloads just launched, nothing run yet.** Shared here
(with comments, per the user's request) so Becky can see the actual approach and code before any
result exists — not a finished, validated deliverable.

## Why the AOI step looks different from Borneo's

Borneo's `01_build_aoi.py` dissolves HydroBASINS units independently. This project's Philippines
AOI instead comes from Rich's own shared output raster — see `01_build_aoi_from_rich_mask.py`'s
docstring for the full story, including a real false start (the first attempt used the shared
workspace's routing-domain files and got a polygon 46% too large, because that routing domain
includes ~118,000 km² of upstream-contributing Borneo terrain with zero actual retained output —
caught by checking, not assumed). The corrected approach polygonizes the actual valid-data
footprint of Rich's `QF.tif` instead. Full narrative: `docs/swy/research_notes.md`'s 2026-09-11
entries.

## Pipeline, in order

| # | Script | What it does | Needs | Status |
|---|---|---|---|---|
| 1 | `01_build_aoi_from_rich_mask.py` | Polygonize the valid-data footprint of Rich's shared PH output raster into this project's own AOI | Becky's shared workspace already on disk (`data/swy/philippines/rich_shared/`) | **Done** — `ph_aoi.gpkg`, 284,981 km² |
| 2 | `02_fetch_dem.py` | Download SRTMGL3 DEM (90m) via AppEEARS | NASA Earthdata `.netrc` (same as Borneo's — see `swy_borneo_run/README.md`) | Launched |
| 3 | `03_fetch_ndvi.py` | Download MOD13A3 NDVI + QA (2020) via AppEEARS | same `.netrc` | Launched |
| 4 | `04_fetch_precip_chirps.py` | Download + clip 2020 monthly CHIRPS precipitation | none (public) | Launched |
| 5 | `05_fetch_et0_terraclimate.py` | Download + clip 2020 monthly TerraClimate reference ET0 | none (public); reuses Borneo's already-downloaded global netCDF if present | Launched |
| 6 | `06_extract_lulc_2020.py` | Extract the Philippines from the real 2020 C3S land cover netCDF | the netCDF on the user's WWF OneDrive, same file Borneo used | Not yet run |
| 7 | `07_build_biophysical_table.py` | Build the CN_A-D + Kc_1-12 + root_depth table, per lucode | outputs of steps 3 and 6 | Not yet run |
| 8 | `08_build_rain_events_table.py` | Write a **placeholder** rain-events table (weaker justification than Borneo's — see script docstring, real Philippine monsoon/typhoon seasonality) | none | Not yet run |
| 9 | `09_run_swy_ph.py` | Call `inspring.seasonal_water_yield.execute()` with everything assembled above | run inside the same Docker image `swy_borneo_run/Dockerfile` builds | Not yet run |

Steps 1–8 run in the project's own `.venv`. Step 9 needs `inspring`, which runs inside Docker —
see `swy_borneo_run/README.md`'s "Running the model" section for the exact command (identical
mechanics, just point it at `09_run_swy_ph.py` instead).

## The year is not confirmed

Everything above assumes **2020**, matching this project's other anchor-year conventions and
Becky's shared workspace's folder name (`baseline_historical_climate`). She was asked directly
what year her shared run actually represents; no answer as of this writing. If it turns out to be
a different year, `03_fetch_ndvi.py`, `04_fetch_precip_chirps.py`, and `05_fetch_et0_terraclimate.py`
all need re-running with `YEAR` changed (and NDVI only goes back to 2000 — MODIS's launch — so a
pre-2000 year would need a different approach entirely).

## What happens after step 9

Once this project's own PH run completes, the actual comparison against Becky's shared workspace
happens in the **real** overlap zone — both cover the same Philippines islands, unlike the
Borneo/routing-domain false start above. QF is the safe layer to compare (no routing dependency,
see `docs/swy/swy_methods.qmd`'s output-variable table); `L_sum`/`B`/`B_sum` should be compared
with the same routing-anomaly caveat found in the Borneo run.

## Known, deliberate compromises (same spirit as Borneo's, one Philippines-specific weakening)

- **Rain events table is a placeholder**, and a rougher one than Borneo's — see
  `08_build_rain_events_table.py`.
- **Kc uses the NDVI regression for every vegetated class, including cropland.**
- **`root_depth` is a placeholder** (1000mm) — confirmed dead code in this `inspring` fork.
- **CSV biophysical-table path, not raster-overrides** — same pragmatic-for-now choice as Borneo.
- **AOI is derived from Rich's own output footprint**, not independently verified against an
  official Philippines political boundary — see the note above.
