---
title: "Data Catalog — NatCap/TNC Modeling Outputs Inventory"
status: "Written 2026-08-28, from two OneDrive spreadsheets found while chasing a missing raw-raster blocker (sediment retention SPC). Verify before relying on for anything beyond documentation — several links inside are already dead (see below)."
---

This documents two spreadsheets that were sitting in a personal OneDrive folder
(`global_NCP\data_NCP\Spring\`), not previously referenced anywhere in this repo, and easy to lose
track of — this doc exists so that doesn't happen again. Written while trying to recover the raw
1992/2020 USLE and sediment-export rasters (see `analysis_configs/services_slim.yaml`); that
specific search failed (see "Known dead links" below), but the spreadsheets themselves are a real,
useful map of what NatCap/TNC modeling output exists and where — worth keeping.

## Source files

- `Links to NatCap modeling outputs for WWF projects.xlsx` — 5 sheets: `Full inventory - global
  outputs` (986 rows), `Inputs`, `FWC`, `NBS-OP`, `SIPA`.
- `inventory_global_products.xlsx` — 4 sheets: `inventory_global_products` (219 rows, includes
  actual 1992-baseline rows the first file is missing), `Initial Exploration`, `Scratch` (units),
  `Band LC_ESA` (land cover NetCDF band-to-year mapping).

Both live in the same OneDrive folder; treat `inventory_global_products.xlsx` as the more complete
one for the "Full inventory" content — it has 1992 Historic-scenario rows the first file lacks.

## Confirmed buckets (Google Cloud Storage, public `storage.googleapis.com` URLs)

| Bucket | What it holds |
|---|---|
| `ecoshard-root` | The main NatCap/TNC data-versioning bucket (hash-suffixed "ecoshard" filenames). Holds ESA/C3S land cover base layers, CI Restoration project outputs, population/beneficiary layers, WWF NBS-OP and SIPA (Indonesia) program outputs, and Justin Johnson's country correspondence table (see below). |
| `sci-ncscobenefits-spring` | The **TNC NBS** project — this is the source for this repo's own 8-service pipeline (nitrogen/sediment/coastal/nature access/pollination). Filenames here match `services_slim.yaml`'s expected raw filenames exactly (see verification below). |
| `gtap_invest_seals_2023_04_21` | Justin Johnson's (NatCap TEEMs) cartographic/correspondence outputs. |
| `critical-natural-capital-ecoshards` | Downstream-beneficiary layers (Stanford/Morgan Stanley project), different from this repo's own serviceshed routing. |

## Verified against this repo's own config — real, not coincidental

Cross-checked several `sci-ncscobenefits-spring` filenames directly against
`analysis_configs/services_slim.yaml`'s expected raw raster filenames — **exact match, including
the MD5 hash suffix**, confirming this bucket is genuinely the source this pipeline was built from:

| Variable | Filename (identical in both places) |
|---|---|
| N export, 1992 | `global_n_export_tnc_esa1992_compressed_md5_728edc.tif` (also physically confirmed present in this project's local data folder) |
| Sediment export, 1992 | `global_sed_export_marine_mod_ESA_1992_compressed_md5_18eaae.tif` |
| Sediment export, 2020 | `global_sed_export_marine_mod_ESA_2020_compressed_md5_a988c0.tif` |

**USLE (potential erosion) 1992/2020 do not appear in either spreadsheet under any service-name
search** (checked "USLE", "erosion", the known MD5 hashes `7e68e0`/`99e715` directly) — either
never catalogued here, or filed under a name/path not yet found.

## Known dead links (checked 2026-08-28, do not assume these work)

The three `sci-ncscobenefits-spring` URLs above (N export 1992, sediment export 1992 and 2020) all
returned **HTTP 404** when actually requested, despite being listed in the spreadsheet. The files
have apparently been deleted from the bucket since the spreadsheet was last updated. **Do not
assume any link in either spreadsheet is live without checking it first** (`curl -sI <url>`).

## Where the original processing actually ran

The `Band LC_ESA` sheet's source-file comment reveals the original land cover band-extraction ran
on a Linux server, home directory path `/home/jeronimo/global_ES_modeling/esos-c/data/ndr/ESA_LC/
ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif` — almost certainly the "Lilling" server the
user has mentioned running the original InVEST models on. **This is the most promising lead for
recovering the missing raw USLE/sediment-export 1992/2020 rasters** — worth checking that server's
filesystem directly (not just the public GCS buckets, which may only ever have held a subset of
outputs) before concluding the data is unrecoverable.

## Units (from the `Scratch` sheet — worth preserving, wasn't documented elsewhere found)

| Service | Unit |
|---|---|
| Coastal Protection | Risk Reduction Index |
| Nitrogen Export | kg/ha/year |
| Pollination | Equivalent people fed/ha |
| Sediment Export | ton/ha/year |

## `ee_correspondence` provenance lead (paper's open Justin Johnson citation question)

`Inputs` sheet, row "Canonical countries (Justin)": `https://storage.googleapis.com/gtap_invest_seals_2023_04_21/cartographic/ee/ee_r264_correspondence.gpkg`.
Name is a close match to the `ee_correspondence` table used in `paper_draft_5service.qmd`'s
Methods section — **not confirmed to be the same or current version**, user flagged there may be a
newer one. See the callout in that qmd file's Ancillary Datasets subsection; don't cite this
without Justin confirming directly.

## Broader opportunity, not yet scoped (user's idea, 2026-08-28)

The `Full inventory` sheet catalogs far more than what this project currently uses — CI Restoration
scenario layers (forest regeneration, agroforestry, multiple restoration extents) for the same 5
core services, at multiple future time points (2035, 2050), plus entirely separate program
inventories (`NBS-OP`: Zambia/Yucatan/Madagascar landscape+population layers; `SIPA`: 722 rows of
Indonesia-specific service-overlap/scenario rasters). None of this is wired into this project's
pipeline. Worth a deliberate scoping conversation later about whether any of it extends this
paper's analysis (e.g., additional time points, restoration scenarios) or is better suited to a
separate project — not something to start on unprompted.
