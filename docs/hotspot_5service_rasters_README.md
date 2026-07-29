---
title: "5-Service Hotspot Rasters — What They Are, For Rich's Beneficiaries Pipeline"
date: "2026-07-28"
status: "Global scope only, delivered for beneficiaries rerun handoff"
---

## What this covers

Per Becky's 2026-07-21 meeting + Slack instructions: a redesigned hotspot definition using **5
services** (Nitrogen Export, Sediment Export, Coastal Risk, Pollination, Nature Access), dropping
the 3 retention/ratio services (N Retention Ratio, Sediment Retention Ratio, Coastal Risk
Reduction Ratio — export and retention of the same pollutant aren't independent, and retention
increases can reflect upstream degradation rather than local improvement).

Same 10km equal-area grid, same top-5%-per-service threshold methodology as the existing
8-service pipeline (`analysis/hotspot_extraction.qmd`) — only the service set and the derived
overlap categories are new.

## Where the files are

- **Vector (gpkg)**: `data/processed/hotspots_5service/{pct,abs}/global/hotspots_global_5service_{pct,abs}.gpkg`
- **Rasters (for Rich)**: `data/processed/hotspots_5service/rasters/*.tif`
- **Summary index**: `data/processed/hotspots_5service/_hotspots_5service_index.csv`

**Note**: none of the above are in git (this repo's `data/` directory is gitignored, same as
every other data output in this project) — share these with Rich by direct file transfer, not
by pointing him at the repo.

## The two metrics: `pct` vs `abs`

Every file comes in two versions:
- **`pct`** — hotspots defined by Symmetric Percentage Change (relative/proportional change,
  1992→2020). This is the **primary, canonical metric** used throughout the paper and book.
- **`abs`** — hotspots defined by absolute change. Included for completeness/consistency with
  the existing 8-service convention, which also produces both. Use `pct` unless there's a
  specific reason to want the absolute-change version.

The two metrics identify *mostly but not entirely* the same cells (189,927 for pct vs. 191,759
for abs) — expected, since a cell can rank in the top 5% by relative change without ranking in
the top 5% by absolute change, or vice versa.

## The four rasters, one per metric (8 files total)

All rasters: 10km resolution, EPSG:8857 (Equal Earth, same projection as the existing 8-service
rasters), Byte type, **nodata = 255**, LZW-compressed.

**Critical interpretation note**: these rasters only have valid pixel values where a cell **is**
a 5-service hotspot (one of the 189,927/191,759 cells above). Everywhere else — including ocean,
non-hotspot land, and the excluded Lakes/Rock & Ice/Antarctica cells — is `nodata = 255`, not
`0`. A raster value of `0` is a real, meaningful value (it means "this cell is a hotspot, but not
for this particular category") — don't treat `0` and `nodata` as interchangeable in any zonal
calculation.

| File | Values | What it means |
|---|---|---|
| `count_water_{pct,abs}.tif` | 0, 1, 2 | **Water overlap category.** Count of water services (N export, Sed export) for which this cell is a hotspot. 0 = hotspot cell, but not for either water service. 1 = hotspot for exactly one. 2 = hotspot for both. **"Water overlap hotspot" = value ≥ 1.** |
| `count_access_{pct,abs}.tif` | 0, 1, 2, 3 | **Access/coastal/pollination overlap category.** Count of access-type services (Nature Access, Pollination, Coastal Risk) for which this cell is a hotspot. **"Access overlap hotspot" = value ≥ 1.** |
| `combined_cross_{pct,abs}.tif` | 0, 1 | **The 3rd, cross-category overlap Becky asked for.** 1 = this cell is a hotspot for at least one water service **AND** at least one access-type service simultaneously. 0 = it's a 5-service hotspot cell but doesn't meet that cross-category condition (e.g., water-only or access-only). This is the "combined" map — explicitly excludes water-only and access-only cells per the spec. |
| `hotspot_count_{pct,abs}.tif` | 1–5 | **Not one of the 3 requested categories — included for reference only.** Total count of all 5 services (not just water or access) for which this cell is a hotspot, 1 through 5. Useful if Rich's pipeline wants an overall compound-risk view under the new 5-service definition, parallel to the existing 8-service `hotspot_count` rasters. |

## The numbers (pct metric, primary)

- **189,927** total 5-service hotspot cells (3.42% of the ~5.55M valid service-cell rows across
  the 5 kept services — this percentage is a rows-across-services denominator, matching the
  existing pipeline's own convention for this figure, not literally "% of the world's land area").
- **110,756** are water-overlap hotspots (≥1 of N export / Sed export).
- **127,172** are access-overlap hotspots (≥1 of Nature Access / Pollination / Coastal Risk).
- **48,001** meet the combined cross-category condition (≥1 water AND ≥1 access).

## What's NOT included yet

- Subregional breakdowns (by income group, WB region, WWF biome, country) — global scope only
  for this handoff. Same extraction approach extends to those if/when needed.
- The corresponding hotspot **maps** (visual PNGs) for water/access/combined — that's the next
  step (Phase 2 of `docs/hotspot_redesign_plan.md`), not part of this raster handoff.
- Rich's own two new beneficiary YAML configs (water-only downstream, access-only travel-time) —
  his to write, per his own message; this handoff just makes sure the input rasters exist in the
  shape his pipeline expects.

## Provenance / how these were built

`scripts/extract_hotspots_5service.R` (vector extraction, reuses `R/get_hotspots.R`'s
`extract_hotspots()` — the same function the main 8-service notebook uses) →
`scripts/gdal_rasterize_hotspots_5service.R` (rasterization, mirrors
`scripts/gdal_rasterize_hotspots.sh`'s conventions exactly, implemented via `sf::gdal_utils()`
since GDAL CLI binaries aren't on PATH in this environment). Full detail, including a near-miss
caught and fixed during this process (a candidate master-grid file with no usable ID column),
in `analysis/WORKLOG.md` (2026-07-28 entries).
