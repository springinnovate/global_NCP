---
title: "Staged draft: 5-service hotspot methodology section (NOT yet in the paper)"
status: "DRAFT — for Becky's review only. Not inserted into paper_draft.qmd. Do not treat as final."
date: "2026-07-28"
---

## Why this exists

This is a holding area for candidate language describing the 5-service hotspot redesign, so we
have something concrete to share with Becky and react to — **not** a commitment to edit the
paper yet. The actual redesign isn't finished (no maps, no beneficiaries rerun from Rich, no
biome-alignment check done). Once those land and Becky's confirmed the framing, this becomes the
starting point for a real edit to `paper_draft.qmd`'s methods section. Until then, treat
everything below as a draft to discuss, not text to merge.

The book is **not** being edited to remove anything — this redesign adds new sections/groupings
to the book alongside the existing 8-service material, which stays as-is (there's room there).
This staging doc is specifically about what the *paper* (which is comparatively terse and has to
choose one framing) might eventually say.

---

## Candidate paragraph: service set change

> Following methodological review, we restrict hotspot identification to five services with
> clearly independent, non-redundant signals: Nitrogen Export, Sediment Export, Coastal Risk,
> Pollination, and Nature Access. We exclude Nitrogen and Sediment Retention Ratios and the
> Coastal Risk Reduction Ratio from hotspot detection: export and retention of the same
> pollutant are not statistically independent, and an increasing retention ratio can indicate
> upstream degradation rather than local ecological improvement — a distinction that would
> require substantial additional explanation to interpret correctly and is out of scope for the
> main hotspot analysis. [Retention ratios remain part of the broader change analysis — see
> Figure X — just not part of the hotspot *definition* itself.]

## Candidate paragraph: three overlap categories

> Beyond the pooled hotspot count, we define three overlap categories reflecting distinct
> exposure pathways. **Water overlap** hotspots are cells where at least one of the two
> hydrological services (Nitrogen Export, Sediment Export) is in its top-5% decline tail.
> **Access overlap** hotspots are cells where at least one of the three access-type services
> (Nature Access, Pollination, Coastal Risk) is in its top-5% decline tail. **Combined
> cross-category** hotspots are the subset of cells meeting *both* conditions simultaneously —
> at least one water-pathway decline and at least one access-pathway decline in the same 10km
> cell — explicitly excluding cells that are water-only or access-only. This distinction
> matters because water and access pathways connect to beneficiaries through structurally
> different mechanisms (downstream hydrological routing vs. direct travel-time access), which
> in turn require different exposure-buffer treatments in the beneficiary analysis (see Section
> X).

## What's genuinely still open / needs Becky before this becomes real paper text

1. **Whether this replaces or supplements the 8-service framing in the paper.** The book keeps
   both; the paper likely can't afford to show both in full — needs her call on emphasis.
2. **Where retention ratios go in the paper if dropped from hotspot detection** — mentioned in
   passing (per the candidate paragraph above) or given their own subsection?
3. **The actual updated headline numbers** — status as of 2026-07-30:
   - Global raw hotspot counts: done (189,927 pct-metric hotspots; see
     `docs/hotspot_5service_rasters_README.md`).
   - Global maps (water/access/combined overlap, native-10km paired change figure): done.
   - Regional/income/biome/country area-coverage, global-share, relative-intensity, and
     multi-service "hotness" breakdowns: **done** (`data/processed/tables/hotspot_area_stats.csv`,
     `hotspot_multiservice_stats.csv`, 219 per-group CSVs under
     `data/processed/tables/regional_subsets/`). Not yet pulled into paragraph form here.
   - Population exposure by income/HDI/GDP/Gini, subregional: **deliberately deferred**, not
     started for the 5-service definition.
   - Beneficiary counts from Rich's water-hotspot/access-hotspot rerun, and the Gini/HDI KS test:
     still blocked on his reply (see Phase 3/4 in `docs/hotspot_redesign_plan.md`).
4. **Whether the water/access/combined framing gets its own figure/table in the paper**, or is
   folded into the existing hotspot-count discussion as an additional lens.

## Source material

Drawn from `docs/hotspot_5service_rasters_README.md` (the technical raster documentation) and
`docs/hotspot_redesign_plan.md` (the execution plan) — both already reflect the actual, verified
current state of the redesign work. This document is purely about translating that into
paper-appropriate prose, once there's something final enough to write about.
