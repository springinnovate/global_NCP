---
title: "Hotspot Redesign Plan — 5-Service Set, New Overlap Categories, Beneficiary Reanalysis"
status: "DRAFT — for Becky's review, she is on holiday this week"
date: "2026-07-28"
branch: "feature/hotspot-5service-redesign"
---

## ⚠️ NEXT SESSION — READ THIS FIRST

1. **Monthly progress report — do this FIRST, before anything else.** No other task, question,
   or "quick check" should start before this is done. (Details/location of the report weren't
   specified in this session — first step tomorrow is locating it.)
2. ~~Verify the native-change-figure regeneration~~ — done: confirmed after session sign-off,
   4-row layout renders correctly (see WORKLOG "2026-07-29 (evening close-out)"). Nothing to
   check here tomorrow.
3. Then reach out to Becky with today's progress (she's on vacation, said to reach out when
   necessary; this is a progress update, not a question needing her sign-off): the 5-service
   overlap maps + summary stats (already sent), the Figure 9 bug fix, the Phase 5.3 mangrove/biome
   check (ruled out), and the new native-10km paired change figure. Slack draft for the
   color-scheme explanation is already written (see chat history / can be re-derived from the
   "orange/teal" reasoning in Phase 5.2 below) — still needs the fuller progress summary wrapped
   around it.
4. Everything else in this doc (subregional reruns, housekeeping items, waiting on Rich) resumes
   after 1-3, not before.

---

## Why this exists

Becky and Steve met last week (Jerónimo not present, Steve's first time weighing in) and raised
several concerns about the current 8-service hotspot analysis. Becky followed up directly in
Slack with concrete instructions. This document turns both into one ordered execution plan,
flags what's genuinely open/uncertain, and is meant to be adaptable into a status message back
to Becky (she's on holiday, but wants the hotspot rasters ready for Rich to pick up).

**Checkpoint status**: all prior work committed and pushed to `feature/swy-model-integration`
(commit `711a621`). This plan executes on a new branch, `feature/hotspot-5service-redesign`,
branched off that checkpoint — if anything here needs to be rolled back, that branch point is
safe to return to.

---

## Status as of 2026-07-28

**Resolved**: the Figure 9 / >10B population bug (open question #2 below) — confirmed root
cause (a "Global" summary row was double-counted in `analysis/plot_multiplier_effect.R`), fixed,
verified against the manuscript's already-correct prose numbers (no text changes needed
anywhere), regenerated, paper/book/presentation re-rendered. Full detail in WORKLOG
(2026-07-28 entry).

**Update (2026-07-29)**: Phase 1 (5-service extraction) and Phase 2 (the 3 overlap maps) are
now both done. Rasters + maps handed off to Rich, buffer-distance and LC-attribution questions
sent to Becky. Also added, beyond the original 3 maps: 2 pairwise breakdowns of the access
category (Access+Coastal Risk, Access+Pollination — the original 3-way access map hid a real
signal since Coastal Risk is a narrow shoreline-only phenomenon), plus a summary CSV + faceted
bar chart giving cell counts/land-area shares for every tier across all 5 maps. Remaining:
Phase 5.3 (biome/mangrove row-offset check), Phase 5.1/5.2 (10km-native change maps, restructured
export/retention figure), and the housekeeping items below. See
`scripts/mapping/make_5service_overlap_maps.R`, `scripts/mapping/make_5service_overlap_summary.R`,
and `docs/hotspot_5service_rasters_README.md` for what was delivered.

**Update (2026-07-29, later still)**: Phase 5.3 (biome/mangrove row-offset check) is done —
**ruled out, not a bug**. Tested all 3,423 `WWF_biome == 'Mangroves'` cells in
`10k_change_calc.gpkg` against a fresh, independent `gpd.sjoin` against raw `Biome.gpkg`,
bypassing `build_master_grid.py`/`enrich_grid.py`'s merge chain entirely — zero mismatches.
Code review also confirmed the merge chain uses genuine index-label joins throughout (`gpd.sjoin`
preserves left-index values; the final restore-geometry step is `left_index=True,
right_index=True`, not a positional `cbind`/`concat`), so this was never structurally the same
failure pattern as the historical `seq_len()` bugs. Both known artifacts in the biome-level
change maps (this one and the Mongolia one) now trace to biome-level aggregation itself, not a
join/ID bug — see WORKLOG (2026-07-29, "Phase 5.3" entry) for full detail.

**Update (2026-07-29, later still, cont.)**: while waiting on Rich, did prep work so his water-
hotspot/access-hotspot beneficiary folders can be absorbed without another code edit. Found
`analysis/compare_exposure_serviceshed.R` was dead code (read a per-folder CSV layout that
`Python_scripts/extraction_script.py` replaced months ago; its own output was referenced nowhere
else in the repo) and removed it — a small piece of housekeeping item 3 done early. Generalized
the two scripts that are actually live: `extraction_script.py` now auto-discovers
`hotspot_beneficiaries/` subfolders instead of a hardcoded 4-item list, and
`plot_multiplier_effect.R`'s compound dumbbell section now drives its category handling off a
`category_defs` table (pre-populated with water/access/combined entries pointing at the 5-service
gpkg) instead of a hardcoded `case_when()`. Regression-verified: reran the script, zero byte
differences in any output vs. before the change. Full detail in WORKLOG (2026-07-29, "Removed
dead compare_exposure_serviceshed.R" entry).

**Update (2026-07-29, later still, cont. 2)**: Phase 5.1 + 5.2 are done. New script
`scripts/mapping/make_native_change_figure.R` plots `10k_change_calc.gpkg`'s native grid directly
(no dissolve step -- eliminates the Mongolia-class artifact and, separately, the mean-value
sign-flip risk that dissolved maps carry) in paired export/retention rows (nitrogen -> sediment ->
coastal -> pollination -> nature access). Rasterized via `terra` for ~5x the speed of a direct
vector render; basemap added so sparse-data services (Coastal Risk) don't look broken; standalone
per-service panels saved at native pixel resolution for zooming. Output filenames are new
(`map_native10km_*.png`), not a silent overwrite -- `map_biome_*.png` and its "WWF Biome" captions
are untouched. Full detail in WORKLOG (2026-07-29, "Phase 5.1 + 5.2" entry).

**Update (2026-07-29, evening close-out)**: native change figure switched to a 4-row layout
(Pollination + Nature Access sharing a row, per Becky's suggestion) instead of 5; goods/damages
color direction re-verified against the live `HOTS_CFG$loss`/`HOTS_CFG$gain` in
`hotspot_extraction.qmd` (not just cross-script consistency). Regeneration completed and was
visually confirmed correct after session sign-off. See WORKLOG.

**Update (2026-07-29, evening close-out, cont.)**: fixed a recurrence of a previously-solved
problem — opaque near-white fill at near-zero values was hiding the basemap in several panels
(N Ret Ratio, Sed Ret Ratio, Pollination, Nature Access worst affected). Same fade-to-transparent
fix already used in `make_paper_supplement_maps.py`, ported to this script. Regenerated and
visually confirmed. **Phase 5.1/5.2 fully done, nothing outstanding on this figure.** Flagged in
WORKLOG as a pattern to check for in any other basemap+diverging-scale script.

**Not started yet**: subregional (income/region/biome/country) hotspot reruns, Rich's beneficiary
rerun (blocked on his reply), the KS/Gini analysis (blocked on Rich), and the rest of both
housekeeping items (grid file naming, remaining script consolidation).

**In progress**: Phase 1 (5-service extraction, global scope) — done via a new standalone script,
`scripts/extract_hotspots_5service.R`. Produced `data/processed/hotspots_5service/{pct,abs}/
global/hotspots_global_5service_{pct,abs}.gpkg` with the 3 new overlap columns (`count_water`,
`count_access`, `combined_cross`). pct metric: 189,927 hotspot cells (1.88%), water=110,756,
access=127,172, combined_cross=48,001. Not yet rasterized for Rich (next step) — see Phase 2.

**Near-miss caught and fixed during this step**: `landgrid_1_clean_enriched_4326.gpkg` (the file
the existing notebook falls back to when no in-memory grid is available) has **no ID column at
all** and would have silently produced a fake positional ID. Switched to `10k_change_calc.gpkg`
and verified its `grid_fid` matches the canonical hotspot output's identity space exactly (zero
mismatches across 225,113 cells, all shared attributes). Full detail in WORKLOG
(2026-07-28, "Near-miss" entry).

---

## Pending major paper edits (defer until this redesign settles — don't lose track)

A running list of substantive paper/book fixes identified during this redesign work, deliberately
**not** being done now — the paper is being left alone until the 5-service numbers are final and
Becky has weighed in on the open structural questions. Do these as one batch once Phase 1-4 land:

1. **The "19.37% / 1,302,099 evaluated cells" figure is wrong, not just old.** Traced to
   `Python_scripts/extract_book_data_fills.py`'s `total_cells_global = ...sum() / 8` — a flawed
   average that only works if every service has the same valid-cell count (they don't: Coastal
   Risk has ~80,000 valid cells vs. ~1.3-1.5M for the rest). The correct distinct-cell denominator
   (verified directly against `10k_change_calc.gpkg`) is **1,372,621** for the same
   Antarctica/Seven Seas/Lakes/Rock & Ice exclusion used throughout this analysis. Needs a proper
   recompute of the paper's hotspot-coverage percentage once the final (5-service) hotspot count
   is in hand — full detail in WORKLOG (2026-07-29 entry).
2. **LC-change/attribution section removal from the paper** — pending Becky's confirmation (message
   sent, not yet answered). Book keeps this content regardless.
3. **5-service methodology section** — staged, not inserted: `docs/paper_5service_methodology_staging.md`
   has candidate paragraphs ready to drop in once numbers are final and Becky's confirmed framing.
4. **`audit_claims.R` full rerun** — every hotspot-count-derived number in the paper needs
   re-verifying once the 5-service definition is finalized (see reconciliation table below).
5. **Access map insight for the paper/book text**: the 3-way access map (Access+Pollination+Coastal
   Risk) undersells its own best signal — worth a sentence noting Access+Pollination is the real,
   broad terrestrial story (12,106 cells) while Access+Coastal Risk is a genuine but narrow
   shoreline phenomenon (967 cells), rather than presenting them as one blended category.
6. **Swap the main change figure reference from `map_biome_{pct,abs}.png` to
   `map_native10km_{pct,abs}.png`** in `paper_draft.qmd` (lines ~226/228) and
   `03-global-patterns-WHAT.qmd` (lines ~37/39) once ready — new figure is built and verified
   (Phase 5.1/5.2, 2026-07-29), captions will need updating too since "WWF Biome" no longer
   describes the new figure's content.

---

## Three housekeeping items to close out before this redesign is considered "done" (not blocking tonight's Rich handoff, but don't let them get lost)

1. **Grid file naming/consolidation.** This repo has several similarly-named, undocumented
   grid-like gpkgs (`landgrid_1_clean_enriched_4326.gpkg` — no ID column — `10k_change_calc.gpkg`,
   `10k_lcc_granular_metrics.gpkg`, `AOOGrid_10x10km_land_4326_clean.gpkg`, one with a literal
   timestamp baked into the filename) with no documented hierarchy anywhere. This ambiguity is
   the root cause behind four separate grid-identity incidents in this project now (grid-ID
   crosswalk bug, many-to-one join bug, the 2026-07-08 WORKLOG `seq_len()` fallback, and tonight's
   near-miss). Needs a proper pass: document which file is canonical and why, and/or
   rename/consolidate so there's one unambiguous master grid file — not several
   similarly-named candidates of unknown relative authority. Deliberately not done tonight
   (renaming under time pressure risks breaking references across dozens of scripts) — flagged
   so it doesn't quietly disappear.

2. **Document the `combos` mechanism as a first-class, user-facing capability — new item (2026-07-29).**
   Tonight's work (water/access/combined-cross, then the two pairwise refinements) was only
   possible because `10k_change_calc.gpkg`'s identity is now solid — that's the actual payoff of
   getting the grid-ID foundation right. But right now, "how to build a custom service grouping"
   only exists as tribal knowledge from this session, not as documentation a future user (without
   this conversation) could follow. Needs, as one deliverable:
   - **A written how-to** (candidate location: `docs/methodology.md` or a new
     `docs/hotspot_service_grouping.md`): what `HOTS_CFG$combos` is, how a named list of service
     vectors becomes a `count_<name>` column automatically, and — the part that ISN'T automatic —
     how to derive an AND/cross-category combo (like `combined_cross`) that the native mechanism
     doesn't support natively. Use tonight's water/access/combined example as the worked
     illustration.
   - **An explicit, reusable tool, not just prose**: right now, deriving a cross-category column
     is a hand-written `mutate(new_col = count_A > 0 & count_B > 0)` line specific to each case
     (see `scripts/extract_hotspots_5service.R`). Worth turning into a small, documented helper
     function (e.g., in `R/get_hotspots.R` alongside `extract_hotspots()`) that takes two (or
     more) combo names and returns the AND column, so a future user calls a function instead of
     re-deriving the pattern from scratch.
   - **This should be scoped together with housekeeping item 1 above (the config-duplication
     consolidation)** — a clean, documented combos tool is undermined if a new user still has to
     know to update 7 separate files to add a new service grouping. Do both in the same pass.

3. **Script consolidation/cleanup.** This session (and the sessions before it) have accumulated
   standalone diagnostic/one-off scripts (`scripts/extract_hotspots_5service.R`,
   `scripts/compute_attribution_true_union.R`, various ad-hoc `check_*.R` scratch scripts) written
   quickly to answer an immediate question or unblock a specific step. Before this redesign is
   considered finished, do a pass to: (a) decide which of these are genuinely reusable pipeline
   steps that should be properly named, documented, and either merged into the relevant `.qmd`
   notebook or kept as a permanent, well-documented script; and (b) delete anything that was
   truly one-off/diagnostic and has served its purpose. Don't let the repo accumulate an
   ever-growing pile of similarly-named scratch scripts — that's the same class of problem as
   item 1 above, just for code instead of data files. **Partial progress (2026-07-29):**
   `analysis/compare_exposure_serviceshed.R` identified as dead (superseded by
   `extraction_script.py` + `plot_multiplier_effect.R`, referenced nowhere else) and removed —
   see WORKLOG. Rest of the pass still open.

---

## Reconciliation with the pre-meeting plan (memory, as of ~2026-07-09)

Before last week's Becky/Steve meeting, the working assumption (tracked in project memory, not
in this repo) was that the paper was close to submission-ready, pending a short list of
confirmations from Becky. That list needs to be re-read against what actually came out of last
week's meeting — some items are now superseded, some still stand independently, and at least one
needs to be *redone*, not just answered, because the hotspot definition itself is changing.

| Old open item (pre-2026-07-09 "MUST ASK BECKY" list) | Status now |
|---|---|
| #0 Attribution-gap sign-off (34.5%/65.5% framing) | **Superseded.** Those exact numbers were since corrected again (2026-07-24, now 34.2%/65.8%) — but more importantly, last week's meeting raised moving attribution analysis out of the main paper entirely (supplement, country/biome aggregation, or a separate follow-on paper). The sign-off question is moot until that structural decision is made; don't chase the old framing further. |
| #1 Paper structure (methods to supplementary? target journal?) | **Still open, untouched by this redesign.** Independent question, still needs Becky's answer whenever she's back. |
| #2 SDR/NDR climate inputs (fixed vs. era-specific) | **Still open, untouched.** Independent of the hotspot redesign. |
| #3 Ch05 scatterplot (replace X-axis or drop) | **Likely moot** if attribution moves out of the main paper — revisit only if attribution stays in. |
| #4 Literature validation of hotspot locations | **Do not do this yet.** It would validate the *current* 8-service hotspot clusters — pointless to spend time on before the 5-service redesign lands, since the clusters themselves are about to change. Explicitly deferred until Phase 1-2 complete. |
| #5 Rt_serv confirmation (C_Risk/C_Risk_Red_Ratio only) | **Still open, independent** — though note C_Risk_Red_Ratio is one of the 3 services being dropped from the hotspot definition (not from the underlying data), so this question may partly resolve itself. |
| `audit_claims.R` / systematic claim verification pass | **Must be rerun from scratch** once the 5-service redesign lands — every hotspot-count-derived number in the paper changes when the service set changes. Don't spend time re-verifying 8-service-era numbers now. |
| Biome boxplots → Annex (deferred item) | **Related but distinct** from the new "replace biome-level change maps with 10km-native maps" ask — check whether these are the same assets or two separate things once Phase 5.1 starts. |
| Phase 5 candidates (WWF Colombia deck, corporate supply chain, GDP exposure) | **Unaffected for now**, but any of these picked up later will need to reflect the new 5-service hotspot definition, not the old 8-service one. |

**The honest summary**: the paper was in "final polish, minor confirmations" mode before last
week. It is not anymore — the 5-service redesign changes the hotspot definition itself, which
cascades into every hotspot-count, intensity, and beneficiary number in the book and paper. That
said, per the technical research already done (see Phases 1-2 above), this is a **bounded,
mechanical effort** — a config change plus a rerun, not a rebuild — the underlying pipeline
architecture holds up fine.

---

## Two things to verify before or alongside execution (not blocking, but worth a quick check)

1. **Buffer distance discrepancy.** Meeting notes say the downstream buffer may be "too
   generous" at 500km. This repo's own methods documentation (`02-methods.qmd`) and the actual
   output filenames (`full_raster_extent_downstream_50k_population.tif`) both say **50km**
   flow-accumulation threshold, not 500km. Either the notes are misremembering, or Rich's
   `wwf_es_beneficiaries` repo uses a different/newer value not reflected in this repo's docs.
   **Action:** ask Rich directly what the current downstream threshold actually is before
   assuming either number.

2. **The 96% / 10B+ / double-counting numbers likely aren't a data bug.** The existing
   beneficiary output structure has four overlap-tier folders (`all hotspots`, `2+`, `3+`, `4+`
   overlapping) that are **nested subsets, not mutually exclusive bins** (4+ ⊂ 3+ ⊂ 2+ ⊂ all).
   Summing "beneficiary count" across those four tiers as if additive would overshoot world
   population immediately — this matches the notes' own "scale error, not a data error"
   instinct closely. **Action:** before rebuilding anything, find out exactly which figure/sheet
   produced the 96%/10B+ numbers and check whether it summed across tiers instead of treating
   them as nested.

---

## Phase 1 — Hotspot redesign: 5 services, 3 new overlap map types

**Goal (from Becky's Slack message, verbatim instructions):**

Keep only: Nitrogen Export, Sediment Export, Coastal Risk, Pollination, Nature Access.
Drop: Nitrogen Retention Ratio, Sediment Retention Ratio, Coastal Risk Reduction Ratio (the
"same pollutant, not independent" problem — retention increases can reflect upstream
degradation, not local improvement, and explaining that nuance would cost too much space in the
paper).

Produce exactly 3 new overlap map outputs:

1. **Water overlap hotspots** — N export + Sed export, hotspot in either or both.
2. **Access/coastal/pollination overlap hotspots** — Nature Access + Pollination + Coastal
   Risk, hotspot in any one, two, or all three.
3. **Combined cross-category overlap hotspots** — across all 5, but keep only cells with **at
   least one water-service hotspot AND at least one non-water-service hotspot** (exclude
   water-only or access-only cells).

**How this maps onto the existing code** (traced directly, not guessed):

- `analysis/hotspot_extraction.qmd`'s existing `combo` mechanism (`HOTS_CFG$combos`) already
  does exactly "count how many named services are hotspots in this cell" — this is the right
  mechanism for categories 1 and 2:
  ```r
  combos = list(
    water  = c("N_export", "Sed_export"),
    access = c("Nature_Access", "Pollination", "C_Risk")
  )
  ```
  This gives `count_water` (0-2) and `count_access` (0-3) columns automatically, no core-logic
  changes needed.
- Category 3 (the cross-category AND) is **not** natively supported by the combo mechanism —
  needs a small addition: either a patch to `R/get_hotspots.R` for a new "cross" combo type, or
  a one-line derived column after extraction: `combined_cross = count_water > 0 & count_access > 0`.
- Dropping the 3 retention/ratio services means editing `loss`/`gain`/`combos` in `HOTS_CFG` —
  the underlying threshold/union logic in `extract_hotspots()` needs no changes, since it's
  already generic over whatever's in `loss`/`gain`.

**Real risk found during research — config duplication.** The service list + direction-of-bad
logic is currently copy-pasted across **7 separate files**, and one of them
(`analysis/compare_exposure_serviceshed.R`) already has an inverted (wrong) copy relative to the
others:

| File | What it holds |
|---|---|
| `R/utils_hotspot.R` | `svc_order` (canonical 8-service order) |
| `analysis/hotspot_extraction.qmd` | `svc_order`, `canonical_lookup`, `HOTS_CFG` (the live config) |
| `analysis/hotspot_synthesis.qmd` | a second, independent copy of `HOTS_CFG` |
| `scripts/mapping/make_faceted_maps.R` | `goods`/`damages`/`canon_order` |
| `scripts/mapping/make_paper_supplement_maps.py` | Python copy of GOODS/DAMAGES + name mapping |
| `gdal_rasterize_hotspots.sh` | `COLUMNS` array |
| `analysis/compare_exposure_serviceshed.R` | **inverted** loss/gain lists (pre-existing bug, unrelated to this redesign, found in passing) |

This project has already had three separate incidents caused by exactly this kind of silent,
duplicated-source-of-truth drift (the grid-ID crosswalk bug, the many-to-one join bug, and now
this). **Recommendation: while touching all 7 files anyway for the 8→5 cut, consolidate into
one canonical service-config object** (a single YAML or R list, sourced everywhere) so this
can't silently drift again. This is optional extra scope — flagging it as a recommendation, not
assuming you want it. Either way, the pre-existing inverted bug in
`compare_exposure_serviceshed.R` should get fixed while we're in there regardless.

**Deliverables for this phase:**
- Updated `HOTS_CFG` (5 services, 2 new combos + 1 derived cross-category column) applied
  consistently across all touched files.
- Three new hotspot gpkgs (global scope first, matching existing `hotspots_global_{pct,abs}.gpkg`
  pattern) with the new binary/count columns.
- Rerun `gdal_rasterize_hotspots.sh` to produce rasters for the new columns
  (`count_water`, `count_access`, `combined_cross`), in the same `data/processed/hotspots/rasters/`
  location Rich's pipeline already expects.

---

## Phase 2 — Map outputs for Becky/Rich

Three visual map outputs matching the three overlap categories above (water, access, combined),
plus rerunning the existing suite of hotspot maps against the new 5-service definition. Target:
**next week**, per the meeting notes — this is the most time-sensitive deliverable since Rich is
blocked on it for the beneficiaries rerun.

---

## Phase 3 — Handoff to Rich: beneficiaries rerun

Becky's Slack message names 4 existing config files in Rich's repo
(`jeronimo_{2,3,4,all}hotspot_beneficiaries.yaml`) and asks for **2 new, similar configs**:

- A **water-hotspot** config — downstream-only buffer, access/travel-time buffer turned off.
- An **access-hotspot** config — travel-time-only buffer, downstream buffer turned off.

This matches the meeting notes' service-specific buffer instruction exactly: water services
(N export, Sed export) → downstream only; coastal, pollination, nature access → travel-time
only. **This repo has no visibility into `wwf_es_beneficiaries`'s YAML schema or README** (it
lives on Rich's machine, not accessible from here) — so this repo's job is only to hand Rich
correctly-structured hotspot rasters/masks; the actual new YAML configs are Rich's to write
(Becky's message already asks him directly: "are we ready... can you do that for me"). Our
concrete deliverable here is making sure the water-only and access-only hotspot rasters exist in
a form his pipeline can point a config at.

**Output Becky wants once Rich reruns**: a table of beneficiary counts by overlap level, plus
downstream/access/combined beneficiary masks.

---

## Phase 4 — Gini/HDI analysis on beneficiary masks

Blocked until Phase 3 completes (needs Rich's rerun output). Once available: KS test (or
equivalent) for whether hotspot-adjacent populations skew toward inequality (Gini) and lower
HDI, likely restricted to 3+ overlaps (~20% of world population per the notes) — this reuses the
existing KS/Cliff's Delta machinery already built for the WHO chapter, just against the new
beneficiary masks instead of the old hotspot_count.

---

## Phase 5 — Figure/map changes (can run in parallel with Phases 1-2)

1. **Replace biome-level change maps with 10km-native change maps — DONE (2026-07-29).** See
   `scripts/mapping/make_native_change_figure.R` and WORKLOG. Reason given: biome
   aggregation misrepresents fine-scale results (e.g., Coastal Risk appearing to affect
   Mongolia). **Gap resolved (2026-07-29): the missing generator script is
   `zonal_stats_toolkit/generate_map_gpkgs.py`** (the sibling repo at `/c/projects/
   zonal_stats_toolkit`) — it dissolves the raw global `Biome.gpkg` polygons by `WWF_biome` name
   and merges in per-biome service stats. **The Mongolia artifact itself is very likely not a
   bug** — dissolving by biome NAME merges every landlocked instance of a biome (Mongolia's Gobi)
   with every coastal instance of that same biome worldwide (Atacama, Namib), so the whole
   dissolved shape gets one blended value and landlocked land visually "inherits" a coastal
   signal that belongs to a different desert on another continent. This is exactly the
   representational limitation 10km-native maps are meant to fix — not something to debug
   further, just confirms the redesign direction is right.
2. **Main change figure restructured — DONE (2026-07-29), same script as item 1.** Paired rows, export next to retention for each service
   (e.g., N export | N retention), red = increases in bad things / green = enhancement, ordered
   nitrogen → sediment → coastal → pollination → nature access. Note: this figure still shows
   retention for context even though retention is dropped from the *hotspot definition* — two
   different roles for the same variable, worth being explicit about in the paper text so it
   doesn't read as inconsistent with the 5-service hotspot set.
3. **Mangrove/Coastal-Risk mismatch check — RESOLVED (2026-07-29), not a bug.**
   `generate_map_gpkgs.py`'s own join is by string name (not positional), so it's not the cause
   here. The suspected culprit — `build_master_grid.py`/`enrich_grid.py`'s `gpd.sjoin` against
   `Biome.gpkg` followed by an index-based merge back onto the grid — was checked both by code
   review (the merge is a genuine index-label join, not a positional `cbind`/`concat`) and by
   direct empirical test (all 3,423 `WWF_biome == 'Mangroves'` cells re-verified against a fresh,
   independent spatial join, zero mismatches). Not the source of the mismatch — see WORKLOG
   (2026-07-29, "Phase 5.3" entry).

---

## Deprioritized / paused, not part of this plan's active scope

- **SWY model integration** — explicitly paused again per your message; picked back up after
  this redesign lands.
- **Attribution analysis (LCC drivers) likely moving out of the main paper** — flagged in the
  meeting notes as a structural decision (supplement-only vs. country/biome aggregation vs.
  separate follow-on paper), with "consensus leaning toward a cleaner main paper." This is a
  discussion item for Becky, not something to execute unilaterally — noting it here so it isn't
  lost, not scoping work against it yet.
- **CN/Kc work for SWY** (three problematic biomes: mangroves, flooded forests, flooded
  savannas) — also paused, noted only for continuity with the already-recovered
  `swy_becky_meeting.qmd`.

---

## Suggested order of execution

1. ~~Checkpoint + branch~~ — done (`711a621` on `feature/swy-model-integration`; this branch is
   `feature/hotspot-5service-redesign`).
2. Consolidate/fix the 7-file service-config duplication (if you want that scope) + fix the
   inverted `compare_exposure_serviceshed.R` bug either way.
3. Phase 1: 5-service hotspot re-extraction + 3 new overlap categories.
4. Phase 2: regenerate maps (highest time-pressure item — Rich is blocked on this).
5. ~~Phase 5.3: biome/mangrove row-offset check~~ — done, 2026-07-29, ruled out (not a bug).
6. ~~Phase 5.1/5.2: 10km-native change maps + restructured export/retention figure~~ — done,
   2026-07-29.
7. Send Rich the water/access rasters; he writes the 2 new beneficiary configs and reruns.
8. Phase 4: Gini/HDI KS test on the new beneficiary masks (blocked on step 7).

---

## Open questions to fold into the message back to Becky

1. Confirm the actual downstream buffer distance in current use (50km per this repo's docs, or
   500km per the meeting notes) — ask Rich directly.
2. Confirm which output (figure/sheet) produced the 96%/10B+ numbers, to verify the
   nested-tier double-counting hypothesis before assuming it's the explanation.
3. Whether to consolidate the 7-file service-config duplication now (recommended, given this
   project's history with this exact failure mode) or defer as a separate cleanup pass.
4. Whether `biome_change_map.gpkg`'s generation step should be rebuilt as tracked code (it's
   currently not reproducible from anything visible in the repo) as part of this work, or
   whether that's acceptable technical debt to leave for later since the plan is to replace it
   with 10km-native maps anyway.
