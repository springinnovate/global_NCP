# Worklog — Global NCP Hotspots (v1.3.4)

### 2026-09-01 — Sediment/coastal unblocked via crosswalk, full 5-service Path B rerun, 6-copy config-drift incident found and fixed architecturally

Picks up directly from 2026-08-31 below, where sediment was blocked on raw rasters from Rich.

**Sediment unblocked without Rich, via a legacy-grid crosswalk.** Found that
`10k_grid_synth_all.gpkg` — a March 2026 zonal-extraction intermediate whose regeneration was later
disabled by default, never deleted — still holds raw 1992/2020 USLE, sediment-export, N-export, and
N-retention levels the raw *rasters* no longer do. It's built on the same legacy
`AOOGrid_10x10km_land_4326_clean.gpkg` grid as the 2026-07-08 LCC striping bug, not the current
master grid, so reusing it required going back through `lc_grid_fid_to_master_fid_crosswalk.csv`.
Didn't just trust the crosswalk's own `match_dist_m` — independently recomputed centroid distance
from each file's own GPKG RTree bounding boxes first (99.4% of rows exact 0.0m matches, flagged-
invalid rows genuinely 6-654km off, not borderline). `scripts/merge_sediment_and_coastal_via_
crosswalk.py` written for this, promoted to canonical `10k_change_calc.gpkg` with a dated backup
kept (`10k_change_calc_BACKUP_2026-08-31.gpkg`).

**Refined once more after B1 caught a real flaw**: of the ~155K many-to-one crosswalk groups, 1,609
have a clearly-best (~0m) match plus a clearly-worse secondary (4,600-5,900m away) that the
crosswalk let through as `valid_match=TRUE` too — averaging that bad secondary match produced 572
genuinely conflicting fids when the same crosswalk was reused for the LC-driver join in
`hotspot_extraction.qmd`. Fixed by keeping only each group's best match(es) (within 1m of the
group minimum) before aggregating — re-ran the sediment/coastal merge with this correction, small
effect (~1,609 of 1.5M cells) but the right thing to do given this data feeds the paper.

**Full Path B (10km-grid) rerun, real numbers, cross-validated three ways.** `hotspot_extraction.
qmd` → 219,138 hotspot cells. `KS_tests_hotspots.qmd` → 38/40 covariate tests significant (both
non-significant results are coastal vs. agricultural plot size, coherent with the paper's own
decoupling narrative). `compute_attribution_true_union.R` → 63.77% attribution gap (was 65.8% under
the old definition), risk ratio 8.23. A separate script, `scripts/extract_hotspots.R` (see rename
below), independently reproduced the same 219,138 for the pct metric — exact match, strong
consistency check. Geographic clustering (mean `relative_intensity` across the 5 services, matching
the established 2026-07-30 methodology exactly): East Asia & Pacific 1.37×, Latin America &
Caribbean 1.22× — same two regions as before, order flipped. Income disparity: lower-middle-income
1.29× vs. high-income OECD 0.69× → ~1.9× (was 1.6×).

**Config-drift incident — 6 independent copies found, not the 3 first assumed.** Fixing `hotspot_
extraction.qmd`'s `HOTS_CFG`/`canonical_lookup` (stale `rt_service` key, no `sed_retention` entry)
surfaced the same pattern independently in `hotspot_synthesis.qmd` and `KS_tests_hotspots.qmd` —
each rendered with exit code 0 while silently computing on the old export/risk service names, caught
only by spot-checking output *content*. A user-driven check against the actual shared Google Drive
folder (screenshot) then revealed a 5th copy, `scripts/extract_hotspots_5service.R` (the generator
of the `count_water`/`count_access`/`combined_cross` overlap columns Rich's beneficiary-buffer
configs literally threshold on) — also independently drifted. A deliberate repo-wide grep sweep
(`grep -rln '"N_export"\|"Sed_export"\|"C_Risk"' analysis/ scripts/ R/`) then found a 6th, more
consequential instance: `R/utils_hotspot.R` defines `svc_order` at `devtools::load_all()` time
(before any qmd chunk runs), and was **silently shadowing** `hotspot_extraction.qmd`'s own
`if (!exists("svc_order"))` guard — meaning an earlier fix to that guard never actually took effect.
`R/hotspot_violins.R` had three more hardcoded copies of the same pattern.

**Architectural fix, not another patch**: `R/service_config.R` is now the single source of truth —
`SERVICE_AMOUNTS`, `SERVICE_RATIOS`, `SERVICE_LEGACY_RAW`, and `service_canonical_lookup()`/
`hotspot_direction_lists()`/`service_names()`/`ratio_names()` accessors, auto-loaded via
`devtools::load_all()` like every other `R/*.R` file. Every one of the 6 drifted files now sources
this instead of redefining the list locally. `hotspot_direction_lists(looking_for=)` also builds in
the direction-flexibility the user asked for going forward (search for declines vs. best
improvements) rather than hardcoding "decline" as the only possible analysis. `extract_hotspots_
5service.R` renamed to `extract_hotspots.R` — the suffix was a leftover from when this was a special
variant of an 8-service scheme; it's the canonical script now, no `extract_hotspots.R` predecessor
ever existed to conflict with. **Known not-yet-audited**: `scripts/audit_hotspot_geography.R` and 7
Colombia/Sandra-deck mapping scripts — not on the current critical path, deliberately deferred and
logged (`docs/pipeline_reference.md` B7) rather than silently assumed fine.

**Rich's hotspot rasters rebuilt and verified.** `scripts/gdal_rasterize_hotspots_5service.R`
rewritten — the old version pointed at a stale July 28 file with old export/risk columns and the
old water/access beneficiary categories, unrelated to today's data. New version reads both
`hotspot_extraction.qmd`'s output (hotspot_count + 5 per-service flags) and `extract_hotspots.R`'s
output (count_water/count_access/combined_cross), producing 18 files total (9 columns × abs/pct),
matching the actual file set in Rich's shared Drive folder (confirmed via user-provided screenshot,
not guessed). Sanity-checked via `terra`: `hotspot_count` range 1-5, per-service columns clean 0/1,
221,372 valid pixels (~1% over the 219,138 vector count, normal reprojection edge effect).

**Boxplot outputs consolidated**, per user request: the old volumetric/ratio/coastal 3-way split
(itself one of the drifted-name locations) dropped in favor of one unified chart per metric — each
facet already used `scales="free_y"` so no information is lost, just fewer files (4 → 2 per
grouping). Surfaced and fixed a related bug: the boxplot's own per-service hotspot direction check
only looked at `HOTS_CFG$loss` (the 5 amounts), silently misclassifying the 3 ratio services as
gain-direction for this chart specifically.

**Paper updated throughout `paper_draft_5service.qmd`**: Abstract rewritten for 5 services, 4 of 5
headline numbers now real (219,138 cells, 63.8% gap, regional/income figures above) — only the
Rich-blocked multiplier-effect figure remains `[TBD]`. Export/risk explicitly scoped as ratio-
formula inputs only, never reported directly (Figure 2's old paired export/ratio layout flagged as
needing to change when rebuilt). Coastal dropped from the global 4-panel change map — data-correct
but a 1-cell-wide coastline fringe is invisible at full-globe scale, confirmed by diagnostic — with
an honest caption rather than a blank-looking panel. Several stale status callouts caught (some by
the user reading closely) and corrected — the top-of-document status note, the Biophysical Modeling
callout, and the Hotspot Identification callout had all drifted to "still pending"/"in progress"
language after the underlying work was actually done.

**New process discipline note**: briefly ran two renders of `hotspot_extraction.qmd` concurrently
(started a second one to pick up the `R/service_config.R`/boxplot fixes without confirming the
first had finished) — no data corruption resulted since `HOTS_CFG` itself was unchanged between the
two attempts, but it was luck, not design. Confirm a prior render has actually completed before
starting another one of the same file.

**Checkpoint commit made before the architectural refactor** (`936be11`) specifically so the
service-config centralization could be attempted freely with a clean rollback point.

**Still pending**: Rich's beneficiary rerun (rasters ready, not yet sent — combined email drafted
covering both this and the still-open Path A raw-raster request), Becky's 6 questions (1 required
before submission — InVEST climate inputs), Figure 2/Annex trajectory maps (blocked on Path A raw
300m rasters, separate `zonal_stats_toolkit` repo), Results/Discussion prose still needs a pass to
replace old numbers with the real ones now available, and the deferred old-output cleanup /
unaudited-scripts list in `docs/pipeline_reference.md` section F.

### 2026-08-28/31 — Service-definition redesign (retention/protection), coastal pipeline dry run, two real bugs found and fixed

Steve clarified his services-list comment (see `becky_steve_feedback_plan.md` and the real email
chain, `docs/manuscript/draft_review emails.pdf`): nitrogen, sediment, and coastal should all move
from export/risk framing to retention/protection *amounts* (not ratios) — the actual ecosystem
service is what's retained/protected, export/risk is the residual. `HOTS_CFG` in
`hotspot_extraction.qmd` updated accordingly (`N_retention`, `Sed_retention`, `C_Prot_service`
replacing `N_export`, `Sed_export`, `C_Risk`; ratios confirmed already excluded from hotspot
detection per the existing Methods text). This also collapses the old damage-vs-good `deg_combo`/
`rec_combo` split, since every service is now benefit-framed — both combos now point at the same
5-service list (kept as two names for output-column compatibility, documented as such).

**Nitrogen**: no work needed — `n_retention_abs_chg`/`pct_chg` already exist in the current
`10k_change_calc.gpkg` from a prior run. Pure config/definition change.

**Sediment**: blocked. Retained amount = USLE − export needs the raw 1992/2020 rasters for both;
only the pre-computed 2020−1992 *difference* rasters survive locally (confirmed absent from both
this project's `data/` and the pre-migration `C:\Users\...\data\global_ncp\` copy). Traced through
a data inventory (`docs/ncp_data_catalog.md`, new this session, from two OneDrive spreadsheets
originally shared by Becky) to a `sci-ncscobenefits-spring` GCS bucket with exact filename/MD5
matches to this project's own `services_slim.yaml` — but the actual URLs 404 (files deleted from
the bucket since the inventory was built). Storage is Rich's; message sent to him 2026-08-28 asking
where the files live now (`docs/manuscript/rich_raw_rasters_request_2026-08-28.draft.md`, since
sent and deleted per the draft-cleanup convention) — **no reply as of 2026-08-31, expected given
weekend timing, but still open.**

**Coastal**: fully unblocked and completed. `Rt_service = Rt_nohab_all − Rt` turned out to already
be computed in `Python_scripts/coastal_protection_join.py`'s join step — not a new derivation.
Rasterized via `rasterize_coastal.py` (had to install `tqdm` into `.venv`, force `PYTHONIOENCODING=
utf-8` around a Windows console crash on a checkmark character, and substitute a same-grid raster
for the original `landcover_gl_1992.tif` template, which no longer exists anywhere local — the
script only needs matching grid geometry, not real land cover values, so this is safe). Extracted
via `summary_pipeline_landgrid.py` inside the project's documented Docker image
(`therealspring/global_ncp-computational-environment`) — see the two real gotchas now written into
`docs/runbook.md`'s new Step 0 (Git Bash's `MSYS_NO_PATHCONV` path-mangling, and the required
`-e ENV_NAME=geopy311` the README never mentioned, without which `python` isn't found in the
container at all).

**Two real, previously-latent bugs found via this dry run, both fixed:**
1. `analysis_configs/c_protection_synth.yaml` referenced four raster files (`Rt_1992.tif`,
   `Rt_2020.tif`, `Rt_ratio_1992/2020.tif`) that no longer exist anywhere local — moved to
   `interim/archive/` on the server at some point, never copied back. Failed loudly
   (`RasterioIOError: ... No such file or directory`) the moment this config was actually re-run
   for the first time in a long while. Fixed by scoping the run to just the new column (those
   values already exist correctly in the current `10k_change_calc.gpkg` from a prior run, no need
   to re-derive), with the reasoning documented inline in the yaml.
2. `process_data.qmd`'s multi-file merge loop used `for (i in 2:length(files_to_load))` — breaks
   when exactly one zonal file is present, because `2:1` evaluates to `c(2, 1)` in R, not an empty
   sequence, causing an out-of-bounds `NA` read (`missing value where TRUE/FALSE needed`). Never
   surfaced before because every prior real run had ≥2 zonal files (services + beneficiaries at
   minimum); this session's scoped coastal-only extraction was the first time anyone hit exactly
   one file. Fixed to `seq_len(length(files_to_load))[-1]`.

**Given process_data.qmd's full merge now needs zonal files that don't survive between sessions,
built a lighter alternative for merging one new/changed variable in**:
`scripts/merge_new_variable_into_change_calc.py` — writes to a clearly-named copy
(`10k_change_calc_DRYRUN_coastal.gpkg`), never touches the canonical file, computes abs/pct change
with the exact SPC formula from the paper's Methods. Deliberately uses raw `sqlite3` rather than
geopandas to read/join — see the "why" comment in that script and the new
`docs/runbook.md` section below, since this surfaced a **second, independent fid-handling bug
class** this project has now hit (first was the LCC `grid_fid` mismatch, 2026-07-08): geopandas/
pyogrio can silently turn a GPKG's `fid` primary key into an unnamed row index instead of a normal
column, depending on file/library version — caught here by an explicit `assert "fid" in
columns`-style check before it could silently mis-join, not by inspection. Also hit and worked
around: GPKG's own RTree-maintenance triggers call SpatiaLite's `ST_IsEmpty()` on *any* row update
to a spatial table (not just geom/fid changes), which plain Python `sqlite3` doesn't have — safe to
drop the two offending triggers on a disposable copy since this operation never touches geometry.

**QA catch worth noting for its own sake**: the first sanity-check sample (10 rows, ordered by
`fid`) showed *identical* 1992/2020 values for every row — looked exactly like a real bug (both
years accidentally rasterized from the same source). Turned out to be an unlucky contiguous
no-change coastal stretch; checking the full 53,186-row set showed 25% with real, small, plausible
differences. Lesson (now baked into the merge script's own sanity-check output): never trust a
small ordered-by-fid sample as representative — check identical-vs-different counts across the
whole non-null set.

**Also fixed while in the neighborhood**: README.md referenced the deprecated
`AOOGrid_10x10km_land_4326_clean.gpkg` grid (the one behind the 2026-07-08 bug) instead of the
correct `landgrid_1_clean_enriched_4326.gpkg`, and six references to
`summary_pipeline_workspace/` instead of the actual `summary_pipeline_workspace_ha/` the configs
use — both stale, both fixed.

**Status**: coastal data validated and sitting in `10k_change_calc_DRYRUN_coastal.gpkg`, not yet
promoted to canonical. Sediment blocked on Rich. Nothing in `paper_draft_5service.qmd`'s Results
section touched yet, per explicit instruction not to change anything before Results until real
numbers are in hand for all three services together.

### 2026-08-27 — Sandra deck fixes (bio slide, Propuesta reframe, render regression); paper/dissertation threads opened

**Sandra deck (`docs/presentations/sandra_valenzuela_colombia_case.qmd`)**:
- Rewrote the "Sobre mi" bio slide's *land systems science* definition and the "me identifico como
  land systems scientist" line — both were thin/redundant. Pulled real language from the user's PhD
  dissertation (`LC_orinoquia/docs/Dissertation_Manuscript_F_JRE2.pdf`, read via `pdftotext` since
  poppler's PATH hadn't propagated yet — see below) instead of writing a definition from scratch: the
  "not only what changes, but how and for whom" framing (Ch.4 discussion) and the "quantitative rigor
  of land system science + critical/political-ecology perspective" framing (Conclusions chapter).
- Reframed the closing "Propuesta" slide: the old intro line ("Colombia hoy existe únicamente dentro
  del agregado regional...") didn't actually state an ask. Replaced with three concrete offers —
  deepen the Colombia-specific cut, sustain institutional reporting, and (the new, sharper point)
  **transfer the know-how itself** — open-source pipeline, IDE-based, AI-agentic workflows, so WWF
  Colombia's own team could run it, not just receive a one-off product. Added a link to the public
  repo (`github.com/springinnovate/global_NCP`, confirmed public via the GitHub API).
- Rearranged that same slide's layout to a proper two-column split (bullets left, map right) with the
  stat callout as a full-width band underneath — the original vertical stack (intro + 3 bullets +
  side-by-side stat/map) was too tall and cut the map off the bottom of the slide.
- **Removed the "23% of Piedmont/Altillanura cells are hotspots" stat callout entirely.** This figure
  had already been corrected twice on 2026-08-20 (see that entry below) via a real point-in-polygon
  check against the dissertation's study-area polygon. On 2026-08-27 the user, looking at the map
  again, judged that the hotspots still read as concentrated toward Caquetá/southern Meta rather than
  Piedmont/Altillanura specifically — casting doubt on the corrected figure too. No time in-session to
  re-run the verification before a meeting, so the stat was pulled from the slide rather than shipped
  unresolved; a note in the `.qmd` flags that it must be re-verified against
  `LC_orinoquia/vectors/msk_pm_crs.geojson` before it's reintroduced.
- Caught and fixed a render regression I introduced: re-rendered the deck with `quarto render --to
  html`, which overrides the YAML's `revealjs` format and silently falls back to pandoc's plain HTML
  writer — the deck lost all reveal.js slide structure (only the first couple of headings rendered,
  everything else collapsed). The `--to html` command is correct for `paper_draft_5service.qmd` (a
  plain HTML doc) but wrong for any `revealjs` presentation in this repo — use `quarto render
  <file>.qmd` with no `--to` override, or `--to revealjs` explicitly, for decks.

**Environment**: installed poppler via `winget install --id oschwartz10612.Poppler --scope user` to
enable PDF reading (needed for the dissertation above). The PATH update did not propagate to a fresh
Claude Code session or a fresh PowerShell process — worked around by calling `pdftotext`/`pdftoppm`
directly by full path (`%LOCALAPPDATA%\Microsoft\WinGet\Packages\oschwartz10612.Poppler_...\
poppler-25.07.0\Library\bin\`). Likely needs an actual Windows sign-out/restart to fix system-wide.

**Two other threads opened, not substantively started**: (1) a new dissertation-manuscript review
thread (`LC_orinoquia/docs/Dissertation_Manuscript_F_JRE2.pdf`) — user flagged the last chapter as
weak/low-priority; (2) the paper's Methods section review, explicitly paused mid-thread by the user
before this session and not yet resumed. See `docs/HANDOFF_2026-08-27.md` for full detail on both.

### 2026-08-21 — Tremie follow-up sent (CLEC abstract confirmation + Módulo 5)

Sent the overdue follow-up to Tremie: confirmed the CLEC abstract was submitted by the Aug 12
deadline, and re-surfaced the Módulo 5 ("Priorización de intervenciones") course-design interest
that had gone quiet for over a week. Both draft files (`docs/applications/
tremie_reply_email_draft.md`, the pre-submission Aug 11 version, and `tremie_followup_2026-08-20.md`,
the actual one sent) removed now that the email is out — no longer needed once sent, per this
project's convention of not keeping stale communication drafts around.

### 2026-08-20 (later) — Geographic claim in Sandra deck was wrong; verified and fixed, new audit script built

A pre-existing, never-verified claim on the "Dónde"/"Where" slide ("concentración más fuerte en el eje
cafetero, los Andes centrales y el piedemonte de la Orinoquía") turned out to be inaccurate — caught
because the user's own on-the-ground GIS read suggested the real clusters were around Caquetá and
Magdalena Medio instead. Verified properly with a real spatial join (point-in-polygon against Colombian
departments, `rnaturalearth`/`rnaturalearthhires`, not a nearest-feature approximation): of 1,423 national
hotspot cells, **Meta = 259, Caquetá = 204, Antioquia = 178** — nothing else comes close (next is Guaviare
at 84); the coffee axis doesn't appear in the top 15 departments at all. Fixed on both ES/EN decks' "Dónde"
slide bullet.

A second round of scrutiny (user correctly pushed back that Meta/Caquetá/Antioquia are large departments,
so raw count could be a size artifact, and that the pattern looked like two clusters, not three) led to a
deeper check that also caught a second, related overclaim on the closing "Propuesta"/"The offer" slide
(originally "2 métodos, 1 geografía," asserting the hotspot map and the user's own dissertation independently
confirmed the same geography). Verified: (1) size-normalized rate confirms Meta genuinely leads even
per-area (29.7%), not just by raw count; (2) k-means (k=2) on high-concentration cells confirms two real
national clusters — one blending southern Meta with Caquetá/Guaviare/Putumayo (Amazon deforestation arc),
one in Antioquia/Santander (Magdalena Medio) — "Meta" as an administrative label was hiding that most of
its hotspot cells belong to the southern cluster, not a distinct Piedmont zone; (3) checked hotspot cells
against the dissertation's actual study-area polygon (`LC_orinoquia/vectors/msk_pm_crs.geojson`), not an
invented latitude cutoff — only **26 of 1,423 national hotspot cells (1.83%) fall inside the real
Piedmont/Altillanura boundary**, not 18%. Within that boundary the hotspot rate is a real, still-meaningful
23% vs. 12.5% nationally — closing slide now states this instead of the inflated department-wide figure.

New reusable tool: `scripts/audit_hotspot_geography.R` — generalizes this whole verification chain (raw
count by admin unit → size-normalized rate → point-in-polygon not nearest-feature → k-means cluster check →
optional check against a specific study-area polygon) so the next geographic claim gets checked before
it ships, not after. Follows the existing `audit_claims.R` convention (claim stated in a comment, computed
value printed alongside it). Confirmed runs standalone end-to-end, reproduces the same numbers as the
ad-hoc scratchpad version that found them first.

Both decks re-rendered and screenshotted after each fix — no overflow, both confirmed clean.

### 2026-08-20 — Sandra deck: full English translation, WWF-brand visual redesign, global↔Colombia demonstration slides

Big-picture arc: the Sandra Valenzuela deck went from 10 Spanish-only slides to 13 slides in both
Spanish and English, restyled to match the WWF PowerPoint template's own "big number in a rounded
card" pattern instead of burying headline stats in bullet text, plus real content fixes caught by
close review rather than just polish.

**Visual system**: new `.stat-row`/`.stat-callout` and `.mini-table` CSS components in
`wwf_theme.css`, applied throughout — 2.93×, 57.9%, 75.8%/38.1M etc. are now visual tiles;
compact reference tables define the 5 headline services on both the "value" (Oportunidad) and
"change" (Dónde) slides, same 5 underlying NCPs viewed through two lenses.

**Three new slides**: "Critical assets, service by service" (6 new maps — 5 individual + combined
— from the paper's own per-service `prioritizr` solutions staged 2026-08-19; script
`scripts/mapping/make_colombia_cna_per_service_maps.R`) before Oportunidad; and a paired "global
then Colombia" raw-change demonstration (`scripts/mapping/make_global_change_5panel.R`, reusing
`make_native_change_figure.R`'s rasterize+geom_tile approach for the 1.37M-cell global run)
before Dónde — visually proving the "same methodology, any scale" claim instead of just asserting
it.

**Real bugs, not just cosmetic**: the priority-overlap map caption was showing 14.2% instead of
57.9% (the exact framing the deck's own notes warn against) — fixed in
`make_colombia_priority_overlap.R`. GEP was mis-described as "the GDP equivalent for nature's
value" — corrected to "modeled on GDP, for the value of ecosystem services" (verified against the
paper's actual methods). "1,423 celdas" on the Priorización slide had no stated denominator — now
explicit (hotspot-of-change cells specifically, 12.5% of Colombia, not "all of Colombia" as it
read before).

**Methodological question surfaced, not yet resolved**: verified against Chaplin-Kramer et al.'s
actual methods text that critical-natural-asset criticality already incorporates
beneficiaries (downstream population for retention services, travel-time population for nature
access, flight/protection radius for pollination/coastal) — it is NOT pure in-situ biophysical
magnitude, contrary to an assumption this session started with. This project's own
hotspot-of-change metric carries no such weighting. Crossing the two is still methodologically
sound (same "value × threat" logic as Myers' biodiversity hotspots) but the deck's separate
"Quién" population-buffer step is an independent, unreconciled beneficiary computation from CNA's
own internal one — flagged as a real question for Becky, not a deck problem. Full grounding in
memory `project_becky_per_service_cna_idea.md`; tightened version now in the Priorización slide's
notes (both languages).

**Not done**: variable-intensity color ramp for the 5 new per-service CNA maps (currently flat
binary fill; aggregate CNA map already does this via continuous 1-20 rank, but per-service
solutions only have a binary 90%-target, no rank — fix requires the continuous "realized"
magnitude layers instead, nature-access file matching still ambiguous). Also pending: the same
visual treatment for `docs/reports/colombia_clec_report.qmd`, and the paper/SWY threads
(untouched, as instructed). Full next-session plan in `docs/HANDOFF_2026-08-20.md`.

### 2026-08-20 (continued) — Variable-intensity fix for the per-service CNA maps, built

Closed out the "not done" item above. `make_colombia_cna_per_service_maps.R` now modulates fill
opacity within each service's critical (green) cells using the continuous "realized" magnitude
layers in `individual_layers/` (bilinear reprojection, 1st/99th-percentile clip, same pattern as
`make_colombia_report_maps.R`'s change panels) — a disclosed different data source from the binary
solution itself (magnitude of provision, not optimization priority rank), not a substitute
pretending to be the aggregate map's rank. Nature access (no 1:1 realized-layer match) approximated
as mean(rural_60, urban_60) per user decision — closest match to the solution's "within 1hr"
framing. All 5 individual panels + combined 5-panel figure regenerated and visually verified: real
within-service variation is now visible (e.g. dark Andean-spine clusters for pollination/sediment,
gradient across nitrogen's eastern-plains halo) instead of the flat fill. README updated to match.

### 2026-08-12 — Colombia CLEC/Sandra report matured into a real "priority" analysis; abstract finalized and shared with Laín

Continuation of the Colombia CLEC congress / Sandra Valenzuela (WWF Colombia) thread from
yesterday. Big-picture arc of today's session: the Colombia report (`docs/reports/
colombia_clec_report.qmd`) went from "two layers shown side by side" to an actual integrated
priority analysis, and the CLEC abstract (`docs/applications/clec_abstract_draft.md`) went
through several rounds of precision fixes before being shared.

**Critical natural assets got the same statistical treatment hotspots already had.** New scripts
`scripts/mapping/make_colombia_priority_overlap.R` and `make_colombia_cna_stats.R`: Colombia
holds **2.57% of the world's critical natural assets while being only 0.88% of eligible global
land — 2.93× relative intensity**, stronger than any hotspot-of-change finding. Within Colombia,
**páramo/montane biomes lead at 1.64×** (sabanas/Llanos second, 1.34×), reinforcing the
"páramos that supply Bogotá" framing already used elsewhere.

**The actual prioritization signal is the overlap, not the two layers separately** — a point the
IADB workshop deck's own notes had already made but never computed. New: **57.9% of Colombia's
hotspots of change also fall inside a critical natural asset** (824 of 1,423 cells) — the real
"avoid this first" signal, now the lead finding in both the report and the Sandra deck.

**Corrected a real attribution error, traced across the whole session's materials**: had been
calling the road-infrastructure policy "Infraestructura Vial Alineada con la Biodiversidad (GRI)"
and attributing it to WWF Colombia — wrong on both counts. The actual policy is the
**Lineamientos de Infraestructura Verde Vial (LIVV)**, a Mintransporte-led interministerial policy
(with Minambiente, ANI, INVIAS, DNP), where WWF Colombia and FCDS are credited as technical
support, not authors. ("Infraestructura Vial Alineada con la Biodiversidad" is actually the title
of the separate CLEC post-conference course brochure — a different thing entirely.) Fixed
everywhere: report, deck, abstract. Also verified via live web search that "jerarquía de
mitigación de impactos" (impact mitigation hierarchy) is real, LIVV-documented terminology
(confirmed against a WWF Colombia article on the LIVV pilot, El Retorno–Calamar) — not, as
initially drafted, an invented "road ecology" framework.

**Beneficiary map fixed for a real accuracy problem**: the reach-zone map's teal layer was being
captioned as "population" when it's actually just the geographic buffer mask population gets
overlaid onto — not population itself. Fixed the caption, and added a genuine population-density
panel (`colombia_beneficiary_population_map.png`, using the previously-unused
`full_raster_extent_union_population.tif`) as a second tab, so the report now shows both the mask
and actual population density, not one mislabeled as the other.

**Two new deliverables**: `docs/reports/colombia_clec_report_en.qmd` (full English translation of
the report, prose-only — charts keep their Spanish labels, per explicit scope decision, to avoid
re-running ~10 R scripts under today's time pressure) and `docs/applications/clec_abstract_en.qmd`
(faithful English translation of the abstract, explicitly not word-capped — exists to mirror the
Spanish accurately, not to meet the 300-word submission limit). Both render to `.docx` via
`quarto render --to docx` for easy sharing outside the repo.

**Abstract went through several precision passes** (terminology: "hotspots de cambio," not bare
"hotspots"; the LIVV/mitigation-hierarchy fix above; fixed a stray gerund-led sentence and a
missing em-dash from live edits; defined "categoría cruzada combinada" inline instead of dropping
undefined jargon; connected an orphaned sentence about sediment export back to the road-relevance
point). Final Spanish version: 294 words (limit 300) — verified directly against the actual
paragraph text, not just the file's own count note (which had drifted stale mid-session more than
once; worth double-checking word count from the source text directly, not trusting a comment).

**Status**: Spanish abstract (`docs/applications/clec_abstract_es.docx`) shared with Laín Efrén
Prado (co-author, WWF Colombia) today. English version (`clec_abstract_en.docx`) being prepared
to show Becky before final submission — CLEC deadline is today (2026-08-12), submission portal
`clec-lactwg.org/inscripcion-y-resumenes/`. Still open: institutional-name format unconfirmed
against the actual form, Becky's preferred author-name format unconfirmed, and the
Colombia-specific HDI/Gini/GDP beneficiary-disproportionality analysis (parallel to the Phase 4
report) is flagged as a TODO directly in `colombia_clec_report.qmd` near the "Quién" section —
deferred until after today's noon meeting.

### 2026-08-11 (later) — Coastal Risk denominator bug: root cause, scoped Colombia fix, pipeline fix

Picked back up the Coastal Risk 0.12x flag left open in the CLEC/Sandra thread (see entry below,
"Colombia country-cut groundwork"). Traced the root cause in `analysis/hotspot_synthesis.qmd`
(the `calc-intensity` chunk, ~line 317): `n_total` for every service was being counted off the
full global land grid (`grid_df`), not the set of cells where that service actually has valid
data. That's correct for Pollination/Sed_export/N_export/Nature_Access (valid on ~1.38-1.40M of
~1.4M land cells, effectively the whole grid) but wrong for Coastal Risk, which is only valid on
a narrow coastal fringe — confirmed directly from `plt_long.rds`: exactly 80,040 rows for C_Risk
globally (and 0 of them NA — `plt_long` simply omits ineligible fid×service pairs rather than
storing them as NA). Because `expected_share = n_total / sum(n_total)` reused the same
land-grid-based `n_total` for every service, Colombia's "expected" Coastal Risk share was computed
as its share of *total land area* (0.87%) rather than its share of the *actual global coastal-cell
pool* — Colombia is disproportionately interior (Amazon/Andes/Llanos), so this inflated the
expectation and made the real disparity look worse than it is.

**Scoped fix (for today's CLEC/Sandra materials only)** — ad-hoc R query direct against
`plt_long.rds`, filtered to non-NA/valid C_Risk cells, same exclusions (`income_grp != "2. High
income: nonOECD"`, `exclude_regions`) as the main pipeline: Colombia has 182 valid Coastal Risk
cells globally out of 41,635 valid worldwide (post-exclusions). Corrected relative_intensity =
**0.24x** (up from the buggy 0.12x), corrected pct_area (coverage within Colombia's own coastal
cells) = 2.20% (4 of 182). Still <1.0x — Coastal Risk is genuinely not a disproportionality story
for Colombia even corrected — so this doesn't change the plan to lead the CLEC/Sandra materials
with Pollination (1.83x) and Sediment export (1.51x), which were never affected by this bug (it's
specific to Coastal Risk's sparse geography, not a general area_stats problem). Script not saved to
repo (ad-hoc diagnostic, per standing preference).

**Pipeline fix** (same session, applied directly since the code change was small): patched the
`calc-intensity` chunk to build each service's per-group cell count from `plt_long`'s actual
fid×service pairs (`valid_cells_by_svc`, inner-joined into the grouping grid) instead of the full
`grid_df`. This is a real fix, not scoped to Colombia — it corrects `n_total`/`pct_area`/
`expected_share`/`relative_intensity` for Coastal Risk across every grouping (region, income,
biome, country), and incidentally also corrects the same class of bug for `C_Risk_Red_Ratio`,
`N_Ret_Ratio`, and `Sed_Ret_Ratio` (also geographically-restricted services, per `plt_long` row
counts — 80,040 / 1,020,987 / 1,036,796 respectively vs. ~1.40M for the full-coverage services),
though those aren't part of the 5-service headline pipeline. Triggered a full
`quarto render analysis/hotspot_synthesis.qmd` in the background to regenerate
`hotspot_area_stats.csv` (all groupings) and the ~20 intensity/coverage/share plots under
`outputs/plots/intensity/`.

**Verified**: render completed clean (all 26 chunks, `analysis/hotspot_synthesis.html`).
`hotspot_area_stats_Colombia.csv` now matches the scoped calc exactly (n_total=182, pct_area=2.20%,
expected_share=0.437%, relative_intensity=0.240x). Side effect caught: Nature_Access wasn't 100%
full-coverage either (11,378→11,376 valid cells for Colombia) — same class of bug, much smaller
magnitude, now also corrected. Checked `docs/manuscript/*.qmd` and `docs/presentations/*.qmd` for
any cited Coastal Risk relative_intensity ratio (the region/country disproportionality metric this
bug affects) — none found; existing Coastal Risk mentions there are all from the separate
HDI/Cliff's-delta socioeconomic-correlate analysis (`paper_draft.qmd`/`02-methods.qmd`), which is
unaffected. No downstream correction needed elsewhere right now.

### 2026-08-11 — Phase 4 report finalized and shared with Becky and Rich; thread closed

Multi-session polish pass on `docs/reports/phase4_beneficiary_disproportionality_report.qmd` (user
hand-edited throughout, several rounds of Claude fixes/additions on top): added the two-stage
AND/OR flowchart (static PNG, not live Mermaid — portability requirement), a 4-way tabbed map
comparison (downstream/travel-time/union/intersection, CSS-only tabs, no JS), the union-vs-
intersection statistical robustness check (Cliff's delta barely moves for GDP/Population/Gini,
confirms HDI's null result), numeric backing added to the Key Findings cards, and a GDP-flat-
vs-Gini-rising interpretive note (different dimensions — average wealth vs. within-place
inequality — diverging exactly at the most compound-risk tier). Clarified the diagram is
specifically the union version, and that the maps use ~0.93km source-raster resolution
deliberately (not the 10km statistical grid) so buffer geometry stays visible.

**Report shared with Becky and Rich (2026-08-11).** Ball is in their court — open items still
awaiting their input: (1) Rich's confirmation on the 50km downstream buffer's lateral width and
whether the distance itself is right; (2) Becky's call on how "people reached" should be framed
given the union-vs-intersection gap; (3) whether the intersection-based numbers become the
headline in the paper or stay a robustness footnote.

**Render workflow going forward**: `quarto render docs/reports/
phase4_beneficiary_disproportionality_report.qmd --to html` from the repo root — regenerates the
same portable, self-contained file (`embed-resources: true`, zero external dependencies) at
`docs/reports/phase4_beneficiary_disproportionality_report.html`. Source is real markdown +
`phase4_report_styles.css`; only the SVG bar/distribution charts and the map-tabs widget are raw
HTML blocks (data-driven — regenerate via the Python scripts referenced in-file rather than
hand-editing coordinates).

**Also flagged, not yet worked**: `docs/manuscript/paper_draft_5service.qmd` needs a real revision
pass — user's own assessment is that it currently has "a lot of AI slop" and needs substantial
work before it's submission-ready. Not started this session; noted here so it isn't lost.

**Next**: this thread (5-service hotspot redesign / Phase 4) is closed pending Becky/Rich's
response. Active work shifts to the Sandra Valenzuela meeting prep and the CLEC congress
module/workshop submission — see `docs/applications/clec_sandra_sprint_plan.md` and
`docs/applications/colombia_capability_portfolio.md` (both already tracking this thread from
2026-08-07) — picked up fresh in a new session per the user's own token-budget call.

### 2026-08-10 — Intersection-based KS test run for all 3 categories; qmd source built for the report

**Ran the actual statistical comparison, not just area.** Built intersection rasters (pixel-level
AND of downstream+travel-time, geo-aligned via `rasterio.windows.from_bounds`) for all 3 report
categories (combined-cross, 3+, 4+), zonal-extracted onto the 10km grid locally (RasterioRasterSource,
no Docker — already validated equivalent), reran KS + Cliff's delta against HDI/Gini/GDP/Population.
~11 min total, backgrounded.

**Result**: GDP, Population, and Gini are robust to the union-vs-intersection choice (deltas barely
move, Gini actually *strengthens* under intersection: 0.33→0.38 at the 4+ tier). **HDI is not
robust** — already the weakest finding under union (δ 0.04–0.10), it shrinks further under
intersection and flips sign at the 4+ tier (δ -0.0097) — reinforces the original "no meaningful
relationship" caveat rather than undermining it. Full table:
`data/processed/tables/union_vs_intersection_comparison.csv`.

**Built a Quarto source** (`docs/reports/phase4_beneficiary_disproportionality_report.qmd` +
`phase4_report_styles.css`) for the report, since the hand-authored HTML was hard to navigate for
manual edits. Prose/tables/callouts in real markdown (fenced divs for the custom-styled blocks,
real blockquote for Becky's quoted ask); only the 8 SVG bar/distribution-chart panels stay as raw
HTML (data-driven, regenerate rather than hand-edit if numbers change). `quarto render ... --to
html` with `embed-resources: true` reproduces the exact same self-contained, portable file at the
same path — confirmed output is byte-comparable in size and content markers to the hand-authored
version. Redeployed to the same artifact URL after adding the intersection-comparison table.

**Files added**: `docs/reports/phase4_beneficiary_disproportionality_report.qmd`,
`docs/reports/phase4_report_styles.css`,
`data/processed/tables/intersection_mask_coverage_10km.csv`,
`data/processed/tables/ks_results_beneficiary_masks_INTERSECTION.csv`,
`data/processed/tables/union_vs_intersection_comparison.csv`, plus
`derived_intersection_downstream_AND_traveltime_coverage.tif` in the `tier_3plus`/`tier_4plus`
beneficiary folders (combined-cross's was already written 2026-08-07, regenerated here for a
nodata-flag consistency fix — see same-day entry below).

### 2026-08-07 (cont. 6) — Union vs. intersection check (real, not cosmetic) + report expanded for a broader audience

User's sharp catch: Stage 1 selects combined-cross-category cells using AND (water hotspot AND
access hotspot, same cell), but Stage 2's beneficiary buffer combines the two resulting masks
(downstream, travel-time) using OR — a real inconsistency worth checking empirically, not just
theoretically. Since downstream reach follows the river network and travel-time reach follows
roads, expected intersection ("reached by both") to be substantially smaller than union ("reached
by either"), because those two networks are geometrically unrelated except very close to the
source.

**Two real bugs caught and fixed while checking this, in sequence** — worth recording precisely
since both were self-caught before any number was reported:
1. First attempt compared `full_raster_extent_downstream_50k_coverage.tif` (15,881 rows) against
   `..._within_travel_time_coverage.tif` and `..._union_coverage.tif` (16,113 rows each) using raw
   pixel-row indices — silently misaligned by ~117 rows / ~1° latitude. Caught because the
   individual downstream-only and travel-time-only areas happened to match the known-good
   reference table closely by coincidence, but the union didn't — a real discrepancy, not noise.
2. Second attempt fixed the alignment (bounds-based windowing via `rasterio.windows.from_bounds`)
   but hardcoded pixel size as 0.01° instead of reading the actual resolution (1/120° =
   0.008333...°) — inflated every area by exactly (0.01/0.008333)² = 1.44×, confirmed by the ratio
   matching precisely.
3. Third attempt: correct on both counts, all three components (downstream-only, travel-time-only,
   union) now match the reference table within ~0.4% — validated before trusting the result.

**Result**: intersection area for combined cross-category is 14.1M km² (10.28% of land) vs. the
union's 32.5M km² (23.65% of land) — intersection is only 43% the size of the union. Substantively
different, not a rounding difference. **Not yet checked for the 3+/4+ tiers** — same structural
question applies (both use the same downstream-OR-travel-time combination), just not verified.

Wrote the validated intersection mask to a new file, `derived_intersection_downstream_AND_
traveltime_coverage.tif`, into the same `combined_cross_category_beneficiaries` folder (name
prefixed `derived_` so it's never confused with one of Rich's original outputs) — user wants to
inspect it directly in QGIS.

**Confirmed the coverage/population file pairing** per user's question: each mask type (`type:
travel_time_population` in the configs) produces two paired rasters — `_coverage.tif` (binary
0/1 buffer flag) and `_population.tif` (actual LandScan population values, already masked/clipped
to the buffer — sum directly, no separate coverage × population multiplication needed). Same
pairing across all masks in all 8 category folders.

**Read all 8 of Rich's configs directly** (not just the 3 used in the report) to build a complete
reference table — shared inputs (LandScan Global 2023 for population, ASTER GDEM, HydroSHEDS
level-6 subwatersheds, friction_surface_2019 for travel time) and per-folder condition
raster/expression/masks. Notably: **the beneficiary "people reached" figures use a different
population dataset (LandScan 2023) than the HDI/Gini/GDP test's own population variable
(GHS_POP_E2020_GLOBE_sum)** — worth naming explicitly so nobody assumes it's the same number
twice. Full table added to `docs/hotspots_rasters_data_dictionary.md`.

**Report expanded for a non-Rich/Becky audience**, per explicit user request ("assume we need to
share this with more people, not necessarily so savvy with the depth of the data"): added the
full 8-category menu (which 3 were used and why — purely because Becky named exactly those three,
nothing else), the LandScan-vs-GHS_POP population-source note, and the union-vs-intersection
callout with the empirical 43% result, framed as an open decision rather than a resolved one.
Redeployed to the same artifact URL.

**Files added**: `data/processed/hotspots_5service/rasters_5_var/output_..._combined_cross_
category_beneficiaries/derived_intersection_downstream_AND_traveltime_coverage.tif`. **Files
changed**: `docs/hotspots_rasters_data_dictionary.md` (new detailed input table + union/
intersection + spatial-alignment-caution sections), `docs/reports/
phase4_beneficiary_disproportionality_report.html`.

### 2026-08-07 (cont. 5) — Made the report actually portable: replaced live Mermaid with a static image

User wants to Slack the report file directly to Becky and Rich, not just share the claude.ai
link. Caught a real problem before that could go out: the category-logic flowchart used
`<pre class="mermaid">`, which only renders because the claude.ai artifact viewer injects Mermaid
support server-side — a plain browser (or Slack's own file preview) has no Mermaid.js loaded and
would show raw text instead of a diagram. Confirmed this was the *only* non-portable element
(`grep`'d the whole file for `cdn`/`<script src`/`mermaid` — zero other hits, every image was
already base64-embedded).

Rebuilt the same flowchart as a static matplotlib figure (boxes, AND/OR hexagon gates, arrows —
hand-laid-out coordinates, same two-stage logic as the Mermaid version) rather than trying to
embed the Mermaid.js library inline, which would have bloated the file for uncertain
cross-browser reliability. Verified visually before embedding. Replaced the `<pre class="mermaid">`
block with the rendered PNG (base64), removed the now-dead `pre.mermaid` CSS rule.

**File is now genuinely self-contained**: zero external dependencies (confirmed via grep), ~937KB,
safe to attach as an actual file in Slack — not just the claude.ai link. Republished from the same
repo path, same URL as before (`91cc70fd-08ea-4503-8df6-009a990a687d`).

**Files added**: `docs/reports/diagram_category_logic.png` (source asset, also embedded in the
report).

### 2026-08-07 (cont. 4) — Report moved into the repo, two new visuals, tone pass for an expert (Rich) audience

**Relocated the report from the ephemeral scratchpad into the repo**: `docs/reports/
phase4_beneficiary_disproportionality_report.html` — user wanted a real, stable file to open and
edit directly. Republishing an Artifact from a different file path mints a new URL (confirmed via
the tool's own docs, not assumed) — old link (`00787df4-...`) is now stale and was never shared,
so no harm; new canonical link is `91cc70fd-08ea-4503-8df6-009a990a687d`. Will keep publishing
from the repo path going forward, not the scratchpad copy.

**Two new visuals added, both requested directly**:
1. **Maps of the actual downstream mask** (combined cross-category, 50km-downstream component
   only — deliberately excluding the travel-time component so the water-pathway geometry isn't
   confounded by the access-pathway one). Global overview (decimated 6x via manual numpy max-pool,
   `Resampling.max` isn't valid for plain reads, only warp operations — had to fix that) plus a
   native-resolution zoomed inset over the Amazon/Andes river systems. Both built from the
   existing fine-resolution `full_raster_extent_downstream_50k_coverage.tif`, no new zonal
   extraction. Basemap: `data/vector_basedata/cartographic_ee_r264_correspondence.gpkg` (country
   boundaries, EPSG:4326, matches the raster CRS directly). **Genuinely informative once rendered**:
   the pattern is clearly dendritic/drainage-network-following (not random blobs unrelated to
   hydrology), but individual traces have real width, and in high-hotspot-density areas so many
   traces overlap that it reads as a solid mass — visual evidence bearing on both halves of the
   open question to Rich at once.
2. **Distribution comparison chart** (GDP, Population, Gini, HDI) — q10/median/q90 range, inside
   vs. outside the mask, per category. This is what the KS test actually operates on, not just
   the single Cliff's delta summary already in the report. Log-scale panels for GDP/Population
   (linear would make q10≈0 vs q90 in the hundreds of millions unreadable), linear for Gini/HDI.
   Computed pixel positions in Python from `ks_results_beneficiary_masks.csv`'s existing quantile
   columns (no new statistics run), then hand-built the SVG — same approach as the earlier
   Cliff's-delta chart.

**Tone pass, prompted directly by the user**: caught narrating Rich's own tool back to him
("This determines how 'generous' 50km really is in practice" and similar) — same over-explaining
pattern already flagged once before in this project's history for an expert audience (see
`project_idb_wwf_workshop` memory, "blockquote felt patronizing"). Trimmed the Rich-question
callout to the two bare questions, no mechanism narration. Also condensed the "reach numbers"
callout from a dense paragraph into a 4-point list per explicit request ("more technical / point
by point"). Removed the "— Becky, Slack, 2026-08-05/06" citation line under her quoted question,
also per direct request.

**Files added**: `docs/reports/phase4_beneficiary_disproportionality_report.html` (canonical,
replaces the scratchpad copy), `docs/reports/map_downstream_global.png`,
`docs/reports/map_downstream_inset.png`.

### 2026-08-07 (cont. 3) — Added the category-logic diagram to the Becky report itself

Same two-stage AND/OR Mermaid diagram just added to `docs/hotspots_rasters_data_dictionary.md`
also added directly to the Becky-facing HTML report (`<pre class="mermaid">`, rendered natively by
the artifact platform, no library embed needed), right after the "What was tested" term
definitions. Wrapped in a fixed-light-background card (`.diagram-card`) rather than the page's
themed surface, since Mermaid's default rendering assumes a light background — keeps it legible
regardless of the viewer's light/dark setting. Redeployed to the same artifact URL.

Also wrote a strict Monday-Wednesday sprint plan for a separate, unrelated thread (CLEC congress
abstract + Tremie course response + Sandra Valenzuela meeting materials, all converging on
2026-08-12) — see `docs/applications/clec_sandra_sprint_plan.md`, not part of this hotspot
redesign work, tracked there and in memory instead of here.

### 2026-08-07 (cont. 2) — Caught and fixed a real self-contradiction in the report's own callouts

User flagged, correctly, that the "upper bound" callout asserted the buffer is "wide enough to
cover nearly the whole populated map almost by construction" — a claim about *mechanism* (buffer
width) — while the adjacent "open question" callout added minutes earlier honestly said the
lateral width is unconfirmed and is exactly what needs asking Rich. Direct contradiction: one
callout presented as settled fact the very thing the other flagged as unknown.

What's actually verified is only the *empirical outcome* (these categories reach a large share of
land/population — real, computed numbers, not in question). What's not verified is *why* —
buffer width is one possible explanation, but an equally plausible one is that ~190,000 dispersed
hotspot cells, each contributing even a narrow downstream/travel-time trace, sum to a large
combined footprint once unioned across the whole map. Rewrote the callout to state only the
verified fact and name both candidate explanations as open, rather than asserting the width-based
one. Redeployed.

### 2026-08-07 (cont.) — Becky report: buffer-generosity callout added; Colombia country-cut groundwork

While the user reviewed the Phase 4 artifact, added a caveat callout he specifically asked for:
the 50km-downstream/travel-time buffers are generous enough that even the restrictive categories
(combined-cross, 3+, 4+) partly inherit the same "covers nearly the whole populated map almost by
construction" effect already flagged elsewhere in this project's history for the 1+/access-only
tiers — added directly to the report next to the 4.1B/2.3B/872M reach figures so it isn't read as
a precise headcount. Redeployed to the same artifact URL.

**Verified the 50km/1-hour buffer parameters directly against Rich's configs**, prompted by the
user asking whether "50km" was ever actually confirmed or just repeated from memory — it hadn't
been: `docs/hotspot_redesign_plan.md`'s own "open questions" section had flagged a real,
never-resolved 50km-vs-500km discrepancy since Phase 3. Checked all 7 config files directly:
`max_downstream_distance_m: 50000` and `max_hours: 1.0`, consistent everywhere, water-only configs
correctly lack the travel-time param and vice versa. 50km confirmed correct; 500km doesn't match
anything in the actual run configuration. Marked resolved in the plan doc. Added the missing
1-hour travel-time figure to the report (previously only said "travel-time buffer" with no
duration) and redeployed.

**Follow-up methodological question, raised by the user, not yet resolved**: is 50km downstream
too generous? Tried to find the actual buffer-generation tool (`workflow_runner.py`) to check
whether "downstream distance" is a flow-path distance along the drainage network or a simple
radial buffer — not present in this repo or in the Docker image (`therealspring/
global_ncp-computational-environment`), lives only on Rich's side, so this couldn't be verified
directly. Inferred from the config (`dem_raster_path` + `subwatershed_vector_path` as required
inputs) that it's very likely flow-path-based, not radial — a simple circular buffer wouldn't need
a DEM or a subwatershed layer. Added as an open, two-part question to the report rather than
asserting an answer: (1) confirm the actual geometry with Rich, (2) independent of that, whether a
tighter distance (e.g. 25km) is worth testing as a sensitivity check, since river confluences
dilute a hotspot's real signal well before 50km in many systems.

**Also clarified a real point of confusion, same session**: the beneficiary categories split into
two independent axes (which pathway — water/access/combined — vs. how many of the 5 services
total, tiers 1-5), which share overlapping vocabulary and are easy to conflate. Documented as a
synthesis matrix in `docs/hotspots_rasters_data_dictionary.md` (new section), including the
explicit fact that Becky's Phase 4 report tested only 3 of the 8 possible categories
(combined_cross, 3+, 4+ — not water-only, access-only, or tiers 1+/2+/5+).

**Colombia country-cut groundwork** (separate thread, `docs/applications/
colombia_capability_portfolio.md` — CLEC congress + Sandra Valenzuela meeting prep, both Aug 12):
confirmed no new pipeline run is needed — `data/processed/tables/regional_subsets/nev_name/
hotspot_area_stats_Colombia.csv` already has Colombia broken out under the current 5-service
definition (11,378 land cells; Pollination 1.83x and Sediment export 1.51x relative intensity are
the standout disproportions; Coastal Risk's 0.12x is flagged as likely a denominator artifact,
needs checking before external use). Computed Colombia-specific beneficiary population reach by
joining this week's `beneficiary_mask_coverage_10km.csv` to Colombia's grid cells (no new zonal
extraction — pure aggregation): combined cross-category reaches 75.8% of Colombia's population
(38.1M), the 4+ tier reaches 35.5% (17.9M) — flagged the same buffer-generosity caveat here too.
Output: `data/processed/tables/colombia_beneficiary_population.csv`.

### 2026-08-07 — Phase 4 built and closed: HDI/Gini/GDP KS test on Rich's beneficiary masks

Picked up Phase 4 (unblocked 2026-08-06): Becky's direct ask to test whether beneficiary areas
(combined cross-category, 3+/4+ nested tiers) are socioeconomically disproportionate vs. the rest
of the landscape. Ended up being two separable pieces of work: a local-toolchain detour (this
machine had never run this project's heavy zonal-extraction step before), then the actual build.

#### Toolchain: local venv couldn't run exactextract/GDAL — traced to a machine change, resolved via Rich's Docker image

`Python_scripts/summary_pipeline_landgrid.py` imports `exactextract` and GDAL's `osgeo` Python
bindings — neither was installed in this machine's `.venv`. Root cause, per the user: this
project's heavy geoprocessing (zonal stats, rasterization) used to run on an external server
("lilling"), not this machine; that server is no longer available for this purpose, so any of that
workload now has to run locally. `pip install exactextract` worked fine (pure wheel, 0.3.0). `pip
install gdal` failed — no prebuilt wheel for Python 3.14 on Windows, and compiling needs MSVC
Build Tools this machine doesn't have.

**Checked the repo's own documentation before improvising further** (`README.md` lines 16-52,
276-300; root `Dockerfile`; `environment.yml`): this is a solved, documented problem. The project
ships a `Dockerfile` (micromamba + conda-forge `geopy311` env: `exactextract=0.2.2`,
`gdal=3.10.3git`, `geopandas=1.0.1`, `rasterio=1.4.3`, plus the `ecoshard`/`taskgraph` deps
`summary_pipeline_landgrid.py` needs) built into a published image,
`therealspring/global_ncp-computational-environment:latest` — Rich's image (`therealspring` /
`springinnovate` in the `Dockerfile`'s `ecoshard` clone). Docker Desktop was installed but the
daemon wasn't running; started it, and the image was **already pulled locally** (`docker images`
shows it dated 2025-05-19) — no download needed. Verified the container's env matches
`environment.yml` exactly (`exactextract 0.2.2`, `gdal 3.10.3`, `geopandas 1.0.1`, `rasterio
1.4.3`). Machine has plenty of headroom for this (22 logical cores / 47.5GB RAM vs. the
container's actual usage of ~1 core / ~1GB) — closing other apps wouldn't have changed anything;
the bottleneck was the script's sequential per-category loop, not resource contention.

**Windows/Git-Bash + Docker gotcha, worth remembering**: `docker run -v "C:/...:/workspace"` fails
with `the working directory 'C:/Program Files/Git/workspace' is invalid` unless
`MSYS_NO_PATHCONV=1` is set first — Git Bash's automatic POSIX-path conversion mangles the
container-side `/workspace` path. Every `docker run` in this session used
`MSYS_NO_PATHCONV=1 docker run ...`.

#### Two real bugs in the raw approach, both caught before trusting any output

1. **`full_raster_extent_union_coverage.tif` ships with `nodata=0`, which is also the "not
   covered" pixel value.** GDAL (and therefore `exact_extract`) silently excludes nodata pixels
   from any statistic — so a naive `mean` zonal stat would only average over the always-1 "valid"
   pixels and return 1.0 for any polygon touching the mask at all, never a true fraction. Fixed by
   wrapping each source raster in a tiny hand-built VRT (`<VRTDataset>`/`<SimpleSource>`, no
   `<NoDataValue>` element) before extraction — both 0 and 1 then count as real data, and `mean`
   becomes the true area-weighted fraction of each polygon covered by the mask. Confirmed via
   `rasterio.open(vrt).nodata is None` and a decimated pixel-value check before trusting it
   further.
2. **`exactextract`'s `GDALRasterSource` needs `osgeo` bindings even when given a path string** —
   traced by reading its `__init__` source, not just the docstring (which implies a path alone is
   enough). Used `RasterioRasterSource` instead (works identically, no `osgeo` dependency); result
   values matched exactly between a local no-`osgeo` test and the same call re-run inside Docker
   (same bbox, same polygon count, same `mean` distribution) — confirms the substitution is a
   pure implementation-detail swap, not a behavior change.

**Grid-identity care, given this project's 4 prior incidents of exactly this failure mode**
(`landgrid_1_clean_enriched_4326.gpkg` has no real ID column; `summary_pipeline_landgrid.py`'s own
`zonal_stats()` falls back to a positional `fid` whenever the input vector lacks a column literally
named `"fid"`). Deliberately did **not** reuse `summary_pipeline_landgrid.py` unmodified — it would
have silently discarded `10k_change_calc.gpkg`'s real `grid_fid` column and substituted a
positional index. Wrote a dedicated script instead
(`Python_scripts/zonal_extract_beneficiary_masks.py`) that explicitly carries `grid_fid` through
`exact_extract`'s `include_cols` and never falls back to row position, and asserted `grid_fid` is
unique before extracting.

#### Full run + validation

Ran all 8 of Rich's beneficiary categories (water, access, combined-cross, and nested tiers
1+ through 5+ — not the 5 ecosystem services, a separate axis entirely) against the full
1,522,073-cell grid inside the Docker container, ~28 minutes total
(`data/processed/tables/beneficiary_mask_coverage_10km.csv`, 12,176,584 rows = 8 × 1,522,073,
confirmed exact). Did all 8 rather than only the 3 Becky asked about because the marginal cost per
raster is the same and it gives 5 extra independent validation targets for free.

**Validated against the already-published, independently-computed fine-resolution area
percentages** (`outputs/tables/hotspot_5service_beneficiary_area_pct.csv`, 2026-08-05 — computed
via `terra::expanse()` directly on the rasters, no grid, no `exactextract`, completely different
code path). Aggregated the 10km zonal results to % of land area (both as area-weighted mean
coverage fraction and as % of cells flagged ≥0.5) and compared: all 8 categories land within
0.04–1.8 percentage points of the independent reference, and the 3 target categories are the
tightest matches (combined-cross −0.5pp, 3+ −0.3pp, 4+ −0.09pp on the mean-coverage measure). This
is a clean pass, not a coincidence — built a hard `stop()` into the KS notebook itself
(`analysis/KS_tests_beneficiary_masks.qmd`, `validate-coverage` chunk) that halts the whole
render if any target category drifts >5pp from this reference, so a future grid-identity
regression would fail loudly rather than silently producing a wrong headline number.

#### KS test: new notebook, one real bug caught before results were final

Built `analysis/KS_tests_beneficiary_masks.qmd`, reusing `run_ks_hot_vs_non()` from
`R/ks_hotspots.R` (the same function `analysis/KS_tests_hotspots.qmd` uses for the WHO chapter) —
substituted the grouping variable: instead of a service's extreme-change hotspot definition, it's
the beneficiary coverage fraction thresholded at ≥0.5 (majority rule) into an in-mask/out-of-mask
flag, one row per category × grid cell. Comparison is **direct in-mask vs. everywhere else**, not
the WHO chapter's "median background" tail comparison — there's no equivalent "typical change"
concept for a spatial buffer-membership flag, and Becky's question was simply mask vs. outside.

First render aborted on a real bug in the validation chunk: `mutate(diff_meancov_vs_ref =
mean_coverage_pct - ref_pct)` referenced a column `ref_pct` that didn't exist (the join actually
produced `ref_pct_of_land`, per an earlier `select()` rename) — R silently resolved `ref_pct` to
the outer-scope *data frame* of the same name instead of erroring on a missing column, giving
`non-numeric argument to binary operator`. Fixed the column reference; re-ran clean. PDF output
also failed (no TinyTeX on this machine) — rendered to HTML instead, R-chunk execution and outputs
identical either way.

**Headline results** (`data/processed/tables/ks_results_beneficiary_masks.csv`,
`outputs/plots/ks_beneficiary_masks/`): beneficiary-mask areas (combined-cross, 3+, 4+) are
strongly and significantly **wealthier and more populated** than the rest of the landscape —
GDP and population both show Cliff's δ ≈ 0.48–0.53 (large effect), p_adj (two-sided, BH-adjusted)
effectively 0 for every category. **Gini** shows a smaller but real effect in the same direction
(δ ≈ 0.22–0.33, small–medium) — beneficiary areas skew toward *more* unequal regions, not less.
**p_adj is effectively 0 (highly significant) for all 12 service × variable combinations tested,
without exception** — Gini's actual pattern is that *effect size* grows with tier exclusivity
(δ 0.22 → 0.22 → 0.33 from combined-cross → 3+ → 4+), not that significance weakens.

*Caught and corrected before this went any further*: an earlier draft of this entry (and of the
plan-doc Phase 4 summary, and of what was told to the user) claimed the 4+ Gini result was
"borderline, p_adj=0.057" — a genuine misread of the CSV column order. That 0.057 value is
`p_hot_greater_adj`, a stricter *one-sided* directional test (specifically: is beneficiary-Gini
stochastically greater across the *entire* distribution, not just on average), not `p_adj`, the
primary two-sided significance measure used everywhere else in this analysis. Re-verified all
three significance columns (`p_adj`, `p_hot_greater_adj`, `p_hot_less_adj`) directly against the
CSV before writing anything Becky-facing. **HDI** shows the weakest signal by far (δ ≈ 0.04–0.10,
negligible–small) — beneficiary status isn't strongly tied to human development level in either
direction. One technical oddity worth a future look, not disqualifying today's results: the
`tier_4plus`/HDI row's one-sided p-values (`p_hot_greater_adj`, `p_hot_less_adj`) both came out ~0,
unlike every other row where they're complementary (~1/~0) — plausible given KS's sensitivity to
distribution *shape* (not just location) at very large n (n_non=917,690), consistent with the
row's unusually large D (0.124) paired with a small Cliff's delta (0.035, i.e. a shape difference
more than a location shift), but flagging since it doesn't match the pattern of the other 11 rows.

**Files added**: `Python_scripts/zonal_extract_beneficiary_masks.py`,
`analysis/KS_tests_beneficiary_masks.qmd`, `data/processed/tables/beneficiary_mask_coverage_10km.csv`
(gitignored), `data/processed/tables/ks_results_beneficiary_masks.csv` (gitignored),
`outputs/plots/ks_beneficiary_masks/**`. **Files changed**: `docs/hotspot_redesign_plan.md`
(Phase 4 marked done with results summary).

### 2026-08-06 (cont. 2 — forked a real 5-service paper draft, rewrote its abstract with verified numbers)

Per the user: the staging-doc approach (`docs/paper_5service_methodology_staging.md`) felt awkward — candidate paragraphs that never actually land anywhere. Forked a real second paper draft instead: `docs/manuscript/paper_draft_5service.qmd` (`cp` of `paper_draft.qmd`), original left completely untouched. Added a status callout at the top marking it in-progress and listing exactly what's updated vs. still-copied-8-service-text.

**Rewrote the abstract with real 5-service numbers, not reused 8-service ones** — checked each figure against actual verified data rather than adapting the old text by feel:
- Hotspot count/land share: 189,927 cells, 13.84% of the 1,372,621-cell land base (`189927/1372621`, computed fresh) — genuinely different from the 8-service paper's 252,215/18.4%.
- Water/access/combined-cross land-area breakdown (8.1%/9.3%/3.5%): from `outputs/tables/hotspot_5service_category_shares_pct.csv` (2026-08-05).
- Beneficiary exposure (3.1B/7.1B/4.1B): from Rich's rerun, `outputs/tables/hotspot_5service_beneficiary_area_pct.csv` — deliberately used the water-downstream, access-travel-time, and combined-union numbers respectively, matching each category's actual buffer type.
- Regional/income disparity (LAC 1.55×, East Asia-Pacific 1.20×, lower-middle-income 1.69× vs. high-income OECD): computed fresh from `data/processed/tables/hotspot_area_stats.csv` (the 2026-07-30 subregional rerun), averaging `relative_intensity` across the 5 services per group — **not** the old 8-service paper's 1.6× figure, which doesn't hold under the new service set (real answer is 1.69×, close but not identical).

**Explicitly flagged as pending, not silently reused or omitted**: land-cover-conversion attribution and the Phase 4 Gini/HDI/GDP test haven't been rerun under the 5-service definition at all — the abstract has a bracketed `[PENDING]` marker instead of either the old 8-service attribution numbers (which would be wrong here) or no mention at all (which would hide a real gap).

**Not done**: body chapters past the abstract are still an unmodified copy of the 8-service text — real work still ahead once Phase 4 lands.

**Files added**: `docs/manuscript/paper_draft_5service.qmd`. **Files changed**: `docs/hotspot_redesign_plan.md` (pending-edits item 3 updated).

**Follow-up fixes, same session**: rendered the new file (`quarto render ... --to html`, clean, no errors — `paper_draft_5service.html`); removed the "migrated from 8 to 5 services" narrative from the abstract (reads as having always assessed 5 services, not a redesign-in-progress story — that framing stays in the status callout, not the abstract); then rewrote the abstract's prose register per user feedback ("too baroque, too Claude-y" compared to the AGU abstract) — cut nested parentheticals, the bold `[PENDING]` bracket flag, and the semicolon-heavy triple-parallel sentence structure, matching the AGU abstract's plainer, more declarative cadence. Same numbers throughout, just tighter prose.

### 2026-08-06 (cont.)

#### Pct/abs ambiguity resolved: Rich confirmed pct used for all 7 categories, verified against his actual config files

Rich replied on Slack: "I used the combined_cross_pct.tif files... I think this pipeline I have just treats these as a mask, so I'm not sure there would be a difference one way or the other if the pixels are all still defined in the same spot," and attached his actual analysis configs (`data/jeronimo_2026_07_beneficiaries_analysis_configs/*.yaml`, 7 files).

**Didn't just take the chat message at face value** — read all 7 YAML configs directly. Every `condition_raster_path` across all 7 categories (water, access, combined-cross, and all 5 nested hotspot-count tiers) points to a `_pct` file (`count_water_pct.tif`, `count_access_pct.tif`, `combined_cross_pct.tif`, `hotspot_count_pct_2026_07_29_18_49_00.tif`) — **zero references to any `_abs` file**. This fully resolves the ambiguity flagged 2026-08-05: Rich used the `pct` metric consistently for everything, matching the user's stated intent (pct for main text, abs as annex benchmark) and matching what this repo's own percent-area analysis already used. No rework needed anywhere — everything already lines up.

**Also verified while in there**: the buffer logic in each config matches Becky's original spec exactly — water-only config has only a `downstream_50k` mask (no travel-time), access-only config has only a `within_travel_time` mask (no downstream), and both the combined-cross-category and all 5 nested hotspot-count tiers correctly include both mask types combined with OR logic. Threshold expressions on the hotspot-count tiers (`value > 0` through `value > 4` for the 1+ through 5+ tiers) correctly match the nested tier definitions.

**Files changed**: none (verification only). Closes the open item from the 2026-08-05 entry below.

### 2026-08-05 (cont. — Becky's follow-up questions on the beneficiary numbers)

#### Discovered a real pct/abs ambiguity in what was sent to Rich; computed beneficiary-raster area percentages

Becky replied to the percent-area table + charts with three things: (1) asked which metric (pct/abs) was actually passed to Rich, since she wants "the one we keep in the main" to match; (2) asked for % of land area (not just population) for all the beneficiary rasters, expecting values between the hotspot-footprint % and the population %, as a sanity check; (3) asked to start the HDI/Gini/GDP disproportionality test using the union coverage masks (combined + 3+/4+ tiers).

**(1) Metric ambiguity, real and unresolved.** Checked the repo for a record of which metric was transferred to Rich — no definitive answer findable (both `pct` and `abs` rasters exist locally, and `docs/hotspot_5service_rasters_README.md` documents both being generated without specifying which subset was actually transferred). User then pulled up a screenshot of the actual shared Google Drive folder: **both `pct` and `abs` versions were uploaded on 2026-07-28, side by side, with no label distinguishing them**, and Rich's returned output folder names don't record which one his pipeline read either. Also found the same Drive folder still has leftover single-service rasters from 2026-05-27 (the pre-redesign 8-service era — `Sed_export_pct.tif`, `N_Ret_Ratio_pct.tif`, etc.), unrelated to this handoff and a further source of potential confusion. **Cannot confirm which metric Rich's pipeline actually used — this needs to go back to Rich directly, not get asserted to Becky.**

**Wrote a Rich-facing data dictionary**, `docs/hotspots_rasters_data_dictionary_for_rich.md` — trims the internal README down to what Rich needs: which 8 files are the current Jul 28 5-service batch, which files to ignore (the May 27 leftovers), an explicit ask for him to confirm which metric his pipeline read per output folder, and a recommendation to clean up the shared folder going forward so this doesn't recur.

**(2) Beneficiary-raster area percentages, computed** — `outputs/tables/hotspot_5service_beneficiary_area_pct.csv`. Used `terra::expanse(byValue=TRUE)` on each `*_coverage.tif` (fine-resolution, ~30 arcsec, lon/lat WGS84, so this is true latitude-weighted area, not naive pixel counting) across all 22 coverage rasters (union/downstream/travel-time, wherever each exists per category) across the 7 categories Rich returned, ~8-10s per file. Denominator: same 1,372,621-cell / 137,262,100 km² land base used throughout this analysis, so these are directly comparable to the already-reported hotspot-footprint percentages. **Confirms Becky's prediction exactly** — a clean, monotonic funnel at every tier: hotspot footprint % → beneficiary buffer area % (3-7x larger) → population % (larger still, meaning the buffers/hotspots disproportionately reach population-dense areas on top of already covering a large area). E.g. water: 8.07% → 30.93% → ~39% of population; access: 9.26% → 36.65% → ~89%; combined: 3.50% → 23.56% → ~51%; nested 1+ through 5+: 13.84%→56.80%→~93% down to 0.001%→0.03%→~0.07%.

**(3) HDI/Gini/GDP disproportionality test (Phase 4)** — scoped, not started. Plan: zonal-extract Rich's fine-resolution union coverage masks (combined-cross-category + 3+/4+ tiers) onto the existing 10km analysis grid (reusing the same `exactextract`-based approach `summary_pipeline_landgrid.py` already uses for everything else), producing a per-fid "inside beneficiary mask" flag, then reuse the existing `analysis/KS_tests_hotspots.qmd` machinery (already built for the WHO chapter, already tests HDI/Gini/GDP/population via two-sample KS + Cliff's Delta) with that flag substituted for the old hotspot definition. Deliberately not started this session — real build, not a quick turnaround, and this session was already substantial.

**Drafted (not sent) a response to Becky** covering all three points — saved to scratchpad, not the repo, for the user to review/edit before sending.

**Files added**: `docs/hotspots_rasters_data_dictionary_for_rich.md`, `outputs/tables/hotspot_5service_beneficiary_area_pct.csv`.

### 2026-08-05

#### Rich's beneficiary data identified + Becky's low-hanging-fruit ask (max hotspot count, percent-area table)

Rich's water-hotspot/access-hotspot beneficiary rerun (Phase 3) is back — user placed it in `data/processed/hotspots_5service/rasters_5_var/` (the `rasters/` folder that was originally handed to Rich was renamed to `rasters_old/` to make room). 7 output subfolders, one per category: `water_overlap_downstream`, `access_overlap_travel_time`, `combined_cross_category`, and nested `hotspot_count_{1,2,3,4,5}plus` tiers (generalizing the old 8-service 2/3/4/all scheme to 5 tiers). Each has population/coverage `.tif`s (only the relevant buffer type per category — water folder has no travel-time raster, access folder has no downstream raster, matching spec exactly) plus one summary CSV.

**Pulled headline numbers from Rich's summary CSVs** (union of downstream+travel-time buffers, world pop ~8B): 1+ services = 7.38B, 2+ = 4.59B, 3+ = 2.26B, 4+ = 872M, 5+ (all 5 at once) = 5.86M; water-only = 3.09B, access-only = 7.05B, combined cross-category = 4.10B. **Flagged to user**: the 7.38B "1+" figure is the same shape of number as the already-known "96%/10B+" caveat in the plan doc — generous 50km/travel-time buffers around ~190K dispersed hotspot cells will cover nearly the whole inhabited planet almost by construction, so it's not a meaningful headline claim on its own. The 4+/5+ tiers are the actually defensible, striking numbers.

**Becky's Slack ask, addressed**: she asked whether `hotspot_count_5plus_beneficiaries` existing is a bug ("shouldn't max be 5?"), and separately asked for percent-area (or percent of valid pixels) per hotspot category. Verified directly against the source rasters (`data/processed/hotspots_5service/rasters_old/hotspot_count_{pct,abs}.tif`): value range is exactly 1–5 for both metrics, confirming Jerónimo's Slack answer to her was correct — max is 5, and only 14 cells (pct metric) / 25 cells (abs metric) actually hit it.

**Produced the percent-area table she asked for**, computed directly from the source gpkg (not the raster, to avoid rounding): `outputs/tables/hotspot_5service_category_shares_{pct,abs}.csv` (renamed 2026-08-05 from `..._category_pct_{pct,abs}.csv` — the original name had "pct" doing double duty, once for "percent-area table" and once for the `pct`-vs-`abs` hotspot metric, which read as confusing/suspicious; "shares" now means the table, the trailing `_pct`/`_abs` still means the metric) — n_cells, % of the 189,927/191,759 hotspot cells, and % of total valid land area (1,372,621 cells — same exclusion definition `extract_hotspots_5service.R` uses, and it matches the plan doc's already-independently-verified 1,372,621 figure exactly) for: water overlap, access overlap, combined cross-category, and both the exact-tier and nested-tier (1+ through 5+) breakdowns.

**Two simple charts** (not wired into the paper/book, just for the Slack reply): `outputs/plots/becky_5service_category_pct.png` (water/access/combined, % of land area) and `outputs/plots/becky_5service_tier_pct.png` (exact 1–5 tier breakdown, % of land area). Kept deliberately simple (single-color horizontal bars, direct value labels, no legend needed) to match "not too fancy" — consistent with this repo's existing hotspot-plot convention (`#E83737` red fill) rather than a new style.

**Files added**: `outputs/tables/hotspot_5service_category_shares_{pct,abs}.csv`, `outputs/plots/becky_5service_category_pct.png`, `outputs/plots/becky_5service_tier_pct.png`. Nothing in `R/`, `analysis/`, or `scripts/` changed — this was pure analysis/reporting off already-existing outputs.

**AGU abstract, separately**: deadline is today, 23:59 EDT (22:59 Bogotá). Decided to adapt the paper's existing, already-vetted 8-service abstract (`docs/manuscript/paper_draft.qmd`, lines 33-39: 252,215 hotspot cells, 3.1B/7.6B beneficiaries, 34.2%/65.8% attribution gap) to AGU format, rather than rushing the still-in-progress 5-service numbers (Phase 4 KS/Gini not done yet) into a submission under deadline pressure. Not started — need AGU session/word-limit details from the user's portal first. User has a prior AGU ID/submission to reuse.

### 2026-08-03

#### Housekeeping item 2: documented the `combos` mechanism + added `derive_cross_combo()` helper

Second housekeeping item from the redesign plan, picked up right after item 3 (script consolidation, prior entry). Scoped down deliberately to just the two deliverables the plan item actually named — **not** the 7-file service-config consolidation the same plan item also mentioned bundling in, which is separate, larger, and riskier (touches config in 7 places at once, the same failure class as past grid-ID incidents), and wasn't what was picked up this session.

**Docs**: added a "Multi-Service Overlap Combos" subsection to `docs/methodology.md` (under "Change Metrics & Hotspot Definition") explaining `HOTS_CFG$combos` — how a named list of service vectors becomes a `count_<name>` column automatically via `extract_hotspots()`, using the 5-service redesign's water/access split as the worked example.

**Helper function**: added `derive_cross_combo(data, combo_names, new_col = NULL)` to `R/get_hotspots.R` — takes two or more combo names (matching `count_<name>` columns already produced by `extract_hotspots()`) and returns an AND-derived 0/1 column, generalizing the `combined_cross = count_water > 0 & count_access > 0` pattern that `scripts/extract_hotspots_5service.R` wrote out by hand. Errors clearly if a requested combo name has no matching count column. Smoke-tested against a 6-row toy data frame with known expected output — matched exactly.

**Documentation-generation near-miss, worth flagging**: ran `devtools::document()` to generate the new function's `.Rd` file and NAMESPACE entry, as this package normally expects. It deleted 19 unrelated `.Rd` files and rewrote large parts of `NAMESPACE` — turns out this package's `NAMESPACE` already has real, pre-existing entries for functions that don't exist in current source (`identify_hotspots`, `make_hotspots`, `align_rasters`, and even non-function garbage like `export("(simple,")` — likely a mangled roxygen comment somewhere), consistent with the "Objects listed as exports, but not present in namespace" warning that's been showing up on every `devtools::load_all()` call this whole redesign (visible in every script's stderr, never actually investigated). Running `document()` would have "fixed" this by syncing everything to current source — but that's a much bigger, unreviewed change than today's task called for. **Reverted** (`git checkout -- man/ NAMESPACE`, then manually deleted the stray newly-generated `.Rd` files it left behind) and instead added only the one needed line to `NAMESPACE` by hand (`export(derive_cross_combo)`) and kept only `man/derive_cross_combo.Rd` (plus `man/filter_multidim.Rd`, which turned out to have no doc file at all despite already being `@export`-tagged and in active use since Phase 2 — a small, directly-related gap worth keeping fixed, unlike the other 19). **The broader NAMESPACE/`.Rd` drift is real and still unresolved** — flagged here rather than fixed, since fixing it properly needs its own reviewed pass, not a side effect of adding one function.

**Files changed**: `docs/methodology.md`, `R/get_hotspots.R` (new `derive_cross_combo()` function), `NAMESPACE` (one line), `man/derive_cross_combo.Rd` (new), `man/filter_multidim.Rd` (new, pre-existing gap).

### 2026-07-30 (cont.)

#### Housekeeping item 3 (script consolidation): audited all standalone scripts — found a documentation gap, not dead code

Picked up the "script consolidation/cleanup" housekeeping item from the redesign plan. Inventoried every standalone script in `scripts/`, `scripts/mapping/`, and `Python_scripts/` against the two READMEs and git history.

**Finding: no genuinely dead/one-off scripts left to remove.** Every script that wasn't already documented traced back to a real, deliberate fix or deliverable when checked against its own header comment and commit history — the 5-service redesign scripts (`extract_hotspots_5service.R`, `gdal_rasterize_hotspots_5service.R`, `make_5service_overlap_maps.R`, `make_5service_overlap_summary.R`), the grid-crosswalk and attribution-union fixes (`build_lc_grid_fid_crosswalk.R`, `compute_attribution_true_union.R`, `make_lcc_true_overlap_map.R`), and the IDB-WWF workshop deck maps (`make_global_thumbnail_maps.R`, `make_lac_critical_assets_map.R`, `make_lac_hotspot_map.R`). The `check_*.R` ad-hoc scratch scripts the plan doc mentioned no longer exist in the repo at all. So this pass was really a **documentation gap**, not a deletion job — `scripts/README.md` just hadn't been updated to list ~12 scripts that had already earned their keep. Fixed: added all of them to the appropriate tables.

**Found and fixed a stale/misleading README entry, unrelated to the above**: `scripts/README.md`'s "archive/" table listed `audit_claims.R` and `export_reclass_table.R` as archived ("one-off, results already produced") — but neither file is actually in `scripts/archive/`; both are active files in the top-level `scripts/` folder, correctly documented in that same README's "Validation" and "Reference data" tables above. Looks like a copy-paste leftover from an earlier README restructure. Removed the two stale rows from the archive table.

**Found and removed an unrelated stray file**: `Python_scripts/photo_processing.py` was a real-estate photo batch-resizer (HEIC→JPG via Pillow/pillow-heif) with no connection to this project at all, committed 2026-05-14, referenced nowhere. Removed (`git rm`) per user confirmation.

**Files changed**: `scripts/README.md` (removed 2 stale rows, added 12 missing entries). **Removed**: `Python_scripts/photo_processing.py`.

### 2026-07-30

#### Subregional hotspot reruns: fixed a syntax bug + stale 8-service config in `hotspot_synthesis.qmd`, reran area/hotness stats + 219 regional subset CSVs on the 5-service definition

Picked up the "not started yet" subregional (income/region/biome/country) hotspot rerun item from the redesign plan. This is separate infrastructure from Phase 1/2's global 5-service extraction — `analysis/hotspot_synthesis.qmd` + `scripts/generate_regional_subsets.R`, built in Phase 2 (2026-06-18) to break hotspot area/share/relative-intensity and multi-service "hotness" stats down by `income_grp`, `region_wb`, `WWF_biome`, and `nev_name` (country) into 219 CSVs. It had never been touched since the 8→5 service redesign started.

**Found, while reading the file to update it: a real syntax bug.** `HOTS_CFG` (line 233) had a stray literal `<` character before the `groupings` entry — `<   groupings = c(...)`. This is invalid R and would have thrown a syntax error the moment the chunk actually ran. It survived undetected only because every heavy chunk in this notebook has been `eval: false` since June — nothing had actually executed the config block since whenever the typo was introduced.

**Also found: the notebook's `HOTS_CFG` was still the old 8-service definition** (`loss` included `N_Ret_Ratio`, `Sed_Ret_Ratio`, `C_Risk_Red_Ratio` — the 3 services dropped in the redesign). Updated it to match the already-validated live 5-service config from `scripts/extract_hotspots_5service.R` exactly: `loss = c("Nature_Access","Pollination")`, `gain = c("Sed_export","N_export","C_Risk")`, `combos = list(water = c("N_export","Sed_export"), access = c("Nature_Access","Pollination","C_Risk"))`, `svc_order` trimmed to the 5 kept services. Also updated the `config-summary` display chunk's combo labels (`Degradation Combo`/`Recovery Combo` → `Water Combo`/`Access Combo`) to match.

Confirmed `combos` isn't actually consumed by this notebook's `calc-overlap`/"hotness" stats (those are driven purely by `HOTS_CFG$loss`/`gain` via `extract_hotspots()`'s per-fid hotspot count, independent of the `combos` argument) — so the combo rename is a correctness/consistency fix for the config-summary table, not something that changes any computed number.

**User chose a minimal rerun scope**: area/coverage/share stats + multi-service hotness only, skipping the HDI/GDP/Gini population-exposure chunk (the heaviest one, previously deferred for the same reason, and conceptually distinct from Rich's pending buffered-beneficiary rerun rather than a prerequisite for it). Set `eval: true` on `load-data`, `global-filters`, `config-summary`, `extract-hotspots`, `calc-intensity`, `plot-intensity`, `calc-overlap`, `plot-overlap`; left `calc-pop-exposure`/`plot-pop-exposure` at `eval: false`.

**Rendered successfully** (`quarto render analysis/hotspot_synthesis.qmd`, ~750MB grid + 147MB cached `plt_long.rds`, backgrounded). Verified `hotspot_area_stats.csv` now lists exactly the 5 kept services (`C_Risk, N_export, Nature_Access, Pollination, Sed_export`) with no trace of the dropped retention/ratio services. Reran `scripts/generate_regional_subsets.R` — regenerated all 219 CSVs under `data/processed/tables/regional_subsets/` in place (4 income groups, 7 regions, 14 biomes, 186 countries), all with fresh mtimes.

**Follow-up (same day): traced the 1,302,099 figure fully — it is correct, not the same issue as the book-script bug.** Reproduced it exactly, independently, cell-for-cell: full grid (1,522,073) → drop `income_grp == "2. High income: nonOECD"` per the notebook's own deliberate `global-filters` chunk (1,355,270 remain) → drop cells with no valid `income_grp` at all (1,302,099). Exact match, no fudging. The earlier flag (that this matches the "wrong, not just old" figure from `extract_book_data_fills.py`'s `/8` average) turned out to be a false alarm: that script's bug diagnosis assumed `n_total` varies per service in `hotspot_area_stats.csv` (Coastal Risk ~80K vs ~1.3M), but in the actual table this notebook produces, `n_total` is constant across all services within a group — so dividing an 8x-repeated sum by 8 was never actually wrong for *this* table, even though the diagnosis attached to it was. **1,372,621** (total valid land, no classification required) and **1,302,099** (land with a valid, non-ambiguous income classification) are two different, both-legitimate denominators for two different questions, not one true number with a stale impostor. No fix needed; corrected here so this doesn't get re-flagged as a live bug next time someone reads this file.

**Files changed**: `analysis/hotspot_synthesis.qmd` (config fix + eval flags). **Regenerated** (gitignored, not tracked): `data/processed/tables/hotspot_area_stats.csv`, `hotspot_multiservice_stats.csv`, `data/processed/tables/regional_subsets/**` (219 files), `hotspot_synthesis.html`, associated `intensity*`/`hotness_*` plots under `out_plots()`.

### 2026-07-29 (evening close-out, cont.)

#### Native change figure: fixed basemap being hidden by opaque near-zero fill (recurrence of a previously-solved problem)

User flagged the basemap (added earlier this session) was still invisible in several panels — correctly diagnosed as opaque near-white fill at near-zero values painting over the entire land footprint, not a basemap-loading problem. This is the exact same issue already solved once before, in `scripts/mapping/make_paper_supplement_maps.py` (the Becky-requested per-service paper supplement, session 18): "opaque near-white 'no change' pixels cover the entire land footprint and the map reads as colored blobs floating with no geographic reference at all."

**Fix, ported directly from that script's approach**: fade alpha toward 0 for near-zero values instead of leaving them fully opaque. `fade_threshold <- 0.08 * limits[2]` (8% of the symmetric 1st/99th-percentile limit, same fraction the Python version uses), `alpha <- pmin(abs(value) / fade_threshold, 1)`, mapped via `aes(fill = value, alpha = alpha) + scale_alpha_identity(guide = "none")`. Only cells with a real, non-trivial change stay opaque; true no-data cells were already absent from the plotted data frame (terra's `as.data.frame()` drops NA by default), so the two together now make "no data" and "no meaningful change" both correctly show the gray basemap, while real change is clearly visible against it.

**Verified**: regenerated and visually confirmed — country-border basemap now visible through what were previously large washed-out near-white regions (N Ret Ratio, Sed Ret Ratio, Pollination, Nature Access most affected, being globally-computed indices with many small/near-zero values). Coastal Risk panels remain mostly gray, correctly (sparse real data, not a rendering artifact — see earlier entry).

**Lesson for future map scripts in this project**: any diverging color scale plotted over a basemap needs this same alpha-fade treatment, not just a `na.value` fix for genuinely-missing data — a solid `mid = "white"` color at the scale's zero point is exactly as opaque as any other mapped color and will hide a basemap just as completely. Worth checking `make_faceted_maps.R` and any other basemap+diverging-scale script for the same latent issue if picked up again.

### 2026-07-29 (evening close-out)

#### Native change figure: 4-row layout (Becky's suggestion) + goods/damages direction re-verified

Two small follow-ups to the Phase 5.1/5.2 native-change-figure work above, same session:

**Layout**: switched from 5 rows (Pollination and Nature Access each full-width with an empty spacer) to 4 rows (Pollination and Nature Access sharing the 4th row) — per Becky's own recommendation. `row_pairs` in `scripts/mapping/make_native_change_figure.R` updated; composite `ggsave` height reduced 36in -> 29in to match. **Regenerated and visually confirmed** (background run completed after session sign-off, checked before close-out): 4 rows render correctly, Pollination/Nature Access share row 4 side-by-side, no wasted space, colors/direction unchanged from the 5-row version.

**Direction check, explicitly re-verified against the live pipeline config, not just cross-script consistency**: confirmed `goods`/`damages` in the new script match `analysis/hotspot_extraction.qmd`'s actual `HOTS_CFG$loss`/`HOTS_CFG$gain` (the config that drives real hotspot extraction) exactly — same 5-vs-3 split, same members (`loss` = `Nature_Access, Pollination, N_Ret_Ratio, Sed_Ret_Ratio, C_Risk_Red_Ratio` = "goods"; `gain` = `Sed_export, N_export, C_Risk` = "damages"). Worth the extra check specifically because this is the same class of bug that made `compare_exposure_serviceshed.R` (deleted earlier this session) wrong — its `loss_services`/`gain_services` were this exact list, inverted.

### 2026-07-29 (later still, cont. 2)

#### Phase 5.1 + 5.2: new native-10km, paired export/retention change figure

New script `scripts/mapping/make_native_change_figure.R`, replacing (as a new, separate deliverable — see below) the dissolved-by-biome `map_biome_{pct,abs}.png` with a figure plotted directly off `10k_change_calc.gpkg`'s native grid, no dissolve step anywhere. Rows paired export next to retention/reduction-ratio for the 3 services that have one (Nitrogen, Sediment, Coastal), ordered nitrogen → sediment → coastal → pollination → nature access; Pollination and Nature Access (no retention analog — confirmed no such column was ever modeled for either) get single full-width-equivalent rows.

**Performance**: a first-pass vector `geom_sf` render of one full 1.37M-cell panel took ~145s (98s of that in the render/save step alone) — infeasible for an 8-service, pct+abs figure (~20 min). Switched to rasterizing each column to 10km via `terra::rasterize` (same resolution `gdal_rasterize_hotspots.sh` already uses for Rich's deliverables, just done in-process via terra instead of the CLI, since `gdal_rasterize`/`ogr2ogr` aren't on this machine's PATH — terra bundles its own GDAL) and plotting with `geom_tile`. Cut per-panel cost to ~15s after a one-time ~65s load/filter/transform, confirmed visually identical to the vector version on a test panel. ~5x faster overall.

**Two rounds of user feedback, both addressed**:
1. No basemap meant sparse-data services (Coastal Risk: 80,040 of 1,522,073 cells — a narrow shoreline-only phenomenon, not a bug, same root cause already documented for the access-map split) rendered as near-blank whitespace, indistinguishable from broken. Added the same custom basemap `make_5service_overlap_maps.R` uses (`cartographic_ee_r264_correspondence.gpkg`, EPSG:8857, gray fill) under every panel.
2. The combined composite was compressing real detail: the raster itself carries ~3406×1667 pixels of native resolution per panel, but the initial composite (12"×22"@300dpi, 2 columns) was rendering each panel well below that. Bumped the composite to 20"×36"@300dpi (overview only, still not 1:1), and added standalone per-service panel exports (`outputs/maps/native10km_panels/`) at exact native raster resolution (1 raster cell = 1 output pixel) so any single service can be zoomed into at full detail.

**User question, confirmed correct**: whether the "sign flip from averaging heterogeneous cells" failure mode (distinct from the Mongolia geographic-bleed artifact, though same root cause) could occur in this new figure. Confirmed no: the old biome/region/income/country maps compute a **mean value per polygon** before dissolving (`generate_map_gpkgs.py` reads `mean_val`/`sym_pct_change` from `{grp}_map_data.csv`), which can sign-flip if a group's few large increases outweigh many small decreases. The native10km figure has no aggregation step anywhere — each pixel is one grid cell's own value; `terra::rasterize()`'s default polygon behavior is "last write wins" (pick one), not "average," so no cross-cell blending occurs at any point in this pipeline.

**Deliberately not wired into paper_draft.qmd or the book chapters** — new output filenames (`map_native10km_{pct,abs}.png`), not a silent overwrite of `map_biome_{pct,abs}.png`, so the old biome-dissolved map (and its "WWF Biome" captions) stay valid and unchanged until the paper/book are ready to switch — per the standing instruction to leave the paper alone until this redesign settles. Swapping the reference is a pending paper edit, added to the existing tracked list.

**Files added**: `scripts/mapping/make_native_change_figure.R`. **Outputs**: `outputs/maps/map_native10km_{pct,abs}.png`, `outputs/maps/native10km_panels/*.png` (16 files, one per service × metric).

### 2026-07-29 (later still, cont.)

#### Removed dead `compare_exposure_serviceshed.R`; generalized the live multiplier-effect scripts to auto-discover new beneficiary categories (prep for Rich's water/access rerun)

While waiting on Rich's beneficiary rerun, started preparing the pipeline to absorb his upcoming water-hotspot/access-hotspot beneficiary folders (Phase 3) without needing another code edit when they land.

**Found `analysis/compare_exposure_serviceshed.R` is dead code, not a fast-follow candidate.** It reads a per-folder CSV (`list.files(folder, pattern = "\\.csv$")`) that hasn't existed since `Python_scripts/extraction_script.py` replaced that layout — the 4 `hotspot_beneficiaries/` folders now contain only Rich's raw `.tif` rasters, zonal-stats'd by `extraction_script.py` into one compiled file, `outputs/tables/exposure_comparison_compiled.csv`. That compiled file is what `analysis/plot_multiplier_effect.R` (the real, live Figure 9 generator, already carrying the 2026-07-28 Global-row fix) actually reads. `compare_exposure_serviceshed.R`'s own output (`exposure_comparison.csv`, no `_compiled`) is referenced nowhere else in the repo, and git history shows only the single commit that introduced it. Also confirmed, in passing: the plan doc's previously-flagged inverted `loss_services`/`gain_services` bug in this file is real (both the damage-service trio and the retention-ratio trio were assigned to the wrong extraction branch, backwards relative to the canonical goods/damages direction used everywhere else) but moot, since the code path never actually executes against current data. **Deleted** (`git rm`), per user confirmation after checking for references.

**Generalized the two scripts that are actually live:**
- `Python_scripts/extraction_script.py`: `subfolders` was a hardcoded list of the 4 known folder names. Changed to auto-discover (`sorted(p.name for p in base_dir.iterdir() if p.is_dir())`) — confirmed byte-for-byte equivalent to the old hardcoded list against current data. New folders Rich drops in (water hotspot, access hotspot) will be picked up automatically on the next run, no code change needed.
- `analysis/plot_multiplier_effect.R`'s compound (global) dumbbell section: replaced the hardcoded `overlap_mapping`/`Label` `case_when()` (both assumed exactly the 4 nested "N or more services" tiers) with a `category_defs` list mapping each `overlap_category` to a label, a source gpkg (`8service` = existing `hotspots_global_pct.gpkg`/`hotspot_count`, `5service` = `hotspots_global_5service_pct.gpkg`/`count_water`/`count_access`/`combined_cross`), and a threshold. The loop now iterates over whatever categories are actually present in `exposure_comparison_compiled.csv`; a category with no matching `category_defs` entry is skipped with a message rather than guessed at or silently dropped. Pre-populated `category_defs` with entries for `"water hotspot"`, `"access hotspot"`, and `"combined hotspot"` using the 5-service gpkg's `count_water`/`count_access`/`combined_cross` columns — exact folder-name strings are a guess pending Rich's actual naming, so these may need a one-line key rename once his folders arrive, but the rest of the logic needs no further changes.

**Regression-verified**: reran `plot_multiplier_effect.R` end-to-end after the refactor — `git diff --stat outputs/` showed **zero byte differences** across every regenerated PNG and CSV (including `downstream_exposure_dumbbell_compound.png`, i.e. Figure 9 itself), confirming the generalization is a clean no-op for the 4 categories that exist today.

**Files changed**: `Python_scripts/extraction_script.py`, `analysis/plot_multiplier_effect.R`. **Removed**: `analysis/compare_exposure_serviceshed.R`.

### 2026-07-29 (later still)

#### Phase 5.3: mangrove/biome row-offset check — ruled out, not a bug

Tested the last open suspect from the redesign plan doc: whether `Python_scripts/build_master_grid.py`/`enrich_grid.py`'s positional-index merge (`left_index=True, right_index=True`, after a `duplicated(keep='first')` drop, following `gpd.sjoin` against `Biome.gpkg`) introduces a row-offset that could explain the mangrove/Coastal-Risk hotspot-vs-biome-change-map mismatch Becky flagged (a separate issue from the already-explained Mongolia artifact — see the entry below).

**Code review first**: `gpd.sjoin` preserves the left GeoDataFrame's original index values in its output (not a fresh `RangeIndex`) — duplicate-index rows only occur when a point matches more than one right-side polygon, and the subsequent `.duplicated(keep='first')` correctly dedupes against that preserved index. The final `grid.merge(enriched_grid[new_cols], left_index=True, right_index=True, how="left")` is a genuine index-*label* join, not a positional `cbind`/`concat` — structurally not the same failure pattern as the historical `seq_len()`/positional-merge bugs in this project.

**Empirical verification, not just code review**: pulled all 3,423 `WWF_biome == 'Mangroves'` cells from `10k_change_calc.gpkg`, took each cell's representative point (same method the pipeline uses), and ran a fresh, independent `gpd.sjoin` against the raw `Biome.gpkg` (16-row, undissolved global biome layer), bypassing the pipeline's merge chain entirely. **Zero mismatches across all 3,423 cells** — every cell the pipeline labels "Mangroves" independently falls inside the Mangroves polygon under a from-scratch join.

**Conclusion**: the biome/mangrove attribute join is not the source of the mismatch Becky flagged. Combined with the already-confirmed Mongolia explanation (biome-name dissolution blending landlocked and coastal instances of the same biome), both known artifacts in the biome-level change maps trace to biome-level aggregation itself, not a join/ID bug in the grid-attribute pipeline — strengthens the case for Phase 5.1 (10km-native change maps) as the fix. Closes out the last open row-offset suspect from this project's grid-identity bug history for this data path.

**Not saved to the repo**: ad-hoc verification script only (per standing preference to keep scratch/diagnostic scripts out of the permanent repo) — logic: load mangrove-labeled cells → `representative_point()` → fresh `gpd.sjoin` against `Biome.gpkg` → compare `WWF_biome` columns.

### 2026-07-29 (later same day)

#### Traced the paper's "1,302,099 evaluated cells / 19.37%" figure — it's a flawed, stale calculation, not an authoritative denominator

While documenting land-area shares for the 5-service overlap summary, found that my own
denominator (1,372,621 valid land cells, after Antarctica/Seven Seas/Lakes/Rock & Ice exclusion)
didn't match the paper's existing "~1,302,099 evaluated cells" figure (`09-annex-methodology.qmd`,
`docs/methodology.md`). Traced the actual source: `outputs/book_data_fills.md` (auto-generated by
`Python_scripts/extract_book_data_fills.py`), last regenerated in an early commit (`f85d25c`,
"Complete book, presentation, and paper deliverables") that predates essentially all of this
project's later correction work.

**The script's own logic is the problem, not just its age**: `total_cells_global =
hotspot_area[...]['n_total'].sum() / 8  # 8 services` — sums per-service `n_total` across all 8
services and divides by a flat 8 to recover a "distinct cell count." This is only valid if every
service has the same number of valid cells. It doesn't: Coastal Risk has only ~80,000 valid
(coastal-only) cells vs. ~1.3-1.5M for the others. Averaging that uneven mix with `÷8` produces a
number with no real geographic meaning — an artifact of the formula, not a rigorous denominator.

**Decision (confirmed with the user)**: trust the current, directly-computed data (1,372,621 —
verified via direct query of `10k_change_calc.gpkg` with the same continent/biome exclusion used
throughout this analysis) rather than the stale 1,302,099 figure. Not fixing the paper's existing
19.37% claim right now (it was computed the same flawed way, so it's a candidate for a proper
recompute whenever the paper is next touched) — flagging here so it isn't lost, not urgent.

### 2026-07-28 (later same day)

#### Near-miss: `landgrid_1_clean_enriched_4326.gpkg` has no ID column at all — confirmed `10k_change_calc.gpkg` as the safe master-grid source for the 5-service redesign

While building `scripts/extract_hotspots_5service.R`, copied `hotspot_extraction.qmd`'s geometry-loading fallback (reads `data/vector_basedata/landgrid_1_clean_enriched_4326.gpkg` when `grid_sf` isn't already in memory, tries `orig_fid`/`grid_fid`/`id` in turn, falls back to `geom_sf$fid <- seq_len(nrow(...))` if none exist). That file's actual columns are `Id` (junk, all zeros), `RasterVal`, `c_fid`, `country`, `continent`, etc. — **none of orig_fid/grid_fid/id exist**, so the fallback fired, producing a purely positional fake ID with no relationship to any other file's cell identity. This is the same failure class as the 2026-07-08 LCC crosswalk bug and the 2026-07-24 many-to-one join bug — the fourth instance of this project being bitten by ungoverned grid-identity assumptions. Caught this time only because a `stopifnot` fid-coverage check happened to fire; not guaranteed in general.

**Fix**: used `data/processed/10k_change_calc.gpkg` instead (has a real `grid_fid` column). **Verified, not assumed**: joined its `grid_fid` against `hotspots_global_pct.gpkg` (the canonical, manuscript-verified 8-service hotspot output) — zero mismatches across all 225,113 hotspot cells on every shared attribute (c_fid, continent, region_un, subregion, nev_name, region_wb, income_grp, WWF_biome). This confirms the two files share the same true identity space, not just a coincidentally-matching column name.

**Not fixed, flagged as a fast-follow, not blocking tonight's redesign work**: this repo has several similarly-named grid-like gpkgs (`landgrid_1_clean_enriched_4326.gpkg`, `10k_change_calc.gpkg`, `10k_lcc_granular_metrics.gpkg`, `AOOGrid_10x10km_land_4326_clean.gpkg`, one with a literal timestamp in its filename) with no documented hierarchy or canonical-source statement anywhere. That ambiguity is the root cause enabling all four grid-identity incidents to date. Recommend a follow-up pass: document (or better, rename/consolidate) so there is one unambiguous, well-named canonical master grid file, referenced consistently, rather than several similarly-named candidates of unknown relative authority.

### 2026-07-28

#### Figure 9 (compound-risk serviceshed multiplier) showed >10B people — confirmed a Global-row double-counting bug, fixed

Becky and Steve flagged this in a 2026-07-21 meeting (Jerónimo not present): the compound-risk
dumbbell chart (`outputs/plots/downstream_exposure_dumbbell_compound.png`, Figure 9 in the paper)
appeared to show over 10 billion people at the "≥1 service" tier — impossible, exceeds world
population.

**Root cause, confirmed exactly**: `analysis/plot_multiplier_effect.R`'s compound-tier
aggregation (`compound_connected`) summed `population` from `outputs/tables/
exposure_comparison_compiled.csv` grouped only by `overlap_category`/`exposure_type`, with no
filter excluding the `country == "Global"` row that the CSV also carries (a pre-computed total
across all 224 countries, alongside the per-country rows). Summing both together exactly doubled
every value: "≥1 service" came out as 14.80B instead of the true 7.40B. The per-region/income/
biome/country breakdowns (the Annex charts, `analysis/plot_multiplier_effect.R`'s per-grouping
loop) were **not** affected — they already filtered out their own grouping column's "Global"
value.

**Second, smaller issue fixed in the same pass**: the script defined "Connected Beneficiaries" as
`travel_footprint` alone, not `combined_total` (the union of downstream + travel-access reach)
that `02-methods.qmd` actually documents as the definition. Switched both the compound chart and
the per-grouping charts to `combined_total` for consistency.

**Verified: no manuscript/book/presentation text needed to change.** The prose numbers (3,065M
in-situ; 6,011M/3,756M/7,584M connected at 2+/3+/1+ tiers; 2.5×/5×/8× multipliers) were already
sourced correctly from `06-hotspot-WHO.qmd`'s own traceability table, not from the buggy script —
only the image itself was wrong. That mismatch (chart visually contradicting correct text) is
almost certainly what read as "the numbers don't add up."

**Also**: added an explicit 10B gridline to the compound chart (previously topped out at 3B,
making the correct ~7.6B point look like it was off the scale) and removed
`outputs/plots/downstream_exposure_dumbbell.png` — an orphaned, unreferenced duplicate from an
earlier iteration of this script, superseded by the `_compound` version, nothing in the repo
generated or pointed to it anymore.

**Files changed**: `analysis/plot_multiplier_effect.R`. Regenerated:
`downstream_exposure_dumbbell_compound.png`, `exposure_multiplier_dumbbell_{region_wb,income_grp,WWF_biome}.png`.
Paper, book, and presentation re-rendered to pick up the corrected figures.

### 2026-07-24

#### Attribution-gap numbers corrected AGAIN (34.5%/65.5% was itself inflated by a many-to-one join) — now 34.2%/65.8%, odds ratio 10.79

**Root cause:** `scripts/compute_attribution_true_union.R`'s crosswalk join (`lc_grid_fid` → `master_fid`, built 2026-07-08) is necessarily many-to-one: the LC source grid has 1,691,819 rows vs. the master grid's 1,522,073, so nearest-centroid matching lets up to 9 LC rows share the same nearest master cell. The script used `nrow(lc)` on the joined-but-not-deduplicated table as both the total grid denominator (`n_grid`) and, via `lc$grid_fid[keep]`, each driver's hotspot-cell count — silently counting duplicate LC-side rows as distinct master cells in both places. Inflated `n_grid` by ~11% (1,681,849 counted vs. 1,515,620 actual distinct master cells) and inflated individual driver-hotspot counts unevenly.

**Fix:** after the crosswalk join, keep only the nearest match (min `match_dist_m`) per master `grid_fid` before computing anything downstream (`group_by(grid_fid) %>% slice_min(match_dist_m, n = 1, with_ties = FALSE)`).

**Corrected numbers (supersedes 2026-07-08's 34.5%/65.5%, odds ratio 12.17):**
- ES-hotspot ↔ LCC-driver-union overlap: **34.2%** (77,058 / 225,113 cells), not 34.5%.
- Background rate among non-ES cells: 4.6% (union covers 9.0% of the 1,515,620-cell grid, not 8.2% of 1,681,849).
- Odds ratio **10.79** [10.66–10.92], risk ratio 7.44, *p* ≈ 0 — still a strong, highly significant positive association, magnitude only.
- Per-driver risk ratios shifted much more than the union (uneven duplicate inflation): Crop_Exp 36.6→**97.5**, Forest_Loss 16.7→**25.4**, Urban_Exp 7.8→**9.1**, Grassland_Loss 4.4→**4.8**, Grassland_Gain 3.9→**4.2**. Ranking order (weakest→strongest) unchanged; the true associations were previously *understated*, not overstated.
- Attribution gap: **65.8%**, not 65.5% — same "coverage limitation" reading holds, magnitude only.

**Manuscript/book/presentation:** all attribution-gap numbers rewritten across `01-problem.qmd`, `05-drivers-WHY.qmd`, `08-conclusions.qmd`, `index.qmd`, `paper_draft.qmd` (including Table 3 and its new "how to read risk ratio vs. odds ratio" callout), `presentation.qmd`. Same-session, separately: standardized all these files on **risk ratio** as the lead effect-size statistic (was inconsistently odds ratio in some places, risk ratio in others, with per-driver odds-ratio and risk-ratio values never reconciled against each other) — odds ratio now noted as a secondary, equivalent statistic everywhere. `05-drivers-WHY.qmd`'s methodological-correction section now documents all three corrections (threshold mismatch → grid-id mismatch → many-to-one join) so the history stays traceable. All still flagged pending Becky/Steve review.

**NOT fixed, deferred until after today's paper-review meeting:** `analysis/hotspot_extraction.qmd` has the *exact same* many-to-one join pattern, unfixed, in two places (the `geom_sf` enrichment chunk and the `lc-hotspot-overlap-setup` chunk — both load `10k_lcc_granular_metrics.gpkg` and join through the same crosswalk without deduplicating). This produces the per-service driver-overlap table (`lcc_es_hotspot_overlap_pct.csv`, shown in `05-drivers-WHY.qmd`'s reactable) and several driver-hotspot maps/heatmaps (`global_lcc_driver_map.png`, `global_lcc_driver_hotspots_map.png`, `heatmap_driver_overlap_pct.png`/`_abs.png`) — these likely carry the same inflation and have NOT been corrected. Deliberately not touched today: fixing it means rerunning the full hotspot-extraction notebook (regenerates maps/gpkgs), which is the same notebook that caused the 2026-07-08 striping-bug incident — too risky to do with under 40 minutes before a live paper-review meeting. **Next priority after the meeting**: apply the same nearest-match dedup fix to both chunks in `hotspot_extraction.qmd`, rerun, regenerate the affected maps/heatmaps/CSVs, and re-check every per-service number and image against the corrected output.

### 2026-07-08

#### Striping bug root-caused and fixed; attribution-gap numbers corrected AGAIN (8.2%/91.8% was itself wrong) — now 34.5%/65.5%, odds ratio 12.17

**Root cause found:** not the positional-join bug hypothesized at the end of the 2026-07-07 session. `10k_lcc_granular_metrics.gpkg`'s own `grid_fid` column indexes a *different* source grid (`AOOGrid_10x10km_land_4326_clean.gpkg`, since deleted from disk, 1,691,819 cells) than the master grid used everywhere else in the pipeline (`landgrid_1_clean_enriched_4326.gpkg`, 1,522,073 cells). `hotspot_extraction.qmd`'s fid-detection logic only recognized lowercase `orig_fid`/`grid_fid`/`id` columns; the master grid's real columns are `Id` (capital, all zeros), `c_fid` (a country code), and `RasterVal` (the actual unique cell id, never checked). None matched, so it fell back to `geom_fid <- seq_len(nrow(...))` — the exact fallback an old code comment warned against. This coincidentally worked for `plt_long`/`10k_change_calc.gpkg` (identical row count/order to the master grid, confirmed 0m offset) but not for the LCC file (different grid entirely). Verified: using LCC `grid_fid` values as positional lookups into the master grid gave a 100% mismatch rate (median offset 8,600 km).

**Bigger discovery:** `scripts/compute_attribution_true_union.R` — written 2026-07-07 to fix the stale 24%/76% bug — had the *same* bug: it intersected `es$grid_fid` (master-grid scheme) directly against `lc$grid_fid` (LCC's own scheme). So the "corrected" 8.2%/91.8% (odds ratio 0.88, "no net excess co-occurrence") figures reported yesterday were themselves wrong, built on the same class of error as the thing they were fixing.

**Fix:** `scripts/build_lc_grid_fid_crosswalk.R` (new) — nearest-centroid spatial join between the two grids (same bbox/CRS, confirming same underlying tessellation); 99.41% of cells match at ~0m, 0.59% have no correspondence and are dropped. Writes `data/processed/lc_grid_fid_to_master_fid_crosswalk.csv`. Applied at the source in `analysis/hotspot_extraction.qmd` (both places `10k_lcc_granular_metrics.gpkg` is loaded — the `geom_sf` enrichment chunk and the `lc-hotspot-overlap-setup` chunk, which also fixes the downstream `lc-hotspots-export-by-group` and `lc-hotspot-overlap-calc` chunks since they consume the same corrected `lc_df`) and in `compute_attribution_true_union.R`.

**Corrected numbers (supersedes 2026-07-07's 8.2%/91.8%):**
- ES-hotspot ↔ LCC-driver-union overlap: **34.5%** (77,668 / 225,113 cells), not 8.2%.
- Background rate among non-ES cells: 4.1% (union covers 8.2% of the 1,681,849-cell grid overall).
- Odds ratio **12.17** [12.03–12.32], risk ratio 8.32, *p* ≈ 0 — a strong, highly significant positive association, not "no net excess co-occurrence."
- Every individual driver strongly positive (previously reported as weak/mixed): Crop_Exp OR 44.0, Forest_Loss OR 19.9, Urban_Exp OR 8.0, Grassland_Loss OR 4.8, Grassland_Gain OR 4.2.
- Attribution gap: **65.5%**, not 91.8% — read as a coverage limitation (LCC monitoring reaches ~1/3 of severe ES decline), not an absence of relationship.
- This is closer to, and stronger than, the *original* pre-2026-07-07 framing, not the reframed one. Per-service driver-overlap table also regenerated (`lcc_es_hotspot_overlap_pct.csv`/`_abs.csv`) — same bug, same fix, per-service overlaps now much higher (up to 46.9% for Sed_Ret_Ratio × Forest_Loss).

**New/changed scripts:**
- `scripts/build_lc_grid_fid_crosswalk.R` (new) — builds the crosswalk; rerun if either source grid file changes.
- `scripts/compute_attribution_true_union.R` (patched) — remaps `lc$grid_fid` through the crosswalk before intersecting with `es_fids`.
- `analysis/hotspot_extraction.qmd` (patched) — both `10k_lcc_granular_metrics.gpkg`-loading chunks now remap through the crosswalk; source-level fix, persists across re-renders.
- `scripts/mapping/make_lcc_overview_map.R` (patched) — driver classification now includes "Other (Grassland only)" instead of dropping those cells (previously ~15,632 cells silently excluded from the map).
- `scripts/mapping/make_attribution_map.R` (patched) — the catch-all class was mislabeled "Attribution Gap (Change without Conversion)"/"Change Occuring Off-Pixel", implying zero conversion; renamed to "Below Top-5% LCC Threshold" to correctly convey "conversion may be present, just not in the top-5% intensity tier."
- `scripts/mapping/make_lcc_true_overlap_map.R` (kept as a verification tool, not a manuscript deliverable) — its output PNG was retired since the consolidated driver-hotspots map (with the grassland class added) now covers the same ground; this script was the one that originally surfaced the striping bug and remains useful for spot-checking the crosswalk fix.

**Manuscript/book/presentation:** all attribution-gap numbers and interpretation rewritten across `01-problem.qmd`, `05-drivers-WHY.qmd`, `08-conclusions.qmd`, `index.qmd`, `paper_draft.qmd`, `presentation.qmd`, `scripts/audit_claims.R`. Framing shifted from "no net excess co-occurrence, mixed by driver" to "strong positive association where conversion is intense, but limited coverage (misses ~2/3 of severe ES decline)" — closer to the pre-2026-07-07 thesis, restated with corrected, verified numbers. `05-drivers-WHY.qmd`'s methodological-correction callout now documents both prior errors (threshold mismatch, then grid-id mismatch) so the history is traceable. All still flagged pending Becky review, 2026-07-09.

**Maps consolidated:** `global_lcc_true_overlap_map.png` (diagnostic companion, ES∩LCC subset) removed as a separate deliverable now that `global_lcc_driver_hotspots_map.png` (all LCC driver hotspots) includes the grassland-only class the companion map introduced. Also fixed a pre-existing caption/content mismatch: several manuscript captions claimed `global_lcc_driver_hotspots_map.png` shows the ES∩LCC intersection — it does not, and never did; it's the unfiltered driver-hotspot footprint. Captions corrected to describe the image accurately.

**Lesson learned for future pipeline runs:** this entire investigation traced back to the granular LCC computation (`LC_change.qmd`) having been run against `AOOGrid_10x10km_land_4326_clean.gpkg`, a different grid export than `landgrid_1_clean_enriched_4326.gpkg` (the master grid used by every other stage). If this analysis is redone in the future (e.g., more time points, finer resolution, regional focus), start every stage — including the LCC/diffeR computation — from the *same* master grid file, or explicitly carry forward a real, stable cell ID (not a fallback `seq_len()`) from day one. That single inconsistency, introduced once early in the pipeline, propagated silently for months and nearly derailed the paper's headline finding the night before a PI review meeting.

**Deferred:** group-level driver hotspot files (`drivers_by_group/pct/{income_grp,region_wb,nev_name,WWF_biome}/`) not regenerated — nothing currently consumes them. Future work idea (not yet started): explore a sliding scale of top-N% thresholds (beyond the fixed 5%) and driver combinations for the attribution-gap analysis, to characterize how sensitive the co-occurrence rate is to the threshold choice rather than reporting a single cutoff.

### 2026-07-07 (cont.)

#### Possible striping artifact in LCC driver-hotspot maps — flagged, NOT diagnosed, top priority next session

User spotted an unnatural-looking horizontal banding pattern in `outputs/plots/maps/global_lcc_driver_hotspots_map.png` and in a new comparison map built the same session (`global_lcc_true_overlap_map.png`, see `scripts/mapping/make_lcc_true_overlap_map.R` — plots the TRUE intersection of ES hotspots and LCC driver hotspots, 17,885 cells / 7.9%, built as a non-destructive companion to the existing driver map for visual comparison). Correctly recalled two historical precedents in this log: the 2026-04-08 "Fragment Bug" (`gdf.explode()` fragmenting grid cells, causing striping + duplicated data, fixed via `st_intersects` re-aggregation, v1.3.1) and the 2026-04-28 "Spatial Alignment Crisis" (`seq_len()` reassignment bug scrambling spatial joins across `process_data.qmd`/`hotspot_extraction.qmd`/`hotspot_synthesis.qmd`, "diagnosed and eliminated").

**One diagnostic check done before stopping:** compared against `outputs/plots/maps/first_look_map_pct.png` — the canonical, heavily-reviewed ES-hotspot-only map. That map shows **no striping** — hotspot cells follow organic, real ecological boundaries (Sahel, Amazon, SE Asia, agricultural belts), clean. This means the banding is **not** the old pipeline-wide bug recurring; it appears specific to the LCC-driver-hotspot data product (`data/processed/hotspots/drivers_by_group/pct/global/hotspots_global_pct.gpkg`), not the canonical ES-hotspot pipeline.

**Not yet done:** actually finding the root cause. Starting point identified — `grep -rln "drivers_by_group"` surfaces `analysis/hotspot_extraction.qmd` as the only candidate for where this gpkg is actually built (the other 3 hits — `make_attribution_map.R`, `make_lcc_driver_map.R`, `make_lcc_overview_map.R` — are downstream map consumers, not the source). Next session should trace that chunk in `hotspot_extraction.qmd` and check for a similar fragmentation/join issue to the two historical bugs above, before trusting the driver-hotspot maps or re-deriving anything visual from that gpkg further.

**Implication for the numbers (not just the maps):** `scripts/compute_attribution_true_union.R` (the 8.2%/91.8% true-union calculation from earlier today) reads directly from the raw `10k_lcc_granular_metrics.gpkg`, not from the `drivers_by_group` derivative — a different code path. That does NOT mean it's automatically clean; it just hasn't been checked for the same class of bug yet. Given how much rides on the attribution-gap numbers (already flagged provisional pending Becky's 2026-07-09 review), this needs to be ruled out explicitly, not assumed.

**Update — user confirmed the raw big file is very likely NOT the problem.** Two more maps checked, both derived directly from the raw LC gpkgs with no hotspot-consolidation step: `global_lcc_driver_map.png` (from `make_lcc_driver_map.R`) and `global_lcc_net_change_map.png` (the first half of `make_lcc_overview_map.R`, straight off `10k_lcc_metrics.gpkg`) — **neither shows striping.** Only the second half of `make_lcc_overview_map.R` (which reads the *consolidated* `drivers_by_group` gpkg) does. This pins the problem specifically to the ES-hotspot/LC-change-hotspot **assembly** step, not the underlying week-long `10k_lcc_granular_metrics.gpkg` computation — good news, since that computation does not need to be rerun.

**Further evidence, and a diagnostic plan.** User noted the striped map (`global_lcc_driver_hotspots_map.png`) looks *regionally* wrong, not uniformly wrong: South America, East Asia, Java/Borneo, and the Caucasus look right; Russia, Sub-Saharan Africa, and parts of South America show stripes. Also noted `global_attribution_gap_map_min2_pct.png` (from `scripts/mapping/make_attribution_map.R`, also an ES↔LCC overlap map) is clean. Working theory (user's, and it fits the evidence well): a **positional join bug** — code assuming two tables share row order and merging by position (`cbind`, direct column assignment) instead of an explicit `left_join(..., by = "grid_fid")`. A constant-offset positional misalignment would land wrong in large contiguous blocks (if the grid is stored in spatially-clustered/latitude-sorted order) while landing right elsewhere by chance — exactly the "some regions look right, some look wrong" pattern reported, and structurally the same shape as the 2026-04-28 `seq_len()` bug.

**Plan for next session:**
1. Read the exact chunk in `hotspot_extraction.qmd` that builds `drivers_by_group/pct/global/hotspots_global_pct.gpkg` — pin down precisely how geometry gets attached / how the 5 per-driver hotspot flags get merged; look for positional (row-order-dependent) joins vs. explicit `by = "grid_fid"` keyed joins.
2. Read `make_attribution_map.R` (source of the clean `global_attribution_gap_map_min2_pct.png`) as a **working reference implementation** for the same kind of ES↔LCC overlap — diff its join approach against #1.
3. Check intermediate row counts at each join step in the suspect chunk — silent misalignment often shows up as an unexpected row count even before checking geometry.
4. Fix: very likely swapping the positional merge for an explicit `left_join(..., by = "grid_fid")`, matching both the working reference script and the 2026-04-08 fix pattern (`st_intersects` + `group_by %>% summarise` re-aggregation).
5. Verify: regenerate `drivers_by_group`, rebuild the two striped maps, confirm visually clean, then recompute true-union numbers from the fixed file and compare against `compute_attribution_true_union.R`'s numbers (raw-gpkg-based, bypasses this step entirely). Closer convergence than today's 8.2% vs 7.9% would confirm both the fix and that today's headline numbers were sound all along.

**Idea for later (not step 1, a fix-quality consideration once the root cause is confirmed):** user proposed doing the ES↔LCC assembly via an actual spatial join (`st_intersects`/`st_join`) rather than an attribute-table join, and pointed out this grid is regular (equal-area squares, no complex overlapping shapes like the biome/country polygons that made the original Fragment Bug expensive) — so a geometry-based join here should be cheap and exact, not the computational burden it would be for irregular polygons. Worth doing once the root cause is confirmed: a plain `grid_fid` attribute join is cheaper *if* the IDs are trustworthy across both files; a geometry-based spatial join is slightly more expensive but doesn't depend on trusting the ID at all, matching by actual location instead — the safer choice if there's any chance the IDs drifted between pipeline stages (plausible, given that's essentially what the 2026-04-28 bug was). Check which situation applies before picking one. Matches the precedent that already worked once (`st_intersects` + `group_by %>% summarise` re-aggregation, 2026-04-08 fix).

**Contingency — if `grid_fid` turns out to have actually drifted between files, don't just patch this one output.** If the keyed-join test (plan step 1 above) reveals the `grid_fid` values themselves are inconsistent between the two files — not just a join-method mistake in this one script — the job isn't done by switching this one map/table to a spatial join and moving on. That fixes the symptom here but leaves the same drift latent everywhere else in the pipeline that still trusts `grid_fid` as a stable key, ready to resurface the next time this data gets rebuilt or a new downstream product joins on it. In that case, trace back through the pipeline to find **where** the two numbering schemes diverged (which processing step re-sorted, re-filtered, or re-generated one grid's `grid_fid` without preserving correspondence to the other) and fix it at the source, so every future join on `grid_fid` — not just this one — is trustworthy again.

### 2026-07-07

#### Attribution gap recomputed: 24%/76% was stale, corrected to 8.2%/91.8% (no net excess co-occurrence)

**What happened:** A systematic claim-verification pass across ch03–ch07 flagged that `05-drivers-WHY.qmd`'s headline "24% co-occurrence / 76% attribution gap" traced to `data/processed/tables/lcc_es_hotspot_overlap.csv`, generated before the driver set was finalized at 5 transitions (Forest Loss, Cropland Expansion, Urban Expansion, Grassland Loss, Grassland Gain). That file only covers 2 drivers and its per-row percentages (4–47%) don't reconcile with the current top-5%-severity threshold used everywhere else — reconstruction (matching magnitude and testing directly) indicates it used a much looser "any detected conversion" threshold instead. The live per-driver pipeline (`analysis/hotspot_extraction.qmd` → `lcc_es_hotspot_overlap_pct.csv`/`_abs.csv`) was fine on its own terms, but never computed the *union* across all 5 drivers — only per-driver marginals existed, so no script had ever produced a defensible single headline number.

**What was built:** `scripts/compute_attribution_true_union.R` — computes the true cell-level union across all 5 current drivers using the same top-5%-cutoff definition as the live pipeline, and tests the overlap against the canonical ES-hotspot cell set (225,113 cells, same set behind the Ch06 in-situ population figures) with a proper chi-square/Fisher's exact test rather than an eyeballed ratio. Outputs: `lcc_driver_magnitude_summary.csv`, `lcc_es_hotspot_per_driver_risk.csv`, `lcc_es_hotspot_true_union.csv` in `data/processed/tables/`.

**Result — a directional flip, not just a magnitude correction:**
- True union overlap: **8.2%** of ES hotspot cells (vs. driver-union base rate of 9.3% among non-hotspot cells) — statistically indistinguishable from background (odds ratio 0.88, 95% CI 0.86–0.89). The naive "~5% expected by chance" framing in the old text was also wrong: the correct chance baseline for a union of 5 overlapping 5%-tail drivers is ~9.1% of the grid, not 5%.
- Per-driver split is real and mixed-sign: Forest Loss (RR 1.18) and Urban Expansion (RR 1.22) show a small positive association with ES hotspots; Cropland Expansion (RR 0.80), Grassland Loss (RR 0.79), and Grassland Gain (RR 0.86) show a small negative one. The aggregate near-zero result is these effects cancelling out, not the absence of a pattern.
- Corrected attribution gap: **91.8%**, not 76%.
- Framing choice made with the user: report the corrected finding as a *stronger* version of the chapter's actual thesis ("is land cover monitoring alone sufficient?") rather than softening or hedging it — the answer becomes "no, and closer to not-at-all, except for a modest forest-loss/urban-expansion signal."

**Files updated with corrected numbers and framing:** `docs/manuscript/chapters/05-drivers-WHY.qmd` (full section rewrite + methodological-correction callout), `01-problem.qmd`, `08-conclusions.qmd`, `index.qmd`, `paper_draft.qmd` (also fixed a pre-existing inconsistency where the Methods section said "three conversion drivers" but Results referenced a 4th — aligned to 5, matching the book), `docs/presentations/presentation.qmd`. `scripts/audit_claims.R`'s Audit 4 rewritten to check the new `lcc_es_hotspot_true_union.csv` instead of the stale file (its Audit 3 was also separately broken — schema drift in `hotspot_pop_exposure.csv` — patched to skip gracefully with a pointer to the correct verification path instead of crashing).

**Housekeeping:** `lcc_es_hotspot_overlap.csv` moved to `data/processed/tables/_deprecated/` (not deleted) with a README explaining its provenance and why it's superseded. RSQLite installed for faster attribute-table-only queries against large gpkg files going forward.

**Not yet resolved:** `paper_draft.qmd` and `presentation.qmd` still say "252,215" hotspot cells, while the book's Ch06 traceability table (and a fresh direct query of `hotspots_global_pct.gpkg`) says 225,113 for the same `hotspot_count >= 1` definition. Flagged for the user, not corrected — touches the paper's abstract headline number and deserves its own verification pass, separate from this one.

**Backup:** branch `pre-attribution-gap-rewrite` created at commit `1d6325f`, immediately before this rewrite began, in case any of this needs to be unwound.

#### Rigor/tone review of the attribution-gap rewrite, terminology pass, and provisional-status flags

Follow-up session, same day, reviewing the rewrite above before it goes any further.

**Rigor check on the new prose.** User pushed back on the initial rewrite in three ways, all addressed:
1. Asked for the overlap numbers and statistics (odds ratio, 95% CI, chi-square, p-value) to be explained in plain terms with the actual counts, not just summary percentages — done conversationally, then formalized as a `callout-tip` in `05-drivers-WHY.qmd` right where the odds ratio first appears, walking through the 4-cell contingency table → odds ratio → CI → p-value-vs-effect-size distinction step by step.
2. Flagged adjective inflation ("considerably sharpens," "More strikingly") and, more substantively, flagged that "suggests that mechanisms ... contribute substantially to observed functional decline" was speculation beyond what a co-occurrence analysis can support (no data measures those mechanisms directly). Reworded to "plausible contributing mechanisms, but this co-occurrence analysis cannot establish their relative contribution."
3. Objected to the paper (`paper_draft.qmd`) narrating its own revision history ("an earlier calculation... since corrected") — reviewers/coauthors don't need that, it belongs in this WORKLOG only. Removed from `paper_draft.qmd` (3 instances); kept in the book (`05-drivers-WHY.qmd`'s methodological-correction callout) and `presentation.qmd`'s internal status slide, since those are working documents where the transparency is useful.

**Terminology pass: "extreme" → "intense".** User found "extreme" vague/loaded, especially doubled up with "hotspot" in the same sentence ("extreme ecosystem service hotspot"). Went through all ~58 instances across 10 files (all book chapters, `index.qmd`, `paper_draft.qmd`, `presentation.qmd`) individually rather than blind find-replace, since usage split three ways:
- Adjective describing the threshold ("extreme (top 5%) land cover conversion") → swapped to "intense" (~44 instances).
- Noun form paired redundantly with "hotspot" ("localized extremes (hotspots)") → dropped entirely rather than replaced (8 instances) — e.g. ch03/ch09 now just say "hotspots."
- Genuine statistical-tail usage ("cells at the opposite extreme," "extreme percentiles," index.qmd's glossary heading "A Relative Extreme, Not an Absolute Threshold," and one instance in `paper_draft.qmd:198` the user explicitly asked to keep) — left as "extreme," these are technically precise, not vague.
- One unrelated instance (ch09: "extreme polar latitudes," about raster coverage) — untouched.

**Further adjective trim.** User pushed back again on my own replacement choices ("exceptionally severe," "unusually high") as still smuggling in unearned judgment/rarity claims. Principle applied: drop the intensifier or let the adjacent number carry the claim, rather than swap one evaluative word for another. Fixed in ch04 (parallel "moderately degraded" / "severely degraded" instead of "exceptionally severe") and ch08 (two instances — "unusually high relative intensities, experiencing... 3.5 to 8.5 times higher" → just "hotspot concentrations 3.5 to 8.5 times higher"; "particularly rapid service decline" → "rapid service decline").

**WHY-pillar framing questioned.** User asked whether ch05's chapter, filed under the book's "WHY" pillar (of the WHAT/WHERE/WHO/WHY four-question framework in `01-problem.qmd`), is actually answering a "why" question. Assessment: no, not in the causal sense — the chapter itself already disclaims causal attribution and its own title is "The Monitoring Gap: Is Land Cover Change Alone Enough?", not a causal-mechanism framing. It's really a monitoring-adequacy/detectability question. The chapter-level reframe (from the 2026-06-24 WHY-reframing session) never fully propagated to the ch01 pillar label. Not resolved yet — flagged for discussion, not fixed, per user's explicit "pause and think" instruction. Also found, in passing: `01-problem.qmd`'s four-question list assigns Chapter 5 to *both* "WHERE Analysis (Chapters 4–5)" and "WHY Analysis (Chapter 5)" — likely a stale range, should probably read "Chapter 4" for WHERE. Not fixed yet either.

**Provisional-status callouts added.** Given how much the attribution-gap section changed (direction of the finding flipped, not just the number) and that this hasn't been reviewed by Becky yet (meeting scheduled **2026-07-09**), expanded the callouts in both `05-drivers-WHY.qmd` (callout-important added above the existing methodological-correction callout) and `paper_draft.qmd` (new callout-important at the top of "The Spatial Attribution Gap" section, matching the paper's existing `[FLAG FOR BECKY]` style — flags the section for review without narrating correction history, consistent with the instruction above). Both explicitly say: do not cite the 8.2%/91.8% figures or the "strengthens the thesis" framing outside this working draft until reviewed.

**State at end of day:** nothing committed — still on `task/housekeeping`, backup at `pre-attribution-gap-rewrite` (commit `1d6325f`). Next session: user will review current state independently, bring additional wordiness instances found during their own read-through, and revisit the WHY-pillar framing question and the 252,215-vs-225,113 discrepancy (still unresolved, flagged in the entry above).

### 2026-06-24

#### Presentation polish (session 10) + WHY reframing across paper, book, and presentation

**Blank slides fully eliminated (second pass)**
Removed 8 additional `---` separators immediately *before* `#` section headers (Part 2: Methods, WHAT, WHERE, WHO, WHY, Regional Profiles, Synthesis, Thank You). First pass (session 9) removed `---` after section headers; these were the before-header instances also creating blank slides.

**Mermaid Analytical Pipeline: font and label fixes**
Reduced `fontSize` to `13px` via `themeVariables`; shortened longest node labels to reduce box overflow. Box sizing still tight — flagged as todo for future pass.

**Methods slides restructured: two new slides added**
- *Data Sources* renamed *Data Inputs* and split into three-column layout (ES / Land Cover / Socioeconomic)
- *Eight Ecosystem Services*: new slide with all 8 services in a table grouped by theme (Habitat & Access / Coastal Risk / Hydrological Regulation), plain-language descriptions, and a callout explaining the retention ratio distinction (declining ratio = functional degradation even without volume change)
- *Socioeconomic Context: The WHO Input Layers*: new slide with `global_socioeconomic_context_map.png` (4-panel: Pop, HDI, GDP, GINI); caption points to KS typology finding
- *Spatial Framework & Analytical Groupings*: kept, with italic grid note and cleaned caption

**WHY section: two new slides added**
- *What Land Cover Change Looked Like 1992–2020*: `global_lcc_net_change_map.png` as setup before attribution gap results
- Existing attribution gap slide unchanged

**Appendix: hotspot intensity boxplot added**
- `boxplots_unified/region_wb/boxplots_volumetric_pct.png` added as appendix slide with compact reading guide callout (wide box = variable, narrow = consistently extreme; Latin America punchline)

**WHY reframing — major conceptual work (presentation + paper + book)**

Core insight: the WHY question is not "what drives ES change" but "is land cover monitoring alone sufficient to track where ES provision is declining most?" — more honest about what the co-occurrence analysis actually measures and more novel as a contribution.

Changes applied across all files:
- **Presentation**: section divider → "WHY: The Monitoring Gap"; slide 4 WHY question → "Is monitoring land cover change alone enough to track where ES provision is declining most?"; mermaid node → "Monitoring Gap"; speaker notes updated
- **paper_draft.qmd**: Discussion section title → "The Sufficiency of Land Cover Monitoring as a Proxy for ES Provision Change"; Becky callout resolved (Option A applied — "consistent with the view that LC monitoring is insufficient", compressed to a short flag asking for an optional citation if she wants Option B)
- **ch01-problem.qmd**: WHY question heading → "Is land cover change alone enough to track ES provision decline?"; WHY framework bullets updated; chapter cross-reference corrected (8→5)
- **ch05-drivers-WHY.qmd**: chapter title → "The Monitoring Gap: Is Land Cover Change Alone Enough? (WHY)"; opening sentence → "To assess whether monitoring land cover change alone is sufficient..."; Summary callout header → "Five findings on the sufficiency of LC monitoring..."

**ch04 boxplot section: reading guide added**
Full callout explaining how to read the boxplots added before the tabset — axes, IQR, whiskers, color, wide vs narrow interpretation, direction of change per service type, and the key point that all cells are already global top 5%. All three ratio boxplot figures (previously captionless) now have proper captions. Latin America finding called out in region tab.

**Paper and book shared with Becky Chaplin-Kramer** for co-author review. Two open questions flagged for her:
1. Were InVEST SDR/NDR runs performed with fixed or era-specific climate inputs?
2. Optional Option B citation for the monitoring proxy claim (REDD+/IPBES literature)

**Nothing committed** — awaiting render review and Becky's feedback before committing.

### 2026-06-22

#### Population exposure numbers — full audit and correction

Traced all population exposure figures to their source data. Two errors found and corrected across Ch06, Ch08, paper_draft.qmd, and docs/methodology.md.

**Error 1 — Service-weighted sum reported as distinct individuals.**
`hotspot_pop_exposure.csv` stores one row per (service × socioeconomic bin). Summing `exposed_population` across all 8 services yields 5,286 million — but this counts each person once per qualifying service (avg 1.68 services per hotspot cell → 377,719 cell-service units across 225,113 unique cells). The correct count of distinct individuals in any hotspot cell is **3,065 million**, computed by joining `hotspot_count ≥ 1` cells from `hotspots_global_pct.gpkg` to `GHS_POP_E2020_GLOBE_sum` in `10k_change_calc.gpkg`.

**Error 2 — In-situ compound hotspot population was unverifiable and wrong.**
The figure of 8.6 million for "people living in 2+ service areas" could not be reproduced from any output file. The beneficiary CSVs for 2+ overlapping cells only store connected (downstream/access) populations in the billions, not in-situ counts. The correct in-situ figure for `hotspot_count ≥ 2` cells is **1,212 million** (85,599 cells).

**Verified numbers (from current data):**

| Filter | Cells | GHS-POP in-situ | Connected (union) | Multiplier |
|---|---|---|---|---|
| `hotspot_count ≥ 1` | 225,113 | 3,065 M | 7,584 M | 2.5× |
| `hotspot_count ≥ 2` | 85,599 | 1,212 M | 6,011 M | ~5× |
| `hotspot_count ≥ 3` | 41,025 | 445 M | 3,756 M | ~8× |

The 7.6B / 6,409M / 7,390M beneficiary figures (all hotspot cells) were verified correct.

The 700× and 1,700× multipliers previously stated were derived from the wrong 8.6M denominator and have been corrected to ~5× and ~8× respectively.

**Files updated:** Ch06 (data traceability callout + all prose + Key Takeaways), Ch08 (Key Takeaway #2), paper_draft.qmd (§Population Exposure), docs/methodology.md (§Population Exposure section with verified table; §Attribution section with symmetric 5%/5% threshold documentation). Changes not yet committed — awaiting render review.

### 2026-06-20

#### Book chapter polish (Phase 3, sessions 5–6)

*   **Chapter 7 (Regional Profiles) reviewed and cleaned:**  All `~X million` population placeholders replaced with `[TBD — extract from pop_exposure table]`. Two suspicious hardcoded numbers removed: "~580 million depending on water/food" (effectively LAC's entire population) and "~1,450 million smallholder farmers" (South Asia's full regional head-count, not just farmers). A `callout-warning` was added explaining how to extract real region-level numbers from the interactive pop_exposure table. "Next Steps" stub section removed.
*   **Key Takeaways sections added to all remaining chapters:** Chapters 4, 6, 7, and 8 now have substantive Key Takeaways callouts (five findings each). These synthesize the geographic concentration patterns (Ch4), cross-scale exposure logic and three socioeconomic typologies (Ch6), Global South burden and within-region heterogeneity (Ch7), and the five top-line policy messages including the pipeline-as-infrastructure framing (Ch8).
*   **Final attribution language fix in Ch8:** Bullet "(no detectable conversion)" in the policy section's Driver-Specific Strategies → "(no co-occurrence with extreme LCC)". Zero remaining instances of the old causal framing across all `.qmd` files.

#### Interactive Summary Statistics — Chapter 4

*   **DT replaced with `reactable`:** The two existing Summary Statistics tables (Hotspot Area Coverage, Compound Risk) were failing to show filter inputs in Quarto tabsets because DT's `filter = "top"` row does not initialize in hidden tabs. Switched to `reactable` (`filterable = TRUE`, `searchable = TRUE`), which initializes lazily and renders filter inputs correctly. All column names cleaned (snake_case → readable labels), numerics pre-rounded, default sort set to Relative Intensity / Mean Overlapping Services descending. Installed `reactable` (v0.4.5) + its dependency `reactR` into the project R library.

*   **Cross-dimensional analysis tab added:** A third tab "Country × Biome (Cross-dimensional)" was added to the Summary Statistics tabset. It loads `hotspots_global_pct.gpkg` via SQL (no geometry, 225K rows in ~1.4 s), pivots the 8 per-service hotspot flags to long format, and computes a (country × biome × service) cross-tabulation with n_hot, n_total, % hotspot area, % of global hotspots, expected share, and relative intensity. Produces 3,553 rows (country–biome–service combinations with ≥1 hotspot cell). The chunk is cached so it only runs on first render.

    Example query verified in R: Brazilian Mangroves — 83 cells total; Pollination 57/83 (68.7%, 2.25× expected); Coastal Risk 7/83 (8.4%, 4.78× expected — nearly 5×). Any similar combination (e.g., Indonesia Mangroves, India Tropical Dry Forests, South Korea Temperate Forests) can be queried live in the rendered HTML book without re-running R.

*   **README updated:** New "## Interactive Book Output" section added describing the three table types, their filter/sort capabilities, and example queries. Aimed at co-authors and future users who receive the rendered HTML and want to understand what analytical questions can be answered without re-running the pipeline.

### 2026-06-19
*   **Paper polish (Phase 3):** Completed Introduction AI-slop cleanup, added Scalability paragraph to Conclusions, swept all book chapters for attribution language. No issues found in chapters 03, 04, 06, 07, 09.
*   **Attribution framing hardened:** Rewrote Results §Spatial Attribution Gap (was making causal claims inconsistent with Discussion), tightened Abstract, Methods, and Conclusions to consistently use co-occurrence language. Table label changed from "Unmapped Degradation" → "No Detected LCC".
*   **Climate forcing mechanism clarified:** Expanded Discussion item 2 to explain the specific R-factor pathway by which precipitation changes propagate through InVEST SDR/NDR into modeled service outputs. Added IPCC AR6 (2021) and Nearing et al. (2004) to references.
*   **⚠️ OPEN QUESTION FOR BECKY — blocks submission:** Were the InVEST SDR/NDR model runs for 1992 and 2020 performed with (a) the **same fixed climate inputs** (e.g., one long-term-average WorldClim R-factor raster applied to both years) or (b) **era-specific climate inputs** for each year (e.g., year-matched CHELSA or ERA5 precipitation/erosivity)? This determines whether "Climate Forcing" is a direct or only indirect explanation for the 76% attribution gap, and therefore how strongly to hedge the Discussion language. A callout note has been added in `paper_draft.qmd` §Attribution Analysis to flag this for the next advisor meeting.

### 2026-06-16
*   **Repository Restructuring & Cleanup:** Conducted a major repository cleanup to align with FAIR principles and good industry practices. Transitioned the project from a standard R package structure to a broader reproducible research project structure, acknowledging its evolution into a large-scale analytical pipeline.
*   **Data Consolidation:** Unified data directories, ensuring that `C:\projects\global_NCP\data` contains the most recent canonical data, while deprecating redundant `home/` directories.
*   **Git Cleanup:** Removed a large number of temporary and untracked files from the `home/` directory (e.g., temporary Rscript runs and libloc files) from the git repository to ensure a clean and reproducible state.

### 2026-06-12
*   **Population Exposure Milestone:** Calculated the total 2020 GHSL population captured across the 1.3 million evaluated 10km grid cells (7,855,519,292 people).
*   **Near-Universal Exposure:** Verified that the 7.6 Billion "Connected Beneficiaries" represent **96.7%** of the evaluated global population. This massive share confirms that almost the entire global population is connected to at least one ecosystem service loss hotspot via downstream hydrological pathways or travel-access footprints.

### 2026-06-11
*   **Methodological Optimization (Population Exposure Extraction):** Bypassed the single-attribute limitation of `zonal_stats_toolkit` for the multi-level hotspot beneficiaries analysis. 
*   **Multi-Dimensional Slicing:** By extracting raw population data directly to the 1.5 million 10km grid cells (`landgrid_1_clean_enriched_4326.gpkg`) using `exactextract` (with `strategy="raster-sequential"` to prevent GEOS C++ crashes) and subsequently grouping in Pandas by `['country', 'region_wb', 'income_grp', 'WWF_biome']` simultaneously, we squash millions of rows into a lightweight, highly flexible CSV.
*   **Analytical Power Unlocked:** This structural decision allows downstream R scripts to effortlessly filter and cross-tabulate complex intersections (e.g., "Exposure in Low-Income countries within Sub-Saharan Africa") on the fly, without needing to re-run expensive spatial intersections.

### 2026-06-08
*   **Ground-Truth Narrative Audit:** Conducted a comprehensive, data-driven audit of all high-level claims in the synthesis chapters and manuscript draft using exact values from `hotspot_area_stats.csv` and the attribution scripts.
*   **Narrative Corrections:** Purged several "echo chamber" inaccuracies in the text. Verified that Lower-Middle Income countries face the highest relative intensity (1.19x absolute, 1.6x OECD), Latin America and East Asia are the true regional epicenters, and Mangroves are the most severely impacted biome (nearly 5x expected intensity). Excluded micro-states (area < 10,000 sq km) from country-level rankings, revealing South Korea, Jamaica, Malaysia, and Guatemala as top intensity spots.
*   **Hotspot Definition Refinement:** Clarified manuscript language to explicitly define hotspots based on the "extreme 5% of relative change values (Symmetric Percentage Change)", correctly identifying approx. 250,000 unique cells with at least one hotspot.
*   **Output Audit Artifact:** Established a permanent logging mechanism (`outputs/audit_summary.txt`) to maintain a paper trail of the core ground-truth statistics for peer review and manuscript defense.

### 2026-06-04
*   **Pipeline Robustness & Zombie Data Fix:** Identified and resolved a critical bug where `process_data.qmd` ingested a stale, misaligned coastal GPKG because the file loading was hardcoded to grab the top 3 files by date. Updated the script to dynamically load *all* GPKGs present in `summary_pipeline_workspace_ha`.
*   **Coastal Extraction Canonical Path:** Restored `analysis_configs/c_protection_synth.yaml` to point to the archived coastal risk rasters (`Rt_1992.tif`, etc.). Confirmed that calculating ratios natively on vectors and *then* rasterizing them is the only stable path, bypassing C-level crashes.
*   **Dateline Artifact Resolution:** Added `sf::st_wrap_dateline()` with `DATELINEOFFSET=180` to the final export step of `process_data.qmd` to prevent horizontal tearing artifacts when rendering the EPSG:4326 output in QGIS.
*   **Future-Proofing the Pipeline:** Refactored year-detection regex from hardcoded `"1992|2020"` to dynamic `"[0-9]{4}"`. Added prominent `[MANUAL UPDATE REQUIRED]` templates directly into the `process_data.qmd` script to guide future users on exactly how to drop specific years (for multi-year comparisons) or add new variables without breaking the analysis.

### 2026-06-03
*   **Geometry Crashes Finally Conquered:** After a grueling two-week struggle involving `GEOSException` crashes, memory leaks, and exploded geometries, we have finally established a mathematically sound and highly performant vector-extraction workflow.
*   **The Breakthrough:** The root cause of the crashes in Python/GEOS was isolated to a small number of malformed "poison polygons" during the C-level EPSG:4326 reprojection phase.
*   **Solution Implementation:** We consolidated the grid creation into a single, robust Python script (`build_master_grid.py`). It uses chunked reprojection (processing the 1.5M cell grid in blocks of ~7500). If a chunk fails the fast C-level reprojection, the script falls back to an isolated row-by-row projection, safely discarding the few mathematically impossible geometries while preserving the rest.
*   **Pipeline Success:** `summary_pipeline_landgrid.py` was successfully run against this new master grid (`landgrid_1_clean_enriched_4326.gpkg`) for both the 1992/2020 Services and the Socioeconomic Beneficiaries. The pipeline finished in ~13 minutes with zero crashes and zero duplicated/exploded fragments.
*   **Housekeeping:** Deleted redundant scratch scripts (e.g., `clean_grid_4326.py`, deprecated in favor of `build_master_grid.py`).
*   **R Consolidation:** `process_data.qmd` was overhauled to simply merge the pristine output GPKGs from the Python workspace based on the reliable `fid`.

### 2026-06-02
*   **Vector Data Enrichment Pipeline Stabilized:** After being blocked for over a week by intractable geometry and performance issues in the R-based `prepare_data.qmd` script, a robust Python-based solution has been successfully developed and executed.
*   **Problem:** The original R script was unacceptably slow and consistently failed with obscure `GEOSException` errors when performing spatial joins on the 1.5M-cell grid.
*   **Solution:** A new script, `Python_scripts/enrich_grid.py`, was created to handle this critical data preparation step.
    1.  **Performance:** The initial polygon-intersection approach was too slow. The script was re-engineered to use a much faster and more stable **centroid-based spatial join**. This reduced processing time from hours to minutes.
    2.  **Robustness:** Iteratively debugged a series of `KeyError` and `ValueError` exceptions related to inconsistent column names (`WWF_BIOME` vs. `WWF_biome`, `country` vs. `nev_name`) and internal `geopandas` state (`index_right` conflicts).
    3.  **Final Output:** The script successfully produced `landgrid_1_clean_enriched.gpkg`, a clean, attribute-rich vector grid containing all necessary biome and country/regional information. This file now serves as the canonical input for the main zonal statistics pipeline, unblocking all downstream analysis.

---

### 2026-05-27 (cont. 7)
*   **Final Strategic Pivot & Course Correction:** The `GEOSException: ...closed linestring` error continues to be completely intractable in the vector-based Python pipeline (`summary_pipeline_landgrid.py`), even with multiple aggressive cleaning patches. This confirms that the vector file's geometry issues are too severe to be reliably fixed on-the-fly in a multiprocessing environment.
*   **Definitive Solution:** The project is now fully reverting to the **hybrid raster-vector workflow** that was prototyped on 2026-05-26. This is the only robust path forward.
    1.  **Deprecate Vector Pipeline:** The `summary_pipeline_landgrid.py` script and its associated vector-based logic are now considered deprecated. All efforts will focus on the raster-based workflow.
    2.  **Create Zone Raster:** The `analysis/create_zone_raster.R` script provides the stable "zone" input needed for Python.
    3.  **Implement Raster Pipeline:** A new configuration (`analysis_configs/services_raster.yaml`) has been created to drive `summary_pipeline_rasterzones.py`. This script performs all zonal statistics using the zone raster, completely avoiding vector geometry processing in Python and thus eliminating the `GEOSException`.
    4.  **Simplify R Consolidation:** The `analysis/process_data.qmd` script has been overhauled. It no longer needs to perform complex spatial joins or aggregations to fix "exploded" fragments. It now reads the clean CSV output from the raster pipeline and performs a simple, fast `left_join` by `fid` against the master grid.
*   This new workflow is not only more robust and error-free but also significantly simpler and faster. The `README.md` has been updated to reflect this as the new canonical procedure.

---

### 2026-05-27 (cont. 6)
*   **Python Pipeline Failure (`GEOSException` Persists):** The `closed linestring` error continues to occur in the `zonal_stats` worker process during reprojection, even after the `buffer(0)` patch was applied.
*   **Root Cause Analysis:**
    1.  This confirms that this dataset contains exceptionally stubborn geometry invalidities.
    2.  The file I/O cycle where the main process writes a temporary GeoPackage and the worker process reads it is the most likely source of re-introducing these subtle errors.
    3.  The single `buffer(0)` call in the worker is insufficient. It may even be creating empty or invalid sliver polygons from highly malformed inputs, which are not being filtered out before the `to_crs()` call.
*   **Resolution:**
    1.  **Aggressive Just-in-Time Cleaning:** The `zonal_stats` function in `summary_pipeline_landgrid.py` has been patched with a much more robust cleaning sequence. It now performs a `buffer(0).make_valid()` and then explicitly filters out any empty or near-zero-area geometries that may have been created. This mirrors the extensive cleaning performed in the main process and ensures the data is as clean as possible immediately before the sensitive reprojection step. This should finally resolve the recurring geometry exceptions.

---

### 2026-05-27 (cont. 5)
*   **Python Pipeline Failure (`GEOSException`):** The pipeline is now running past the `FileNotFoundError` but fails during zonal statistics with a `shapely.errors.GEOSException: IllegalArgumentException: Points of LinearRing do not form a closed linestring`.
*   **Root Cause Analysis:**
    1.  This error occurs during the `gdf.to_crs()` reprojection step inside the `zonal_stats` function.
    2.  This is a classic geometry validity issue. Although the `main()` function performs extensive cleaning (`buffer(0)`, `make_valid()`) before writing a temporary GeoPackage, this error indicates that either the cleaning was insufficient, or that the file I/O cycle and/or the reprojection operation itself is re-introducing or exposing subtle invalidities.
    3.  The history of this project (`WORKLOG.md`) shows a recurring theme of geometry issues when passing data from R's `sf` package to Python's `geopandas`.
*   **Resolution:**
    1.  **Pipeline Hardening:** A patch has been applied to `summary_pipeline_landgrid.py`. The `zonal_stats` function will now re-apply the `gdf.geometry.buffer(0)` cleaning trick immediately after reading the vector data. This ensures that any geometry issues are fixed just-in-time before the reprojection is attempted, making the process more robust against these recurring data integrity problems.

---

### 2026-05-27 (cont. 4)
*   **Recurring Python Pipeline Failure (`RasterioIOError`):** The pipeline failed again with a `No such file or directory` error, this time for `n_retention_ratio_2020.tif`.
*   **Root Cause Analysis:**
    1.  The error message (`/data/base_years_ha/n_retention_ratio_2020.tif: No such file or directory`) is identical in nature to the previous failure. It indicates the Python script is looking for a file in a path that is missing the `/raw/` subdirectory.
    2.  The fix applied in the previous step (updating all paths in `analysis_configs/services_slim.yaml` to include `/raw/`) correctly resolves this issue for all raster layers.
    3.  The fact that the pipeline failed again on a *different* file but with the *same* path issue strongly indicates that the pipeline was executed using the original, un-patched YAML configuration file.
*   **Resolution:**
    1.  **Action Required:** The user must ensure they are running the Python pipeline using the version of `analysis_configs/services_slim.yaml` that was corrected in the previous step. No new code or configuration changes are needed.
    2.  The `summary_pipeline_landgrid.py` script remains robust enough to handle individual task failures if the YAML is partially correct, but the root cause of the `FileNotFoundError` must be addressed by using the fully corrected configuration file.

---

### 2026-05-27 (cont. 3)
*   **Python Pipeline Failure (`RasterioIOError`):** The Python pipeline failed during zonal statistics with a `No such file or directory` error for `sed_retention_ratio_2020.tif`.
*   **Root Cause Analysis:**
    1.  The immediate error is a `FileNotFoundError`, indicating the path in the `services_slim.yaml` config is incorrect or the file is missing.
    2.  The user confirmed with an `ls` command that all required raster files, including the ratio files, exist in a single directory: `.../raw/base_years_ha/`.
    3.  A review of `analysis_configs/services_slim.yaml` revealed that the paths were missing the `/raw/` subdirectory (e.g., pointing to `${GLOBAL_NCP_DATA}/base_years_ha/...` instead of `${GLOBAL_NCP_DATA}/raw/base_years_ha/...`). This path mismatch is the direct cause of the `FileNotFoundError`.
*   **Resolution:**
    1.  **Primary Fix:** All raster paths in `analysis_configs/services_slim.yaml` have been updated to include the correct `/raw/` subdirectory, ensuring they point to the actual file locations.
    2.  **Robustness Patch (Retained):** A patch was previously applied to `summary_pipeline_landgrid.py` to make it more robust. It now checks if a task returns `None` (indicating failure) and skips it, preventing the main script from crashing with an `AttributeError`. This remains a useful improvement.

---

### 2026-05-27 (cont. 2)
*   **Definitive Root Cause & Solution:** The user discovered that the Python `GEOSException` could be bypassed by using `geopandas.read_file(..., on_invalid="ignore")`. This confirms the root cause is a small number of features with invalid geometries being written by R's `sf` package that `geopandas` cannot read by default.
*   **Pipeline Hardening:** Instead of creating more intermediate "cleaned" files, the core Python script (`summary_pipeline_landgrid.py`) has been updated to use the `on_invalid="ignore"` flag. This makes the pipeline itself resilient to these minor upstream errors, providing a much more robust and direct solution. The separate `patch_add_biomes.R` script is no longer necessary, as the main `prepare_data.qmd` can be run in its complete form, and the Python script will now correctly ignore any problematic geometries it produces.

---

### 2026-05-27 (cont.)
*   **Pipeline Unblocking Strategy:** The `prepare_data.qmd` script continues to fail on complex geometry operations. To unblock the pipeline without losing more time, we've adopted a two-stage approach:
    1.  **Generate Base Grid:** Run a simplified version of `prepare_data.qmd` that intentionally excludes the problematic biome attribute join. This is expected to succeed and produce a clean grid with country/regional attributes.
    2.  **Patch Biomes:** Created a new, standalone script (`analysis/patch_add_biomes.R`) that takes the output from step 1 and performs only the biome join. This script uses a robust `st_join` followed by a `distinct(ID)` call to handle any duplicates created if a grid cell touches multiple biomes.
*   This strategy allows us to get a complete, analysis-ready grid file (`AOOGrid_10x10km_land_4326_clean.gpkg`) so the downstream Python pipeline can finally proceed.

---

### 2026-05-27
*   **Data Pipeline Failure & Strategic Rollback:**
    *   **Breaking Change Identified:** After multiple failed attempts to fix the `IllegalArgumentException: Invalid number of points in LinearRing` error, a comparison with the last known working version of `prepare_data.qmd` was performed.
    *   **Root Cause:** The error was introduced when the data preparation logic was changed to accommodate biome attributes. The original, working script used a direct `st_join` on polygons. The new, failing script introduced a call to `st_point_on_surface()` before joining, which is much less tolerant of minor geometric invalidities created during the `st_transform()` reprojection step.
    *   **Resolution:** The `prep-grid-aoo-land` chunk in `prepare_data.qmd` has been reverted to the simpler, more robust logic from the last working version. This removes the dependency on `st_point_on_surface` and the complex, multi-source attribute join, which was the source of the instability. The pipeline should now be able to generate the base grid successfully, as it did before these changes. The addition of biome data will be re-evaluated in a separate, more robust manner after the core pipeline is restored.

---

### 2026-05-26
*   **Data Pipeline Crisis & Strategic Pivot:**
    *   **Root Cause Re-confirmed:** The Python pipeline (`summary_pipeline_landgrid.py`) consistently fails with a `GEOSException: Invalid number of points in LinearRing` when reading the master grid GeoPackage (`AOOGrid_10x10km_land_4326_clean.gpkg`). This indicates a subtle geometry validity issue created by the R `sf` package that Python's `geopandas/shapely` cannot tolerate.
    *   **Failed Repair Attempts:** A standalone patch script (`patch_fix_grid_geom.R`) using `st_buffer(dist = 0)` was created to aggressively repair the geometries. However, this process proved to be unacceptably slow, running for over 4.5 hours without completion, making it an unviable solution.
    *   **New Strategy: Hybrid Raster-Vector Workflow:** A new, more robust strategy has been adopted to permanently solve this issue.
        1.  **Create Zone Raster:** A new script (`create_zone_raster.R`) was created to perform a fast, one-time conversion of the vector grid into a "zone raster" where each pixel's value is its corresponding `fid`.
        2.  **Raster-Based Zonal Stats:** A new Python script (`summary_pipeline_rasterzones.py`) will perform the zonal statistics using the new zone raster and the service rasters. This completely bypasses the need for Python to read the problematic vector file, eliminating the geometry errors.
        3.  **Attribute Join in R:** The main R script (`process_data.qmd`) will be updated to read the simple CSV output from the new Python script and join it back to the canonical vector grid, which contains all the rich attribute data (country, biome, etc.).
    *   This new hybrid approach is faster, more robust, and preserves the methodological integrity of the analysis by separating the geometry-heavy processing from the statistical calculation.

---

### 2026-05-22
*   **Major Data Pipeline Overhaul & Rerun:**
    *   **Root Cause Identified:** The critical `orig_fid not found` error in `process_data.qmd` was traced back to a stale base grid file (`AOOGrid_10x10km_land_4326_clean.gpkg`). This old grid was missing key attributes (like country names) and contained geometric artifacts (dateline wraparound), which were causing cascading failures in the Python pipeline.
    *   **Robust Solution Implemented:**
        1.  The `prepare_data.qmd` script was updated with a new, authoritative chunk (`prep-grid-aoo-land`) to generate a clean master grid from the original sources. This new grid correctly joins all attributes and fixes the dateline artifact.
        2.  The `process_data.qmd` script was simplified by removing the redundant and error-prone "Robust Attribute Assembly" logic, as the pipeline can now trust its clean input.
    *   **Full Data Regeneration Initiated:** A full rerun of the data pipeline has been started on the `lilling` server. This involves:
        1.  Running `prepare_data.qmd` to create the new master grid (a long process, ~1.5 hours).
        2.  Running the upstream Python pipeline (`summary_pipeline_landgrid.py`) for both services and beneficiaries using the new clean grid.
        3.  Running `process_data.qmd` to generate the final analysis-ready datasets.
    *   **YAML Fix:** Corrected a `YAMLException` in `chapters/01-problem.qmd` caused by improper indentation in the YAML header.
*   **Next Steps:** While the data regeneration runs, the focus will shift to refining the narrative, language, and presentation of the final Quarto book. A transfer prompt has been created to start a new chat session for this purpose.

---

### 2026-05-21
*   **Dashboard Layout Debugging Saga:** Spent significant time debugging the layout of `analysis/eda_dashboard.qmd`.
    *   **Initial Problem:** Plots were rendering too small to be readable in the dashboard format.
    *   **Attempt 1:** Switched the document format from `html` (page) back to `dashboard` and enabled `scrollable: true` to allow tall plots to render at their full height.
    *   **Attempt 2:** Implemented a side-by-side `columns` layout for the main plot sections to improve readability and use of space.
    *   **Core Issue Identified:** An unclosed `div` block (caused by a missing `:::` to close a `columns` section) was making all subsequent dashboard tabs appear empty.
    *   **Resolution & Final Layout:** Correctly structured the `columns` blocks for all sections, which fixed the empty tabs. After experimentation with side-by-side layouts (e.g., `width="50%"`), the decision was made to lock in a vertically stacked layout (`width="100%"` for all columns) within each major section. This provides a consistent, readable, top-to-bottom flow for all plots and tables in the dashboard. The layout is now considered stable.

---

### 2026-05-17 (cont.)
*   **Rasterization Workflow Template:** Created `scripts/gdal_rasterization_template.sh` to formalize and document the robust `gdal_rasterize`-based workflow. This template includes steps for GeoPackage reprojection and rasterization of both continuous and binary columns, ensuring easy reusability and preventing loss of this critical methodological knowledge.

---

### 2026-05-17
*   **Output Naming Convention:** Standardized raster output filenames to include the change metric (`_abs` or `_pct`) for clarity and consistency. For example, `hotspot_count.tif` is now `hotspot_count_abs.tif`. This ensures that all raster files can be distinguished by their filename alone.

---

### 2026-05-16
*   **Methodological Reflection:** Acknowledged that the extensive time spent debugging Python-based rasterization was inefficient. The direct use of `gdal_rasterize` from the command line proved to be a faster, more powerful, and more reliable solution from the beginning. Future rasterization tasks should default to using the core GDAL command-line tools to avoid similar issues with high-level library wrappers.

---

### 2026-05-15
*   **Rasterization & Grid Validation Saga:**
    *   Spent significant time debugging a persistent and subtle rasterization issue. Initial attempts to rasterize hotspot counts using the `vector_to_raster.py` script resulted in "ghost rasters" (tiny file sizes, empty when loaded in QGIS/R) and severe spatial misalignment artifacts (a single vector grid cell producing up to four raster pixels).
    *   After exhausting multiple fixes within the Python `rasterio` library (grid snapping, nodata value changes, removing compression/tiling), the root cause was identified as a deep incompatibility within the library stack in the server's Python environment.
    *   **Definitive Solution:** Abandoned the Python script in favor of the core `gdal_rasterize` command-line tool. This immediately produced a correctly aligned raster with a 1-to-1 mapping between vector cells and raster pixels. This will be the standard procedure for all future rasterizations.
    *   **Grid Geometry Verification:** A subsequent check of the reprojected vector grid's geometry (`hotspots_global_abs_epsg8857.gpkg`) initially caused confusion, as the bounding box of individual cells was not 10km x 10km.
    *   **Final Validation:** Developed a new verification script (`verify_grid_area.py`) to measure the true geometric **area** of the reprojected polygons, not just their bounding box. This definitively confirmed that each grid cell has an area of **100 km²**, validating the integrity of our equal-area grid and resolving a long-standing point of uncertainty. The project's core spatial foundation is now fully verified.

---

### 2026-05-12
*   **Finalize `hotspot_synthesis.qmd` & Prepare for Interpretation:**
    *   Completed a major debugging and refinement pass on `analysis/hotspot_synthesis.qmd` to ensure it runs locally and produces clean, final outputs.
    *   Resolved numerous rendering errors, including TeX installation failures (by switching to HTML output), missing `kable()` function errors (by adding `library(knitr)`), and data type mismatches in summary tables.
    *   Significantly improved the population exposure plots by:
        *   Correctly handling and filtering income group categories to remove "NA" values from plots.
        *   Enforcing a canonical service order for facets.
        *   Switching to a fixed y-axis scale for better comparability across services.
    *   Enhanced the report's clarity by replacing the raw configuration code chunk with a clean, formatted summary table.
    *   The `hotspot_synthesis.qmd` notebook is now stable and produces all necessary summary tables and visualizations, paving the way for the final interpretation phase.
    *   Prepared a transfer prompt and a git commit message to checkpoint this progress before moving to a new chat session focused on `analysis/results_interpretation.qmd`.

---

## Project Overview & Goals

**Goal:** Quantify global change in multiple ecosystem services (ES) at ~10-km resolution over 1992–2020, identify **hotspots** of concerning change, and attribute those changes to environmental and socioeconomic drivers.

**Key Objectives:**
1.  Robust spatial extraction of ES changes bypassing Modifiable Areal Unit Problem (MAUP) artifacts.
2.  Identification of ES hotspots using Symmetric Percentage Change (to handle zero-baselines and capture local vulnerability).
3.  Integration of Land Cover Change (LCC) metrics to attribute ES decline to Land Conversion vs. Degradation.
4.  Socioeconomic characterization of hotspots via Kolmogorov-Smirnov (KS) tests.

## Current State: Version 1.3.2 (Visual Unification & Presentation Polish)

**Status:** The core pipeline architecture is mathematically validated, cleaned, and finalized. We have successfully unified the visual styling using canonical WWF colors across all flowcharts and plots, preparing everything for the final presentation.

**Active Focus & The Final Wrap-Up Plan:**
*   **Geographic Clustering:** Finalizing the geographic narrative of "Compound Risk" (Hotness) and "Disproportionate Burden" (Relative Intensity) using the consolidated `hotspot_synthesis.qmd` pipeline.
*   **Land Cover Change (LCC) Interpretation:** Reviewing the outputs of the "Drivers of Change" chunks in `hotspot_extraction.qmd` to define our "Attribution Gap" (conversion vs. degradation).
*   **Socioeconomic Interpretation (KS Tests):** Interpreting KS Test heatmaps and Cliff's Delta plots to profile the socioeconomic context of extreme ES decline.
*   **Presentation & Handoff:** Sharing final exported plots and datasets with co-authors via OneDrive, and drafting the final Key Takeaways and methodology sections.

## Key Challenges & Architectural Solutions (For Final Report)

This section highlights the major technical and methodological hurdles overcome during the pipeline's development, serving as a direct outline for the Methods paper.

*   **The Fragment Bug & Spatial Alignment:** *Challenge:* Bypassing C++ GEOS bottlenecks by exploding complex multipolygons into 1.67M fragments caused striping and duplicated data. *Solution:* Reverted to a mathematically safe `st_intersects` spatial joining and re-aggregation process (`group_by %>% summarise`), collapsing fragments back into pristine 10km parent cells to perfectly align data (v1.3.1).
*   **Spatial Extraction Scaling:** *Challenge:* `exactextract` memory leaks and C++ segmentation faults when processing massive, jagged regional multipolygons (e.g., Biomes). *Solution:* Adopted a "Hybrid Extraction" architecture—using `exactextract` for simple grids (10km) and rasterized `zonal_stats_toolkit` for complex regional polygons.
*   **Simpson's Paradox & MAUP:** *Challenge:* Observing "sign flips" where a region showed negative Absolute Change but positive Percentage Change. *Solution:* Documented the distinct spatial narratives. Mean Absolute Change captures systemic volume shifts (weighted by huge baselines), while Mean Symmetric Percentage Change captures widespread landscape footprint shifts.
*   **Zero-Baselines & Scale Bias:** *Challenge:* Absolute change is heavily biased by the size of the baseline ecosystem, and standard percentage change fails on zero-baselines. *Solution:* Transitioned to **Symmetric Percentage Change (SPC)** to normalize the data, capturing the true *intensity* of ecological response for Land Cover Change attribution.
*   **Aggregation Statistic — Mean for All ES Services (2026-07-02):** *Challenge:* The extraction pipeline produces both `_sum` and `_mean` columns for every ES service raster. Managing mixed `_sum`/`_mean` references across `process_data.qmd`, `calculate_bitemporal_change.py`, and downstream R scripts required constant manual attention and was a recurring source of bugs and confusion. *Decision:* Standardised on **`_mean` for all ES service variables** at the 10km grid-cell level. *Rationale:* (1) All input rasters are pre-normalised to per-hectare units (sourced from `base_years_ha/`), so the mean within each equal-area cell gives a valid, comparable per-ha rate. (2) For hotspot detection, SPC(mean, mean) = SPC(sum, sum) for equal-area cells — the choice does not affect rankings or results. (3) Regional totals for volumetric services are computed separately via Path A, which bypasses the grid entirely. *Implementation:* `services_slim.yaml` now specifies `op_stats: [mean]` only; stale `_sum` references removed from `calculate_bitemporal_change.py` and `process_data.qmd`. *Note:* Socioeconomic variables (GHS-POP, GDP) retain `_sum` — these are absolute counts/totals where summing is the correct interpretation and population exposure figures depend on this.

## Phase 4: Pre-Submission Polish & Repo Cleanup (2026-06-25 to 2026-07-02)

### Manuscript language & framing
*   **"Who bears the burden" → exposure framing:** Replaced "who bears the greatest burden" with "which populations and socioeconomic contexts face the greatest exposure" throughout paper, book preface, and conclusions. Distinction: the analysis measures *exposure co-occurrence*, not welfare impact or burden.
*   **Abstract shortened:** Reduced from ~320 to ~210 words. Removed SPC definition, expanded acronyms, and per-figure methodology from abstract. Kept 4 headline numbers (252K cells, 1.6×, 3.1B/7.6B, 76%). Research objectives updated to reflect WHY reframing ("is monitoring land cover alone sufficient?") and WHO framing.
*   **Paper conclusions stale numbers fixed:** 6.4B / 1.15× replaced with verified 3.1B in-situ → 7.6B connected (2.5×), consistent with Ch06 and presentation.
*   **"markedly" → "substantially/sharply":** Removed LLM-register word throughout paper and book preface.
*   **Sherman et al. (2026) DOI confirmed:** Verification notes removed from all files.
*   **MAUP citations added:** Openshaw (1984), Fotheringham & Wong (1991), Simpson (1951) added to paper references and Key References table in ch02.

### Book chapters updated
*   **Ch01 "Caveats & Transparency":** Expanded from 5 one-liners to 4 substantive paragraphs. Temporal snapshot now notes two-endpoint design, intermediate dynamics gap, 2020 aging, and future extension path. Attribution gap now precisely states "not co-occurring with extreme (top 5%) land cover conversion." "Natural variation" dropped as standalone (absorbed as named mechanism in attribution gap). "About the Analysis" methods section rewritten with correct 8-service list, dual-path description, and proper citations.
*   **Ch02 intro:** Tightened from one verbose paragraph to two short ones. InVEST now "models of eight ecosystem services"; "satellite land cover records" → "ESA CCI and C3S land cover classification maps at 300m"; "gridded socioeconomic layers" → named (population, GDP, HDI, Gini).
*   **Ch02 Pre-processing:** WGS84 / projection detail moved to ch09 Technical Appendix. Ch02 now states decisions plainly (3 short paragraphs). Coastal Risk column-name detail also moved to ch09.
*   **Ch02 Serviceshed section ADDED:** New `## Population Exposure and the Serviceshed Multiplier` section — was present in flowchart but had no dedicated methods text. Covers in-situ baseline, two routing pathways (hydrological + travel-time), multiplier definition, and stratification outputs.
*   **Ch02 Reproducibility:** Now shows both Path A (zonal_stats_toolkit / services_diff_ha_groupings.yaml) and Path B (exactextract / services_slim.yaml chain) execution steps. audit_claims.R included as step 6.
*   **Ch02 Summary:** Serviceshed component added as explicit fourth pillar (was missing despite "four-component framework" claim).

### Repo cleanup
*   **Python_scripts/:** archive/ created; deprecated scripts moved (summary_pipeline_rasterzones, summarize_cp_points_grid, temp_nature_access_diff, check_valid_geometries); dead files deleted (DEM_Mask, processed_data, import rasterio.py); README.md written with pipeline order and per-script descriptions.
*   **R/:** percentileR.R archived (superseded by get_hotspots.R); README.md rewritten with accurate file list and pct_change.R vs pct_change_calc.R dependency explanation.
*   **scripts/:** archive/ created; one-offs moved (gdal_rasterization.sh, plot_hex.R, enrich_grid_attributes.R — superseded by enrich_grid.py); duplicate make_attribution_map.R removed; README.md written with Validation / Reference Data / Output / Mapping / Archive sections.
*   **analysis_configs/:** archive/ created (zonal_stats_diff.yaml, global_ncp_base_ha.ini); README.md written documenting Path A / B / C config mapping; c_protection_synth.yaml path updated from interim/archive/ → interim/coastal_protection_rasters/.
*   **README.md (root):** Fixed critical errors — Quick Start was calling deprecated `summary_pipeline_rasterzones.py` with non-existent `services_raster.yaml`; results_interpretation.qmd references removed (notebook deleted); ES services list updated to correct 8-service table with variable names; audit_claims.R added to pipeline flow.

## Phase 4 continued: Paper/Book polish pass 2 (2026-07-03)

### Data claim verification
*   **Sub-Saharan Africa error caught and corrected (2026-07-03):** The preface and ch08 erroneously stated SSA carries a "disproportionate share" of hotspot burden. Verified from data: SSA relative intensity = 0.78× (BELOW expected given land area). Only LAC (1.40×) and EAP (1.30×) exceed 1.0×. Corrected in index.qmd and ch08. Root cause: AI-generated plausible-but-unverified claim. Prompted systematic review.
*   **5×/8× claims corrected (2026-07-03):** Presentation stated "5× higher population density than median background" and "8× for 3+ service cells." These are actually **serviceshed multipliers** (connected beneficiaries ÷ in-situ for 2+/3+ compound cells) from ch06 data, not density ratios. Fixed in presentation slide and speaker notes.
*   **SPC examples corrected (2026-07-03):** ch02 examples stated 100→0 = -100% and 0→100 = +100%. Both wrong. Correct: **100→0 = -200%** (theoretical minimum) and **0→100 = +200%** (theoretical maximum). Fixed with explanation of ±200% bounds as a feature of SPC.
*   **audit_claims.R expanded (2026-07-03):** Now 8 checks (was 5). Fixed critical bug: income ratio check was comparing "Low income" vs "High income" but the 1.6× claim is "Lower-middle income" vs "High income OECD." Added checks for hotspot count, population exposure, 5×/8× disambiguation, KS test count.

### Paper polish
*   **Abstract rewritten** to ~210 words; closing sentence reframed from limitation ("cannot causally partition") to contribution ("highlights limits of LC monitoring as sole proxy"); nature accounting angle added.
*   **Research objectives** updated with WHAT/WHERE/WHO/WHY labels, WHO reframing ("which socioeconomic contexts"), WHY reframing ("is monitoring LC alone sufficient").
*   **Author list**: Stephen Polasky added; "[Additional Authors TBD]" placeholder retained.
*   **KS methods paragraph** cleaned of AI slop: "utilizing" → "using"; "artificially inflates" → precise language; "matched counterfactual background" → "background of typical stable conditions"; Andam et al. citation removed (was misapplied — Andam is about PA effectiveness, not KS background design); "Type I error propagation across multiple simultaneous testing dimensions" → "Type I error across 40 simultaneous tests (8 services × 5 covariates)."
*   **39/40 result** added to paper KS section, ch02 callout, presentation notes, and methodology.md.
*   **FDR plain-language explanation** added to ch02 (callout), methodology.md, and presentation speaker notes.
*   **Spatial co-occurrence justification** added: explicit paragraph explaining that the comparison operates on ranked binary overlays, not raw values — making the comparison between continuous-derived (SPC) and categorical-derived (Pontius) hotspots methodologically defensible. Becky callout added for literature validation.
*   **Paper conclusions** stale numbers fixed: 6.4B / 1.15× → 3.1B / 7.6B / 2.5× (consistent with ch06).
*   **Data sources paragraph**: verbose pipeline detail about excluded cells removed from paper (now one clause).
*   **Paper structure callout** added for Becky (target journal question → drives restructure decision).
*   **diffeR → Pontius contingency matrix approach**: section heading and framing corrected in ch02 and ch05. diffeR correctly positioned as implementation tool, not the methodology itself.

### Book polish
*   **ch01 How to Use**: corrected chapter numbers (ch4 WHERE was missing from all paths; ch7 was wrong labelled as "WHY"); four reading paths now accurate.
*   **ch01 Users section**: Nature accounting practitioners added as a distinct user type (GEP, SEEA, TNFD).
*   **ch01 About the Analysis**: authors updated (Polasky added).
*   **ch02 intro**: "InVEST biophysical models" → "InVEST models of eight ecosystem services"; "satellite land cover records" → "ESA CCI and C3S land cover classification maps at 300m"; "gridded socioeconomic layers" → named covariates.
*   **ch03**: Stacked three-section layout → tabset (World Bank Region / Income Group / WWF Biome).
*   **ch08 WHO section**: "Hundreds of millions" → verified 3.1B / 7.6B figures.
*   **ch08**: GEP / nature accounting section added ("For Nature Accounting Frameworks") covering GEP, SEEA EA, TNFD.
*   **_quarto.yml**: Polasky added to author line.
*   **index.qmd**: Sub-Saharan Africa claim corrected; universal signal bullet rewritten in plain language; "who bears burden" → "which populations and socioeconomic contexts"; attribution gap typo fixed; nature accounting user type added.

### Documentation
*   **methodology.md KS section** rewritten: plain-language explanations of median background rationale, Cliff's Delta vs p-values, FDR correction with 39/40 result and ecological interpretation of the one non-significant combination.
*   **presentation.qmd**: 5×/8× serviceshed multiplier framing corrected; LAC per-service values flagged for verification; FDR plain explanation added to speaker notes.

## Reference Information
*   **Environment Notes:** Local machine: Lenovo (Windows 11) | Remote: lilling (VS Code Remote SSH) | AI assistant: Gemini Code Assist / Copilot
*   **Active Entry Points:** `analysis/process_data.qmd`, `analysis/hotspot_extraction.qmd`, `analysis/hotspot_synthesis.qmd`, `analysis/KS_tests_hotspots.qmd`
*   **Known Issues / Gotchas:** Hotspot rules (loss vs gain services) must remain centralized in `HOTS_CFG`. Be careful not to mix interpretive direction (good/bad change) with magnitude summaries.

---

## 🛠️ Merged Worklog: Zonal Stats Toolkit (Pre-Integration)

*This section consolidates the historical worklog from the `zonal_stats_toolkit` repository. Moving forward, all notes for both the Python extraction engine and the R/Quarto synthesis pipeline will be tracked in this single document.*

### Key Methodological Milestones (Toolkit)
*   **Spatial Dissolve vs Tabular Grouping:** Proved that geographic dissolves prior to extraction cause massive OOM errors and slowdowns. The optimized design uses a high-res grid and tabular aggregations post-extraction.
*   **Pollination Discrepancy:** Identified that $\text{Mean}_{2020} - \text{Mean}_{1992}$ diverges from $\text{Mean}_{\Delta}$ for Pollination due to NoData mask misalignments (shifting agricultural footprints).
*   **Legacy vs Optimized Validation:** Achieved 0.9975 Pearson Correlation between legacy GDAL rasterize and the optimized `exactextract` pipeline. Variance is strictly due to boundary-pixel handling (`ALL_TOUCHED` artifacts). Optimized pipeline safely calculates exact fractional overlap.
*   **Raster Conversion Overhaul:** Refactored `convert_to_ha.py` to use `rasterio` and `WarpedVRT` in small blocks (sequential `max_workers=1` with `BIGTIFF=YES`), definitively resolving memory and write failures on global rasters.

### Chronological Toolkit Notes (Jan - Mar 2026)
*   **Mar 24:** Visualization refactor for bitemporal difference plots. Switched to SEM for error bars and filtered bottom 10% micro-states to prevent variance skewing. Developed `append_ratios.py` for missing data.
*   **Mar 20:** Runner config enhancements (skip jobs).
*   **Mar 13 (Bi-Temporal Math):** Implemented `calculate_bitemporal_change.py` using `osgeo.ogr` directly on the GPKG. This calculates Absolute and Symmetric Percentage Change (SPC) via raw SQL updates, explicitly bypassing memory-intensive `geopandas` operations and `sqlite3` limitations to prevent OOM crashes on global grids.
*   **Mar 13 (Validation):** Built validation framework `compare_gpkg_columns.py` (NRMSE metrics). Enforced runner determinism.
*   **Jan 28-29:** Coastal protection vector attribute integration (`Rt`, `Rt_ratio`).
*   **Jan 20-22:** Disk space management, permission fixes, and visualization layout refinements.
*   **Jan 12:** Docker execution bypassing host permissions, fixing NaN handling, and output column filtering.

---

### 2026-05-08 (Urgent Task: Rasterization)
### 2026-05-08 (Major Conceptual & Analytical Refinements)
*   **Nuanced Driver Mapping:** Finalized the land cover change driver analysis by significantly refining the classification logic in `make_lcc_driver_map.R`. Replaced the generic "Multiple Overlapping Drivers" category with specific, policy-relevant transitions like "Deforestation for Cropland," "Savannization / Pasture," and "Grassland to Cropland."
*   **Grassland Dynamics:** Fully integrated both "Grassland Loss" and "Grassland Expansion" as distinct drivers into the mapping scripts, ensuring these critical rangeland dynamics are no longer masked.
*   **Refined Attribution Terminology:** Replaced the presumptive "Degradation-driven (Stable Land Cover)" category in `make_attribution_map.R` with the more accurate and defensible term **"Attribution Gap (Change without Conversion)"**. This new label correctly describes ES hotspots that are spatially decoupled from major land conversion.
*   **Clarified Metric Interpretation:** Added a detailed methodological note to `make_lcc_driver_map.R` to explicitly state that the land conversion metrics are calculated as a percentage of the *total 10km cell area*, clarifying the interpretation of "landscape transformation intensity".
*   **Expanded Socioeconomic Analysis:** Enhanced `hotspot_synthesis.qmd` to include absolute population exposure analysis by GDP and Gini quartiles. Removed the redundant 'Built Area' variable from the `KS_tests_hotspots.qmd` analysis.
*   **Visualization & Documentation Polish:** Increased map output resolution to 600 DPI for clarity, improved color palettes for distinguishability, and updated `Key_Takeaways.md` and other documentation to reflect all conceptual shi*   **Hotspot Count Rasterization:** Developed a new Python script `Python_scripts/convert_hotspot_gpkg_to_raster.py` to convert the vector-based hotspot count maps (from `hotspots_global_pct.gpkg`) into a GeoTIFF raster. This provides a raster-based output of hotspot frequency, as urgently requested.
*   **Hotspot Count Rasterization:** Developed a new Python script `Python_scripts/convert_hotspot_gpkg_to_raster.py` to convert the vector-based hotspot count maps (from `hotspots_global_pct.gpkg`) into a GeoTIFF raster. This provides a raster-based output of hotspot frequency, as urgently requested.

---

### 2026-05-11
*   **General-Purpose Rasterization Utility:** Refactored the specialized `convert_hotspot_gpkg_to_raster.py` script into a flexible, general-purpose command-line tool named `vector_to_raster.py`.
    *   The new script is no longer hardcoded to specific "hotspot" columns. It now accepts a list of columns to rasterize via a required `--columns` argument.
    *   Enhanced flexibility by adding command-line arguments to control the output `--resolution`, target `--crs`, `--nodata` value, and raster `--dtype` (e.g., `float32`, `int16`).
    *   This provides a robust and reusable utility for converting any attribute from a vector file into a GeoTIFF, addressing the need for a more reliable rasterization method than manual QGIS operations for our various grid-level summary files.

*   **Pipeline Cleanup & Housekeeping:**
    *   Disabled the creation of large intermediate synthesis files (`10k_grid_synth_all.gpkg`, `10k_grid_ES_change_benef.gpkg`) in `process_data.qmd` by default. These files were useful for debugging but are not required for the final analysis and can be recreated if needed. This change will keep the `processed/` directory cleaner.
    *   Documented the legacy status of `prepare_data.qmd` with a note explaining it is a one-time setup script for the base grid and not part of the routine analytical workflow.
    *   Confirmed that existing intermediate files can be safely deleted to free up disk space.

*   **Documentation Consolidation:** Overhauled the project's documentation to eliminate redundant `README` files and establish a clear, maintainable structure.
    *   Consolidated all high-level information into a single, comprehensive root `README.md`.
    *   Created a central `docs/` directory to house detailed, long-form documentation.
    *   Moved content from various `README_*.md` files into `docs/methodology.md`, `docs/data_dictionary.md`, and `docs/runbook.md`.
    *   Explicitly documented the key methodological distinction between the project's "difference of aggregates" approach (Path B) and the alternative "aggregate of differences" (Path A) in `docs/methodology.md`.

---

## Chronological Log (Newest to Oldest)

### 2026-05-05
*   **Visualization Consistency:** Updated `hotspot_synthesis.qmd` to ensure all "hotness" and "exposure" bar charts use a consistent red intensity color scale (`scale_fill_distiller`) mapped to the value, rather than categorical colors for the groups. This improves visual coherence across the analysis.
*   **Code Health:** Added the `group_palettes` object definition to `hotspot_synthesis.qmd` to resolve a missing object error that was causing rendering to fail.
*   **Visual Unification & Cleanup:** Systematically removed redundant "main report" plotting blocks and ensured the "High income: nonOECD" category is consistently and globally filtered out from all visualizations in `hotspot_extraction.qmd` and `hotspot_synthesis.qmd` to reduce noise.
*   **LCC Grasslands Integration:** Integrated "Model 3: Grassland Loss" into the `LC_change_granular.qmd` pipeline, adding a specific reclassification matrix to explicitly track the conversion of grasslands to other uses.
*   **Narrative Refinement:** Updated `Key_Takeaways.md` to incorporate the "Spatial Attribution / Degradation" findings and highlight the new Grassland Loss model, aligning with the latest feedback.
*   **Plotting Iteration (Synthesis & Volumetric Plots):** Reverted the combined volumetric plots in `hotspot_extraction.qmd` back to separate figures for absolute and percent change. Fixed the y-axis labels in the `hotspot_synthesis.qmd` bar charts to display the numeric key instead of being blank, improving readability.

### 2026-05-06
*   **Boxplot Unification & Refinement:** Refactored the entire boxplot generation logic in `hotspot_extraction.qmd` into a single, unified function. This ensures all boxplots (volumetric, ratio, coastal) have a consistent aspect ratio, a universal numeric legend with a key at the bottom, and larger, more readable fonts. This resolves previous inconsistencies and simplifies future maintenance.
*   **Data Dictionary Updates:** Improved the data dictionaries in `KS_tests_hotspots.qmd` and `hotspot_synthesis.qmd` to provide clearer, more accessible definitions for key statistical terms and output table columns, enhancing the project's usability for collaborators.
*   **Granular LCC Integration:** Verified and finalized the integration of the "Grassland Loss" model into `LC_change_granular.qmd`, ensuring its results are correctly consolidated into the final output GeoPackage.

### 2026-05-04
*   **Infrastructure & Environment:** Resolved persistent VS Code Remote SSH synchronization and connection hangs that have been occurring since last week on `lilling`.
    *   *Diagnosis:* The VS Code server backend was fragmenting and leaving behind orphaned `node` processes for language servers (Pylance, Quarto) and the core RPC server, which blocked new connections.
    *   *Troubleshooting:* Implemented a targeted process-kill command (`pkill -u jeronimo -f .vscode-server`) via terminal to forcefully clean up the hung background processes. This successfully resets the remote connection state without requiring physical or system-level reboots of the server by IT.
*   **Plotting Refinement:** Updated `compare_and_plot_changes.R` to exclude the "High income: nonOECD" group from the main report's bar plots to remove outliers and clarify the primary trends, as discussed in the last review meeting.
*   **Housekeeping:** Identified and removed a redundant, outdated copy of `hotspot_extraction.qmd` that was incorrectly located in the `R/` directory. Confirmed `analysis/hotspot_extraction.qmd` is the correct, canonical version.

### 2026-05-02
*   **Infrastructure & Sync:** Diagnosed and bypassed silent VS Code Remote SSH hangs on `lilling` without a hard reboot (safely wiped corrupted `~/.vscode-server`). Established a `tar`-over-SSH sync workaround to bypass strict Windows IT firewalls lacking `rsync`.
*   **Python Engine Optimization:** Refactored `zonal_stats_toolkit/runner.py` to concurrently schedule both raster and vector tasks in the execution graph, significantly improving parallelism ahead of the v1.4.0 merger.
*   **Visual Polish (Boxplot Color Ramps):** Solved the `ggplot2` global scale dominance issue in the plotting scripts (`hotspot_extraction.qmd`). Implemented localized data normalization (`scales::rescale`) so canonical intensity colors (Reds) dynamically scale from 0 to 1 strictly within their respective facets.
*   **Methodology Documentation:** Updated `README_Methodology.md` to formally transition "Path C" from a hypothetical "Future Analysis" into a completed "Validation Analysis," explicitly confirming that the grid-level hotspots mathematically align with pixel-level differences.
*   **Feedback Manifesto Audit:** Cross-referenced meeting notes to finalize terminology ("Multi-service Decline" over "collapse"), prepared the biome-faceted scatterplots for the "Attribution Gap", and confirmed non-OECD outlier exclusions for main report boxplots.
*   **Next Steps Planned:** Ready to implement "Model 3: Grassland Loss" in `LC_change_granular.qmd` to accurately track Forest-to-Grassland and pristine Grassland-to-Cropland transitions.

### 2026-05-01
*   **Post-Meeting Debrief & Cleanup:** Successfully presented the "Drivers of Change" (LCC Attribution) and "Who is Affected" (Socioeconomic / KS Tests) sections to Steve and Becky. The compound risk mapping, red-intensity boxplots, and LCC driver correlations resonated strongly. 
*   **De-escalating "Rescue Mode":** Safely stripped out local hardcoded `here("home", "jeronimo", ...)` fallback paths from all R mapping scripts (`make_socieconomic_maps.R`, `make_attribution_map.R`, `make_lcc_overview_map.R`, `make_hotspot_count_map.R`). Returned the pipeline to universally use `data_dir()` for server-side processing on `lilling`.
*   **Server Stability & Repackaging:** Remotely rendered `hotspot_extraction.qmd` on `lilling` to establish the final, single source of truth. Repackaged the `global_ncp_data_archive.tar.gz` archive with the updated Data Dictionary, preparing the data outputs for distribution without plot files.
*   **Path to v1.4.0 (The Merger):** With Pillar 4 and Pillar 5 validated, the repository is officially ready for the massive architectural merge. The upcoming `v1.4.0` will natively integrate the Python `zonal_stats_toolkit` directly into the `global_NCP` repo, creating a single, unified pipeline repository.

### 2026-04-30
*   **Visual Unification (4+ Hotspot Cap):** Standardized the compound risk narrative by capping overlapping hotspot counts at "4+" across both spatial maps and regional stacked barplots. Applied a unified semantic color ramp (Yellow to Dark Red) across `make_hotspot_count_map.R` and `hotspot_synthesis.qmd` to ensure immediate visual recognition of extreme compound risk.
*   **"First Look" Overview Maps:** Created minimalist, high-resolution global overview maps (solid red, no heatmaps) for both absolute and percentage hotspots to serve as clean anchor visuals for the presentation slide deck.
*   **Server Rendering & Single Source of Truth:** Pushed all visualization updates to the remote repository and successfully re-rendered the canonical `hotspot_synthesis.qmd` pipeline on the Lilling server, ensuring all plots and CSVs remain perfectly in sync.
*   **Next Immediate Step:** Diving into the "Attribution Gap" (Pillar 4) by analyzing Land Cover Conversion (LCC) overlaps using `lcc_es_hotspot_overlap_pct.csv` to build out the narrative for Coastal Risk (driven by Urban Expansion) and Pollination (driven by Forest Loss/Cropland Expansion).

### 2026-04-28
*   **Spatial Alignment Crisis Averted**: Diagnosed and eliminated a critical `seq_len()` reassignment bug in variable-length datasets across `process_data.qmd`, `hotspot_extraction.qmd`, and `hotspot_synthesis.qmd` that was scrambling downstream spatial joins and creating "striped" artifacts in maps. Enforced strict `stop()` fallbacks to prevent silent spatial corruption.
*   **Emergency "Rescue Mode" Implementation**: Successfully extracted and utilized a 2.2GB data archive (`global_ncp_data_archive.tar.gz`) to bypass long-running spatial joins under a strict deadline, temporarily routing scripts to safely read local `plt_long.rds` and GPKGs.
*   **Visualization Overhaul (Barplots & Intensity)**: Replaced confusing categorical color ramps in signed change bars and intensity plots with strict, semantic "Good (Green) / Bad (Red)" logic. Implemented an automatic alphanumeric `[ID]` key system on the y-axis to perfectly map subregions to legends.
*   **Dual-Metric Driver Analysis**: Upgraded `make_attribution_map.R` and the `hotspot_extraction.qmd` land-cover driver overlap chunks to loop over both Absolute (`abs_chg`) and Percentage (`pct_chg`) metrics. Programmatically recreated the massive `global_attribution_gap_map.png` directly in R to eliminate QGIS bottlenecks.
*   **Upcoming Priorities (Next 48 Hours)**:
    *   **Drivers**: Review and compare the newly generated `abs_chg` vs `pct_chg` scatterplots and heatmaps to finalize the "Attribution Gap" narrative.
    *   **Equity**: Review the "Absolute Population Exposure" (affected people) outputs generated by `hotspot_synthesis.qmd`.
    *   **Socioeconomics**: Perform a final validation pass on the KS analysis results.
    *   **Presentation**: Finalize the "Why" (drivers) and "Who" (people) sections of the presentation slide deck.
*   **Version 1.3.2 Release**: Unified pipeline visual styling (Mermaid flowcharts, spatial maps, and plots) to strictly use canonical WWF colors. Cleaned up redundant documentation and finalized the narrative methodology structure for the presentation slide deck.

### 2026-04-27
*   **Hotspot Boxplot Pipeline Overhaul**: Resolved critical "silent failures" in `hotspot_extraction.qmd` where Quarto intercepted error messages and skipped plot generation due to missing `plt_long` attributes. Implemented a robust on-the-fly attribute join from the master grid (`AOOGrid_10x10km_land_4326_clean.gpkg`), fixed `dplyr` dynamic scoping issues (`across(all_of())`), and added aggressive `stderr()` diagnostic logging.
*   **Coastal Visualization Fix**: Refactored coastal service boxplots to use pre-calculated 1.5*IQR whiskers (`stat="identity"`) instead of `outlier.shape=NA`. This permanently solves the issue of invisible outliers stretching the y-axis and causing scattered point artifacts.
*   **PDF Image Resolution**: Fixed LaTeX pathing during document rendering to ensure the freshly rendered, canonical-colored plots are correctly embedded into the final PDF.
*   **KS Test Enhancements**: Deprecated legacy `cfg$paths` in favor of `data_dir()` across the pipeline, and successfully integrated **Built Area** (`GHS_BUILT_S_E2020_mean`) into the socioeconomic covariate analysis.
*   **Methodology Flowchart (`workflow.qmd`)**: Developed a presentation-ready, high-resolution Mermaid.js flowchart documenting the end-to-end analytical pipeline. Mapped the dual-path extraction architecture (Regional Zonal Summaries vs. 10km Grid Analysis) and perfectly aligned the final deliverables with the slide deck's narrative structure (WHAT, WHERE, WHY, WHO). Bypassed strict parsing bugs in Mermaid v11.6.0 to implement custom WWF color palettes, transparent overlays, and thick routing arrows.

### 2026-04-23
*   **Workspace Integration**: Configured a VS Code Multi-root Workspace bringing `global_NCP` and `zonal_stats_toolkit` side-by-side for unified development.
*   **Documentation Unification**: Merged the historical worklog from the `zonal_stats_toolkit` repository into the central `WORKLOG.md` to officially centralize project tracking.
*   **Aesthetic Unification & Fixes**: Applied universal canonical color palettes for Biomes, WB Regions, and Income Groups across the `global_NCP` and `zonal_stats_toolkit` plotting scripts. Fixed exact string matching issues for Income Groups with numeric prefixes.
*   **Contextual Mapping**: Developed `generate_context_groupings_map()` to produce a 4-facet overview map of all geographic groupings, providing a clean visual baseline for the slide deck introduction.
*   **Equity Analysis (Impact Tier)**: Audited codebase for population metrics and implemented the `Absolute Population Exposure` module in `hotspot_synthesis.qmd`. This calculates the total number of people living in top 5% ES hotspots, segmenting the vulnerable populations by HDI bin and Income Group.
*   **Narrative Consistency**: Enforced standard terminology: "Relative Socioeconomic Shift" for KS statistical testing and "Absolute Population Exposure" for raw population counts.

### 2026-04-13
*   **Data Alignment Bugfix:** Resolved a fatal desynchronization bug in `hotspot_synthesis.qmd` where missing `fid` identifiers in the master attribute grid caused silent Quarto crashes during attribute joins.
*   **Technical Debt Documentation:** Formally documented the "Fragment Bug" spatial join bypass as technical debt across `process_data.qmd`, `README_Methodology.md`, and `README_pipeline.md`. Outlined the V1.4.0 plan to replace it with a robust `orig_fid` tabular join.
*   **Data Packaging:** Created a lean 2.2GB final data archive (`global_ncp_data_archive.tar.gz`) for co-author handoff. It strictly includes the analysis-ready `processed/` datasets, `outputs/` plots, `vector_basedata/` grids, and a standalone `README`. Excluded all raw/intermediate raster data to ensure easy sharing.
*   **Presentation Strategy:** Outlined the final slide deck structure for co-authors, focusing heavily on Compound Risk (Hotness), Disproportionate Burden (Relative Intensity), and the "Attribution Gap" (Land Conversion vs. Degradation).
*   **Housekeeping:** Cleaned up residual Git artifacts and removed deprecated scratch scripts.

### 2026-04-10
*   **Final Synthesis & Key Takeaways:** Successfully consolidated Intensity, Share, Relative Intensity, and Multi-service "Hotness" (Compound Risk) into a single, bulletproof pipeline (`hotspot_synthesis.qmd`).
*   **Codebase Grooming:** Officially deprecated `hotspot_intensity.qmd` and `hotspot_multiservice.qmd`, removed dead code in Python utilities, and prepared the repository for co-author handoff via secure, read-only OneDrive sharing.
*   **Visualization Polish:** Re-engineered compound risk and relative intensity bar charts to automatically loop over all canonical groupings, generating presentation-ready outputs for the final report.

### 2026-04-08
*   **Pipeline Fixes (The Fragment Bug):** Discovered that Python's `gdf.explode()` was fragmenting the 1.5M grid cells into 1.67M jagged pieces to bypass GEOS bottlenecks. This caused severe striping (dropped cells) and impossible hotspot counts (up to 180) due to duplicated data.
*   **Spatial Join & Re-aggregation Patch:** Implemented a robust `st_intersects` spatial join and re-aggregation (`group_by %>% summarise`) in `process_data.qmd`. This mathematically collapses all fragments back into their pristine 10km parent cells, ensuring perfect 1:1 data alignment. Striping is completely eliminated, and max hotspot counts are strictly capped at 8 (the total number of services). Stable extraction pipeline version tagged as `v1.3.1`.
*   **V2 Technical Debt Documentation:** Formalized the "V2 Simplification Plan" to use `orig_fid` (preserved from Python prior to explosion) to bypass spatial joins entirely in future analysis updates (`v1.4.0`).

### 2026-03-24
*   **LCC Driver Correlation Improvements:** Upgraded Land Cover Change (LCC) vs Ecosystem Service scatterplots to use 2D density heatmaps (`geom_bin2d`) with a logarithmic viridis scale to solve massive overplotting. Restructured plots to a faceted 3x3 canonical layout and removed deprecated `USLE`/`N_retention` metrics.
*   **Methodological Documentation (Absolute vs. Percentage Change):** Documented the critical decision to use Symmetric Percentage Change (SPC) rather than Absolute Change for attributing ES declines to drivers. Absolute change is heavily biased by the baseline ecosystem size (e.g., a 5% loss in a massive forest yields a larger absolute drop than a 100% loss in a tiny forest). SPC properly normalizes the data to reveal the *intensity* of the ecological response relative to the local baseline.
*   **KS Socioeconomic Analysis Validation:** Verified the `KS_tests_hotspots.qmd` pipeline. Confirmed the successful execution of balanced sampling (`comparison_mode = "median"`) to correct the 5% vs 95% class imbalance. Updated KS Heatmaps and Cliff's Delta plots to enforce the canonical 3x3 service ordering for presentation consistency.
*   **Hotspot Visual Polish:** Unified boxplot aesthetics in `hotspot_extraction.qmd` to remove arbitrary color maps, using a consistent clean `gray95` fill. Added horizontal, ranked (Top 10/Bottom 10) boxplots for country-level aggregations to drastically improve legibility.
*   **Automated Faceted Mapping:** Developed `make_faceted_maps.R` to fully automate the generation of spatial maps across 4 groupings (World Bank Region, Income Group, Biome, Country). Implemented a dynamic Cartography Rule Engine for automatic color ramp selection (diverging/sequential, goods/damages) and utilized `patchwork` for complex multi-scale layout stitching. Applied Equal Earth projection (`EPSG:8857`) and 1st/99th percentile outlier trimming to ensure high-quality visualization of absolute change.
*   **Documentation Refinement (Conceptual Framing):** Harmonized `README_Methodology.md` with explicit definitions of a "hotspot" (framing it as a *relative extreme* ranking label rather than an absolute threshold or evidence of cause). This analogy (the "marathon finisher") will directly support the framing of the final methods paper.

### 2026-03-20 (Pre-v1.3.3)
*   **Architectural Validation (Spatial Extraction Strategies):** Ran a test using `exactextract` in Python for large regional groupings (Biomes/WB Regions) by exploding them into 85,000 fragments. It ran for over 33 hours without finishing. Definitively proved `exactextract` is unscalable for massive regional groupings. Permanently adopted hybrid approach (`zonal_stats_toolkit` for regions, `exactextract` for 10km grids). Drafted open source feature request for C++ level `groupby`.
*   **Methodological Pivot (True Regional Baselines):** Configured regional base-year extraction to strictly bypass the 10km grid. By summarizing directly from the per-hectare rasters to the large spatial units, we bypass MAUP and grid-level division-by-zero artifacts.
*   **Pipeline & Cache Fixes:** Resolved Quarto caching trap in `KS_tests_hotspots.qmd` and re-enabled hotspot export chunks.
*   **Housekeeping:** Archived legacy QA/QC validation scripts.

### 2026-03-18
*   **Difference Analysis Pipeline Fixes & Completion:** Resolved persistent C++ `Segmentation fault` crashes in exactextract backend caused by microscopic topological errors. Implemented aggressive pre-processing. Completed "Mean of Differences" (Path C) extraction for regional groupings. Created `aggregate_yaml_outputs.py` for mathematical recombination.

### 2026-03-16
*   **Bi-temporal Change Validation:** Successfully cross-validated Symmetric Percentage Change (SPC) calculations between the R pipeline (`process_data.qmd`) and the Python SQLite pipeline (`calculate_bitemporal_change.py`). Both produced mathematically identical results. Confirmed R pipeline as primary workflow due to in-memory speed.

### 2026-03-10
*   **Data Consolidation (Path B):** Finalized primary base-year services dataset (`interim/10k_grid_services_base.gpkg`) using per-hectare corrected base year rasters. Bumped analysis version to **v1.2.1**.
*   **Difference Analysis (Path C):** Completed `summary_pipeline_landgrid.py` run on hectare-normalized difference rasters to establish the "Mean of Differences" dataset. Created `analysis/Consolidation.qmd` to load and validate outputs from Path B vs Path C.

### 2026-03-09
*   **Final Base Year Extraction (Per Hectare):** Initiated fresh run of summary statistics extraction for 1992 & 2020. Corrected volumetric variables to "per hectare" basis for global consistency. Configured `services_diff_ha.yaml` for Path C analysis.

### 2026-03-04
*   **V2 Pipeline Optimization:** Implemented caching for `plt_long`, added fallback logic for LCC driver column names, optimized GPKG export, and corrected export loops in `hotspot_extraction.qmd`. Synchronized continent/biome filters in KS tests.

### 2026-03-03
*   **V2 Pipeline Debugging:** Identified and fixed corrupt `grid_fid` issue causing NAs in `10k_change_calc_v2.gpkg`. Launched full V2 hotspot extraction. Created `compare_hotspots_v1_v2.qmd` to compute Jaccard Index overlaps between methodologies.

### 2026-02-27
*   **V2 Pipeline Implementation:** Created `analysis/process_zonal_stats_v2.qmd` to calculate Symmetric Percentage Change. Updated extraction/KS notebooks to be version-aware (`input_gpkg` and `output_suffix` params).

### 2026-02-24
*   **Refined Granular LCC Workflow:** Created `LC_change_preparation.qmd` for raw ESA/C3S extraction and 9-class reclassification. Updated `LC_change_granular.qmd` and removed testing limits. Launched 48h global extraction in `screen` session. Created `viz_granular_lcc.qmd`.

### 2026-02-20
*   **Granular LCC Analysis:** Created `LC_change_granular.qmd` to implement specific driver models (Forest Loss, Expansion) using `diffeR` metrics. Parameterized input GPKG path in `hotspot_extraction.qmd` for workflow flexibility.

### 2026-02-17
*   **LCC Pipeline Finalization:** Fixed `fid` vs `grid_fid` conflict in `LC_change.qmd`. Fixed grouping aggregation logic to generate `lcc_summary_by_group.csv`. Implemented chunked processing (50k cells/chunk).

### 2026-02-13
*   **LCC Integration & Documentation:** Validated `hotspot_extraction.qmd` logic for LCC overlap (Drivers of Change). Updated documentation to formally include the LCC pipeline and `diffeR` methodology.

### 2026-02-11
*   **Strategic Narrative / Pitch:** Defined the "Drivers of Change" strategy to attribute hotspots to Land Conversion (via `diffeR`) versus Degradation/Intensification.

### 2026-02-10
*   **Land Cover Change Integration:** Shifted focus to attributing hotspots. Created `analysis/land_cover_change.qmd` to compute binary transitions (Natural/Transformed) from ESA 300m maps.

### 2026-02-04
*   **KS Analysis Finalization & Methodology Refinement:** Optimized data pivoting in KS tests, implemented "signed power" transformations for plots, centralized configurations, refined groupings (removed `region_un` and `continent`), and documented Sum vs. Mean aggregation logic.

### 2026-02-02
*   **Hotspot Intensity & Multi-service Analysis Fixes:** Updated `hotspot_intensity.qmd` to calculate against total area and implemented Relative Intensity metric. Fixed setup chunks and alphabetical ordering in `hotspot_multiservice.qmd`.

### 2026-01-31
*   **Refactoring and Scope Refinement:** Initiated refactoring of `Consolidation.qmd` into `prepare_data.qmd` and `process_data.qmd`. Focused groupings on `income_grp`, `region_wb`, and `WWF_biome`.

### 2026-01-21
*   **Technical Issue Resolution:** Confirmed "fat tail" and bi-modal distributions are inherent properties of Symmetric Percentage Change (SPC). Investigated persistent sign flips (MAUP artifact). Added "Hotspot Area Analysis".

### 2026-01-19
*   **Ratio Calculations:** Created `calculate_ratios.py` to generate reliable sediment and nitrogen retention ratios with parallelized, tiled processing and `BIGTIFF=YES` support. Added automated statistical checks.

### 2026-01-16
*   **Repository Cleanup:** Archived legacy R zonal stats workflows (`zonal_stats.qmd`, `asign_ids_grid.qmd`).

### 2026-01-09
*   **Difference Rasters:** Implemented `batch_raster_diff.py` to calculate 2020-1992 difference rasters to support the transition to `zonal_stats_toolkit`.

### 2026-01-07
*   **Sign Flip Resolution:** Resolved absolute vs percent polarity issues by centralizing logic and normalizing service names.

### 2026-01-06
*   **Bug Fixes & Handoff:** Fixed `c_fid` drop bug in `Consolidation.qmd`, normalized service names, bumped to v1.0.1. Extracted pipeline overview to `README_pipeline.md`.

### 2026-01-05
*   **AI Context Migration:** Created `ai_context.md`, migrated to AI assistant (Copilot / Gemini).
`n### 2026-06-16`n- **Documentation Clarification:** Standardized the definition of 'hotspot' across the repository (including `README.md`, manuscript `index.qmd`, `paper_draft.qmd`, methodology, and book chapters) to explicitly state it is the top 5% of grid cells by rank, not a value-based percentile.
