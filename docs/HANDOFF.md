# CONTEXT: global_NCP rolling handoff

**Maintenance note**: this file is updated in place, not superseded by a new dated file each
session. When resuming or wrapping up, edit this doc directly — update stale sections, fold in
new findings, don't create `HANDOFF_<date>.md`. Paste this whole file into a fresh Claude Code
session in `c:\projects\global_NCP` to resume.

*Last updated: 2026-09-21 (final) — report considered done by the user, sent to WWF-SIPA, SWY thread genuinely closed. See this entry first.*

## LATEST (2026-09-21, final) — SWY Philippines thread closed out; user is sending the report to WWF-SIPA and using it as her next meeting's topic

Report went through several more real rounds after the factorial result landed, each a substantive
user-driven correction, not polish: reframed around an explicit two-part question (pipeline
correctness; if results differ, can we say why); added the "model-to-model, not validated against
reality" caveat after the user caught the original framing being close to circular; split future
work into "fix regardless" (tropical CN correction, crop calendar) vs. "real validation" (needs
observational data, a direct question now sent to WWF-SIPA — has her own baseline ever been checked
against anything observed); removed a "wrong direction" phrasing that implied baseflow-too-high was
inherently bad, replaced with the actual mechanism; trimmed a validation-checks table to one line.
A per-class regression breakdown (user spotted a real pattern by eye in the QF scatter plot) found
this project's own CN crosswalk compresses cropland toward grassland/brush relative to WWF-SIPA's
much wider separation between them — logged in full in `research_notes.md`, only a one-line pointer
in the report itself. Fixed a real matplotlib legend-transparency bug along the way (scatter alpha
leaking into legend swatches) in both `12_scatter_comparison.py` and a newly-promoted
`14_factorial_scatter_comparison.py`.

**The user has now made their own manual edits directly to the report, reviewed it, and considers
it final.** They are sending it to WWF-SIPA and using it as the topic for their next meeting with
her. Rendered clean as of this writing. Full narrative: `docs/swy/research_notes.md`'s 2026-09-21
entries; compressed version: `analysis/WORKLOG.md`'s 2026-09-21 entry.

**Nothing further is expected on this thread unless WWF-SIPA or Rich reply, or the user brings it
back up themselves.** The message draft to both
(`docs/swy/message_draft_becky_rich_ph_status_2026-09-18.md`) is ready but the user sends it
manually, not via this session. `swy_ph_30m_final` remains stopped, not retried (see the
2026-09-18 evening entry below for why, and the two mitigation options if it's ever resumed).

## LATEST (2026-09-21, later) — report cut from ~3000 to ~1700 words at the user's direct request, factorial scatter plots added, message draft tightened to match

**Three factorial scatter plots generated** (her-CN-alone QF, her-CN-alone B, her-Kc-alone B) —
confirm the table numbers exactly, and reveal something the numbers alone didn't show: in both B
plots, each WWF-SIPA land-cover class sits on its own tight, straight line, but the different
classes' lines are offset from each other (parallel, not coincident) and from the 1:1 line by
different amounts. Correlation improves a lot when either Kc or CN is corrected, but this
class-dependent structure survives regardless of which one — real evidence that something else,
tied to land-cover type, is still contributing beyond a simple one-variable swap. Whether it closes
once `09h` (her CN + Kc together) lands is the open question.

**Report rewritten, not just edited** — the user's direct feedback: it had become "too long and
convoluted... tries to explain too much... Becky doesn't have time to read a long thing." Cut the
CN/Kc methodology deep-dives, the full 9-item bug enumeration, the mermaid pipeline diagram, and the
full parameter-audit table down to one-line pointers into `swy_methods.qmd` and
`docs/swy/inspring_github_issues_draft.md` — those documents already hold the full detail, no need
to duplicate it in the reader-facing memo. Added a new **"Where this puts us — path to a global
run"** section addressing the user's own named concerns directly: no tropical CN correction, no
region/hemisphere-aware crop calendar, mangrove/flooded-grassland CN gaps, the unconfirmed DEM
choice, B's residual, and the real compute-infrastructure lesson from this week's 30m crash — framed
as "here's the honest scope of what's next," not blockers to sending this result now. Re-rendered,
confirmed clean, reopened for the user.

**Message draft tightened the same way** — now points at the report for numbers/detail instead of
repeating them, and at the GitHub issues draft for bug detail instead of listing all four inline.
One placeholder left deliberately for the user to fill in: how the report itself will actually reach
Becky (attached HTML, hosted link, or key parts pasted into the email body) — not something to guess
on their behalf.

**Update, same day, `09h` landed — and it closes the whole investigation out cleanly.** Her CN and
Kc together, through this project's own pipeline: QF ratio 0.99/r=0.92 (same as CN-alone, expected),
**B ratio 1.04/r=0.92** — essentially perfect, same fidelity as QF. The scatter plot
(`swy_ph_factorial_scatter_b_herCN_herKc.png`) shows the land-cover-class parallel-banding from the
single-factor tests fully collapsing back onto the 1:1 line. **Net conclusion: there is no
unexplained residual anywhere in this comparison.** The entire QF and B gap between this project's
own parametrization and WWF-SIPA's traces cleanly to the deliberate CN/Kc choice — makes physical
sense in hindsight (B depends on water left over after both quickflow *and* evapotranspiration are
accounted for, so correcting only one input still leaves the other's distortion in the water
balance). Report's factorial table, narrative, "Where this puts us" section, and Open Items all
updated to reflect this (B's residual removed from the global-scaling gap list — resolved, not
deferred), re-rendered, reopened for the user. Message draft updated to match — no more "running one
more test," now states the clean final finding.

**This SWY Philippines thread is now genuinely complete, not just paused.** Nothing technical is
left blocking on further investigation; what remains is the user's own review of the trimmed report,
filling in how they'll actually send it to WWF-SIPA (a placeholder was left for this deliberately),
and the four real global-scaling prerequisites listed in the report's own "Where this puts us"
section, which are new work, not follow-up on anything unresolved here.

## LATEST (2026-09-21) — Docker survived the weekend fine, factorial decomposition gives a real answer, report restructured, one more confirmatory run running

**Session-start check, since the user was worried about laptop-sleep/VS-Code-close losing work**:
Docker Desktop's backend wasn't up yet (process alive, engine not responding — needed ~10s to
finish initializing after being asleep since Friday), but once it came up, everything was exactly
as left: no orphaned containers, and Friday's `09f`/`09g` factorial outputs were intact on disk
(files persist independently of Docker/session state — this is a good general reassurance for this
concern going forward: **completed run *outputs* are safe once written**, regardless of what
happens to the laptop/session afterward; only a run genuinely *in progress* at the moment of a sleep
would be at risk).

**The factorial comparison (owed since Friday night, never actually computed then) is in, and it's
a clean, decisive result**:

| Combination | QF ratio | QF r | B ratio | B r |
|---|---:|---:|---:|---:|
| Our CN + our Kc | 0.77 | 0.56 | 1.48 | 0.39 |
| Her CN + our Kc | 0.99 | **0.92** | 1.27 | 0.70 |
| Our CN + her Kc | 0.77 | 0.56 | 1.21 | **0.82** |

**QF's gap is essentially all CN** (Kc doesn't feed QF at all — confirmed both by the model's own
architecture and by these numbers being identical whether Kc is ours or hers; her CN alone gets QF
to r=0.92). **B's gap is mostly Kc, with CN contributing too** (her Kc alone: r=0.39→0.82; her CN
alone: r=0.39→0.70). This is a genuinely strong result — not just "which factor matters more" but
also indirect proof this project's own pipeline is fundamentally sound (r=0.92 on QF when fed her
real CN would not happen if the DEM/grid/pipeline work were quietly broken).

**One more run launched, in progress as of this writing**: `09h_run_swy_ph_herCN_herKc.py` — her CN
and Kc together (not split), through this project's own 90m pipeline. This is the direct "are we
faithfully reproducing her calculation" check the user asked for Monday morning, as a much cheaper
alternative to a full her-parameters-at-30m replication (which would cost days, per the already-
crashed 30m run). Own workspace `workspace_becky_inputs_90m_herCN_herKc`; log at
`data/swy/philippines/diagnostics/09h_run_log.txt`. **Check this before assuming it's done.**

**Real process mistake made and fixed while launching the first two factorial runs Friday night**:
wrapped chained `docker run` commands in `(...) &` *and* passed `run_in_background: true` to the
shell tool — double-backgrounding, which made the tool report "completed" almost instantly even
though the run had only just started. The first run (`09f`) turned out fine (Docker containers run
independent of the launching shell), but the *chaining* to auto-launch the second (`09g`) could not
be trusted once the orchestrating subshell's fate was unknown — fixed by using `docker wait
<container>` instead, which blocks at the Docker-daemon level regardless of shell lifecycle.
**Never combine `&` with `run_in_background: true` on the same command — pick one.**

**Report restructured** (`docs/reports/swy/swy_status_report.qmd`), per direct user request: the
grid-nesting/masking narrative that had accumulated inline in prose was pulled out into a new,
scannable **"Validation checks performed"** table (six checks, one line each: what was checked, what
was found) placed before the results, and a new **"Decomposing the gap: is it CN, Kc, or something
else?"** section holds the factorial table above (with a `*pending*` row for `09h`, to fill in once
it lands). "Where we stand" and Open Item #2 updated to reflect the real, if partial, explanation
for B. Re-rendered, confirmed clean.

**Message draft updated** (`docs/swy/message_draft_becky_rich_ph_status_2026-09-18.md`) with the
factorial results in plain language, still not sent — waiting on `09h` for one more number before
the user sends it.

**Next steps, in order**: (1) check on `09h`; (2) once done, compute its ratio/r the same way as the
other three cells (reuse the pattern in `factorial_comparison.py`, currently only in this session's
scratchpad, not saved as a numbered pipeline script — worth promoting to
`Python_scripts/swy_philippines_run/` if this kind of factorial check gets reused); (3) fill in the
report's `*pending*` row and the message draft's "will send that number" line; (4) user sends the
message; (5) resume whatever Becky/Rich reply prompts, or return to the paper review.

## LATEST (2026-09-18 evening) — grid-nesting fix landed, report/maps regenerated, SWY paused to go finish the paper review

**Done, verified, closed out this session:**
- Grid-snapped 90m re-run (`workspace_becky_inputs_90m_snapped`) completed successfully; verified
  exactly nested inside WWF-SIPA's grid on the actual output, not just the input DEM.
- Explicit "valid in both datasets" masks built (`13_build_valid_comparison_mask.py`) and wired into
  both `12_scatter_comparison.py` (the real numbers) and `10c_render_comparison_maps.py`/`10b`/`11`
  (the map) — first time these two were guaranteed to look at the same set of pixels.
- New numbers: QF essentially unchanged (ratio 0.767, r=0.56 — was 0.766/0.567). B moved modestly
  (ratio 1.48, r=0.39 — was 1.51/0.38) but **the core puzzle is not resolved**: still backwards,
  still weak, now with an even wider per-class spread (0.47–6.0×) than before.
- The fine AET/L/B speckle from earlier today: **deprioritized, not solved**. The user checked
  WWF-SIPA's own independently-built B map and found the same pattern there too, fainter — strong
  evidence this isn't a bug in this project's own pipeline specifically. Not investigated further.
- `data/swy/philippines/` decluttered: `workspace_becky_inputs_90m_gcn250_nodata_bug_ARCHIVE`
  deleted (confirmed superseded); `workspace_becky_inputs_90m_rainfix_nearest_ARCHIVE`,
  `..._precip_et0_near_ARCHIVE`, `..._srtmgl3_backup`, and now the superseded
  `workspace_becky_inputs_90m_rainfix` itself all moved into a new `archive/` folder (with its own
  README). One snag worth remembering: the first archive attempt failed with a file lock — the user
  had `B_ph_becky_inputs_90m_rainfix.tif` open in QGIS, which left a `.aux.xml` sidecar and blocked
  the move until closed.
- Report re-rendered with the corrected numbers, updated Open Items, and a note on why the numbers
  changed from the pre-grid-fix run.
- Combined status/bug-report draft for Becky+Rich written and verified: cloned current
  `springinnovate/inspring` fresh and confirmed every bug claim is still live upstream (last commit
  touching the relevant file: 2025-02-11) — not stale complaints about already-fixed code. Draft at
  `docs/swy/message_draft_becky_rich_ph_status_2026-09-18.md`, not sent.

**Real, still-open problem, NOT part of the above — separate from the Philippines Kc/CN work
entirely: `swy_ph_30m_final` (the 30m native-resolution validation run) crashed after running ~2
days**, with a genuine I/O error (`TIFFFetchStripThing: IO error during reading of "TileOffsets"`)
writing `flow_dir_mfd`'s output — confirmed NOT an OOM kill, confirmed `ecoshard.geoprocessing.
routing` already sets `BIGTIFF=YES` by default (so that's not a missing-flag fix). Best working
theory: a ~2-billion-pixel domain, written in irregular (non-scanline) order over 2+ days through a
Docker-Desktop-on-Windows bind mount, is a known weak spot for exactly this kind of corruption —
infrastructure fragility, not a code bug. **Decision made with the user**: don't just retry blindly
(no guaranteed fix, could burn another 2-3 days for nothing) — if resumed, try addressing the likely
cause first (Windows Defender/AV exclusion on `data/`, and/or moving the run's working files onto
native WSL2 storage instead of a Windows bind mount) before relaunching. **Not started** — paused
along with everything else in this thread.

**Update, same evening, right before the pause above** — one more test launched at the user's
request: a 2x2 CN x Kc factorial design to isolate how much of B's gap traces to CN specifically
vs. Kc specifically. Two cells already exist (her CN+her Kc = her own baseline; our CN+our Kc =
`workspace_becky_inputs_90m_snapped`); built the two missing hybrid biophysical tables
(`07c_build_hybrid_biophysical_tables.py` → `ph_biophysical_table_ourCN_herKc.csv` /
`_herCN_ourKc.csv`, straight column-swaps, confirmed both source tables share row order/`lulc_id`
before swapping) and two new run scripts (`09f`/`09g`, own workspaces
`workspace_becky_inputs_90m_ourCN_herKc` / `_herCN_ourKc`, otherwise identical to `09e`). **Real
process mistake made and caught while launching these**: first attempt wrapped the chained
`docker run` commands in `(...) &` *and* passed `run_in_background: true` to the shell tool —
double-backgrounding — which made the tool report "completed" almost instantly even though the
actual run had only just started, and would likely have silently dropped the second (`09g`) run
entirely once the orchestrating subshell was torn down. Fixed by using a single, non-doubly-
backgrounded call built on `docker wait <container>` (blocks on the container at the Docker-daemon
level, independent of shell process lifecycle) to sequence the two runs reliably. **Worth
remembering for next time: never combine `&` with `run_in_background: true` on the same command —
pick one.** As of this writing, `09f` (our CN + her Kc) is running; `09g` (her CN + our Kc) will
auto-launch via `docker wait` once `09f` exits. Check `docker ps -a` for
`swy_ph_90m_ourCN_herKc`/`swy_ph_90m_herCN_ourKc` and the two log files in
`data/swy/philippines/diagnostics/` (`09f_run_log.txt`/`09g_run_log.txt`) to see where this landed.
The combined Becky/Rich message draft (`docs/swy/message_draft_becky_rich_ph_status_2026-09-18.md`)
has been updated with the corrected numbers and mentions this factorial test is in flight — the
user intends to send it without waiting for these last two runs to finish.

**Everything in this SWY thread is now explicitly paused, not abandoned, at the user's direction**
— a full week has gone into this (their words: "I have already spent more than a week here... I
need to finish the paper review, pronto"). Next session should read this entry, confirm nothing
here needs urgent attention (nothing does — the 30m job is stopped, not silently running up cost),
and hold off on resuming unless the user brings it back up.

## LATEST (2026-09-18, earlier) — status report reviewed and corrected before the grid-nesting work above

## LATEST (2026-09-18) — status report reviewed and corrected, GCN250 result folded in, checkerboard fix re-verified directly on the actual QF/B outputs (not just trusted from the log) — read this first

**Live check on the long-running 30m validation run** (`swy_ph_30m_final`, the run this whole thread is waiting on): still running as of this writing, ~25h elapsed, CPU genuinely active (~31%), not crashed. `flow_dir_mfd_ph_becky_inputs_30m.tif` (pit-filling + flow-direction) finished ~16.5h before this check with no later intermediate file yet — consistent with it now being deep in flow accumulation, the same slow stage that took a previous attempt 24h+. No `aggregated_results` shapefile yet. **Do not restart it.** Separately, `swy_ph_90m_gcn250v2` (the GCN250-direct comparison run) had already exited cleanly (exit 0) — its numbers are the ones folded into the report below.

**Two real, user-requested things done this session:**

1. **Checkerboard fix re-verified directly, not just trusted from the research log.** Rendered matched before/after native-resolution crops (same pixel window, `swy_borneo_run:rainfix4` container, `rasterio`) from three levels: the aligned intermediate continuous inputs (`n_events0`, `et0_a0` — pure fields, no CN texture to obscure anything), and the actual `QF`/`B` model outputs themselves, comparing the pre-fix archive (`workspace_becky_inputs_90m_rainfix_nearest_ARCHIVE`, all-`'near'` resampling) against the corrected live run (`workspace_becky_inputs_90m_rainfix`, bilinear + raw-soil-group fixes). **All four pairs show the same pattern**: hard-edged, grid-aligned rectangular blocks in the "before" crop, smooth/organic texture in the "after" crop, with the block edges specifically gone (not just fainter) in QF and B. This is now confirmed by direct visual inspection this session, independent of the 2026-09-17 log entry's own claim. Diagnostic PNGs and the one-off scripts that made them are in `data/swy/philippines/diagnostics/` and this session's scratchpad — not part of the numbered pipeline, safe to ignore/delete later.

2. **`docs/reports/swy/swy_status_report.qmd` reviewed end-to-end at the user's request and two real problems fixed, then re-rendered:**
   - **A self-contradiction**: the "Where we stand" summary at the top still claimed *"`inspring` has no raster-override capability for either factor; that is the only path this codebase offers"* — flatly contradicted by the "Curve number" section a few paragraphs down, which correctly documents the real undocumented `cn_a_path`-`cn_d_path` raster-override mechanism. This stale claim had already been flagged as fixed once before (`research_notes.md`'s 2026-09-17 entry says so explicitly) but the fix evidently didn't stick in this specific summary box. Corrected now.
   - **A stale status line**: the Curve Number section said a GCN250-direct test run was "in progress... not yet complete" — it had actually finished (see the 90m-`gcn250v2` run above). **Judgment call made, not re-litigated with the user**: folded the real result in as a side-by-side comparison table in that same section (not a footnote), since this is a technical memo for domain experts (Becky/Rich) who'd want the actual numbers, not just the CN methodology narrative. Table: QF ratio 0.77→0.70, QF r 0.57→0.57 (unchanged), B ratio 1.51→1.56, B r 0.38→**0.49** (real improvement in spatial pattern, not magnitude), framed as "doesn't resolve B, tracks it better, does slightly worse on QF" — matches the 2026-09-17 entry's own numbers below, not re-derived. If the user wants this demoted to a footnote instead, easy to change.
   - Re-rendered via `quarto render swy_status_report.qmd` — succeeded (one pre-existing, unrelated Pandoc warning about a raw-HTML table inside the embedded interactive map, not something introduced by these edits). Opened in the default browser for the user to review directly.

**Maps: no regeneration needed.** Verified `_output_map_ph.html` and both scatter PNGs (`swy_ph_comparison_scatter_{qf,b}.png`) already point at the corrected `workspace_becky_inputs_90m_rainfix` run (confirmed via the render scripts' own `WORKSPACE` constants and file timestamps) — they were already current, just re-confirmed rather than assumed.

**Update, same session, immediately after — a real, separate, THIRD checkerboard-looking bug found and fixed, this time in the map-rendering scripts, not `inspring` or the model itself.** The user looked at the freshly-opened report and immediately spotted a speckled/mottled pattern in the "NCP Kc/CN run — Baseflow (B)" map layer, reasonably worried the original bug wasn't actually fixed. Investigated properly rather than reassured:

- Root cause, confirmed directly: `10b_render_output_maps_becky_inputs.py` and `10c_render_comparison_maps.py` both used `rasterio`'s `Resampling.nearest` to downsample the full-resolution rasters for web display (`out_shape=` at `MAX_DIM=1600`) — at this AOI's actual decimation ratio (~7-12x), nearest-neighbor picks one raw pixel per display cell and discards the rest, which aliases the model output's real fine-scale per-pixel texture (CN/soil-group-driven, confirmed smooth and legitimate at full resolution earlier this session) into a salt-and-pepper/moiré pattern in the *display image only*. Confirmed by rendering the same full-extent B layer both ways side by side: `nearest` reproduces the exact speckled look the user flagged; `average` (proper box-filter downsampling) shows smooth, coherent spatial gradients instead — real watershed/terrain structure, not noise.
- **This is unrelated to the 2026-09-17 `inspring`-side fix and does not call that fix into question** — confirmed separately that `12_scatter_comparison.py` (which produces every ratio/correlation number actually quoted in the report) already used `Resampling.average` correctly and was never affected. The report's numbers were correct throughout; only this map's visual display was misleading.
- Fixed: both scripts switched to `Resampling.average`; `10b`, `10c`, and `11_build_output_map_html.py` (which assembles `_output_map_ph.html` from both) re-run to regenerate the map with the fix. Also added a short caption directly on the map (in `11`'s own generated HTML) explaining that display images are downsampled/averaged and that a speckled look at zoomed-out scale is expected, not a data problem — so this doesn't cause the same alarm again for a future reader. Report re-rendered and reopened.
- One thing noticed in passing, not investigated, not urgent: the regenerated `l_sum` layer's reported min came back slightly negative (-893.7, against a max of ~14.8M) — possibly a minor nodata-edge leak from the switch to average resampling, possibly pre-existing. Trivial relative to the layer's own scale and the already-documented, already-open `L_sum` anomaly item; not re-litigated this session.

**Update, same session, later still — two more real, deeper problems found (the user kept
verifying instead of accepting "fixed"), plus a paused one, and an approved plan to fix them:**

1. **Paused, not solved**: after the map-display fix above, the user checked the raw B GeoTIFF
   directly in QGIS and still saw a real fine speckle — genuinely present in the data, not a
   display issue. Traced it to the AET/local-recharge step specifically (present in AET, L, B;
   absent from QF; confirmed not routing/DEM-related since even pre-accumulation L already shows
   it; confirmed Kc is genuinely class-uniform by reading `07b`'s code directly, ruling out
   per-pixel Kc noise; confirmed present identically in the pre-checkerboard-fix archive, so not
   caused by anything this week). **Not yet root-caused** — deprioritized below two bigger findings
   at the user's own direction. Resume by reading `inspring`'s actual Budyko-curve AET solver.
2. **A real, big one**: NCP's own B raster has ~8.16 million NaN pixels (~25% of the valid domain)
   forming a clean ring around every coastline — confirmed via direct pixel comparison against NCP's
   own QF (which has no such gap), confirmed it's introduced at the routing/accumulation step (AET
   and L are both valid at these exact locations). `12_scatter_comparison.py`'s own `isfinite()`
   filter already silently excludes these pixels from every reported B statistic, undocumented —
   and it's not a neutral exclusion, since the classes already flagged as most anomalous (Mangrove,
   Fishpond, built-up) are inherently coastal.
3. **A real, bigger one, the user's own catch**: the two comparison grids (NCP 90m, WWF-SIPA 30m)
   share a clean 3:1 pixel-size ratio but their origins are offset by a non-integer fraction of a
   pixel (~0.91 of a 90m pixel, ~22m). The existing numeric comparison already handles this
   correctly via proper area-weighted reprojection (verified independently), but isn't literally
   pixel-nested. Traced the fix: `inspring` derives its entire output grid from whatever raster is
   passed as the DEM, so a DEM pre-warped onto an exactly-nested grid fixes this without touching
   `inspring` itself.

**Plan approved with the user** (`C:\Users\JerónimoRodríguezEsc\.claude\plans\curious-gliding-whisper.md`;
full reasoning trail in `docs/swy/research_notes.md`'s 2026-09-18 entry): build a DEM snapped to
WWF-SIPA's own grid lattice (`02e_snap_dem_to_baseline_grid.py`, hard-verified for zero fractional
offset before proceeding), re-run the 90m model on it (`09e_run_swy_ph_snapped_grid.py`, own
workspace, multi-hour background job), build an explicit per-variable "valid in both datasets" mask
per the user's own framing (`13_build_valid_comparison_mask.py`, full-3x3-block agreement required,
consumed by both the map script and the scatter/stats script instead of each computing its own ad
hoc validity logic), regenerate the comparison once the run lands, and declutter
`data/swy/philippines/` alongside this (fresh inventory: 81GB total; confirmed-superseded workspace
copies get deleted, undocumented-but-unreferenced ones get archived, not deleted outright).

**Status as of this writing: plan approved, `02e_snap_dem_to_baseline_grid.py` written, not yet
run.** Next concrete steps, in order: (1) run `02e` and confirm its hard zero-offset assertion
passes; (2) launch `09e` in the background; (3) write and test `13_build_valid_comparison_mask.py`
while `09e` runs; (4) once `09e` finishes, run `13`, then re-run `12`/`10c`/`10b`/`11`, compare new
numbers against today's as the real payoff check; (5) re-render the status report and update this
file with the outcome; (6) execute the `data/` decluttering from the plan's Part 4; (7) resume the
paused AET-speckle investigation. **`swy_ph_30m_final` (30m validation run) is untouched throughout
all of this** — still running, check `docker ps` before assuming otherwise.

**Superseded by the above — kept only for the record**: the "next steps" list immediately below,
written before these three findings, treated the GCN250 placement decision and the map-display fix
as the main remaining items. They're both still true and done, just no longer the critical path.

**Next steps, as they stood right after the GCN250 placement decision and the map-display bug fix, before the three findings above**: (1) keep checking `swy_ph_30m_final`; (2) once it finishes, compare its QF/B ratios against the 90m-aggregated numbers to close the resolution-validation question — this is the one substantive thing still blocking; (3) user posts the GitHub issues (`docs/swy/inspring_github_issues_draft.md`) to Rich manually, or asks for `gh` CLI setup first; (4) the map-caption fix for the toggle-misalignment perception (2026-09-17 point 5a) is still open, cosmetic, not urgent; (5) B's core puzzle (direction reversal, weak-but-improving correlation) remains the single biggest open scientific question.

## LATEST (2026-09-17) — five real inspring bugs found and fixed, a real WWF-SIPA masking bug fixed, alignment re-verified rigorously, a genuinely useful undocumented inspring feature found, two more runs in flight

**This was a very long, winding session** (the user's own words: "I thought I would be done with this by Tuesday, yet here we are still at this") — real, valuable findings throughout, but easy to lose track of. This entry is intentionally thorough because the user asked directly for that.

**In order, what actually happened:**

1. **Confirmed the Philippines rain-events run (from the 2026-09-16 entry below) had a real, separate resampling-method bug.** Visual inspection of the output at native resolution (the user's own catch, not something the numbers alone would have shown) revealed a clear, regular checkerboard in both QF and B. Root cause: `seasonal_water_yield.execute()`'s `interpolate_list = ['near'] * len(input_align_list)` applies nearest-neighbor to *every* aligned input uniformly — correct for LULC/soil-group (categorical), wrong for precip/ET0/rain-events (continuous, and coarser than the 90m DEM here, ~1km native). Fixed: those three now get `'bilinear'`. Confirmed by direct pixel inspection of the aligned intermediate rasters, before and after.

2. **Separately, the hydrologic-soil-group input itself had a real, regular block artifact — confirmed to be in the file, not introduced by this project's processing.** `HYSOGs250m_Soil_Groups_reclassified.tif` (this project's copy, from NatCap's own Data Hub) shows a clean checkerboard even in its own raw, unprocessed native-resolution pixels — visually confirmed directly, not inferred. The genuine ORNL DAAC original (Ross et al. 2018, `daac.ornl.gov/daacdata/global_soil/Global_Hydrologic_Soil_Group/data/HYSOGs250m.tif` — public data, but does need Earthdata login; downloaded successfully via `.netrc` + `curl` with a cookie-jar workaround for the OAuth redirect loop) shows genuine fine-scale texture instead, at the same location. Whatever NatCap's own "reclassified" repackaging did, it introduced this artifact — worth flagging to whoever maintains that Data Hub entry (now an Open Item in the status report). **Also directly relevant**: Becky's own `.ini` references `HYSOGs250m_md5_517bfa.tif`, an MD5-named file (the ORNL DAAC direct-download naming convention) — not the same file this project had been using, and whose pixel-value encoding (1-4 base classes, 11-14 dual classes) matches the *raw* product's documented encoding exactly. Switched this project's own pipeline to the raw file too, clipped to the Philippines AOI and dual-class-collapsed to match her own `SOIL_HYDROLOGIC_MAP` logic exactly.
3. **Combining both fixes eliminated the checkerboard entirely** — confirmed by direct native-resolution visual inspection of the corrected run's QF and B outputs. Real remaining spread in the maps is now organic land-cover/drainage structure, not a processing artifact.
4. **A real, separate bug found in WWF-SIPA's own shared baseline B raster**: its nodata flag (-9999) doesn't cover everything her own QF raster's nodata flag does (same grid, verified) — ~55 million pixels she calls "valid" are really ocean, many reading exactly 0.0, which deflates her B mean. Corrected using her QF raster's own clean mask (moves her B mean from 653 to ~699mm/yr in the original full-resolution check, ~699 vs 752 in the paired-comparison version). This is a bug in how the comparison reads her file, not in her model.
5. **The user pushed hard, correctly, on two things that turned out fine but needed real verification, not reassurance**: (a) a perceived pixel misalignment when toggling map layers — re-verified from scratch on the final corrected data (not assumed from the earlier 2026-09-15/16 investigation): 99.44% land/water agreement, best cross-correlation at exactly zero shift, and a zoomed pixel-diff mask showing every disagreement confined to a one-pixel-wide fringe along coastlines/rivers — real boundary quantization between 30m and 90m grids, not a registration bug. The interactive *map* (unlike the numeric comparison) doesn't put both layers on a shared grid, which is why toggling made this look worse than it is — worth a caption fix, not done yet. (b) whether the valid-data extent genuinely matches — quantified precisely (not just a summary stat): 2.82% of NCP's valid area and 1.45% of baseline's have no counterpart, entirely explained by that same coastline/river fringe, visually confirmed.
6. **A real, previously-missed `inspring` capability found**: `_reclassify_or_clip()` (used for `cn_a/b/c/d`, `root_depth`, `kc_1`-`kc_12`) checks `args['{factor}_path']` *before* falling back to the CSV table — if set, it warps that raster directly onto the model grid and uses it, bypassing the table entirely. **This directly reverses an earlier documented claim** (2026-09-14, repeated again by mistake in this session's first pass at today's status report) that no such path exists — that claim was checked against `execute()`'s own docstring, which never mentions this, not against the actual runtime logic. Also means Becky's own `.ini` (`CN_A_PATH` etc.) may not be "a different wrapper/fork" as previously assumed — those parameter names map directly onto this real mechanism. **GCN250's own delivery format doesn't map onto this naively**: it's three rasters by antecedent moisture condition (dry/average/wet), not four by soil group — GCN250 already bakes soil group in during its own construction (crosswalked against HYSOGs250m). Correct usage: point all four of `cn_a_path`-`cn_d_path` at the same "average" (ARC-II) GCN250 raster. **In progress as of this writing**: `GCN250_ARCII.tif` downloaded (Figshare, `ndownloader.figshare.com/files/15377363`, 640MB, global) — a 90m test run using it directly (bypassing this project's own hand-built CN table) is the next concrete step, to see if a real per-pixel product changes the comparison. Not yet clipped/launched.
7. **Three draft GitHub issues written for `springinnovate/inspring`** (`docs/swy/inspring_github_issues_draft.md`), at the user's explicit request — Rich prefers formal issues/PRs over informal messages. Covers: (1) broken Dockerfile/`setup.py` packaging, (2) the three `user_defined_rain_events_dir` bugs plus the resampling-method issue, with our own tested patches as a proposed fix, (3) the undocumented raster-override feature, framed as a documentation-PR ask. **`gh` CLI isn't installed on this machine** — user will post manually via the GitHub web UI, or install `gh` if they want it posted directly later. Not posted yet.
8. **A second, independent 30m validation run is in progress** (`swy_ph_30m_final`, workspace `data/swy/philippines/workspace_becky_inputs_30m`), launched at the user's explicit request to check whether the 90m-vs-her-30m aggregation methodology is itself sound — if a native 30m run gives materially different QF/B ratios than the 90m-aggregated comparison, that would matter a lot; if it matches, that closes the resolution question for good. Uses the same fully-patched image (`swy_borneo_run:rainfix4`) and the same real rain-events/real-soil-group inputs as the corrected 90m run. **As of this writing, ~4+ hours in, still in DEM flow-routing (pit-filling done, flow-direction not yet)** — this is the exact same slow stage (flat/tidal archipelago terrain) that made an earlier, differently-configured 30m attempt take 24+ hours before being aborted (see the 2026-09-16 entry below) — expect this to potentially take a similarly long time; **do not restart it** for the GCN250 test, since DEM-routing doesn't depend on CN inputs at all — once this run reaches/finishes DEM-routing, TaskGraph's own caching (confirmed: task-level, keyed on function+args+input-file state, stored in `cache_dir/taskgraph_data.db`) should let a GCN250-adjusted 30m variant reuse that expensive work rather than redoing it, **if** it's launched against the same workspace directory with only the CN-related args changed. Checked directly against the routing functions' own signatures: `fill_pits`/`flow_dir_mfd`/`flow_accumulation_mfd` expose zero threading/worker parameters — this bottleneck cannot be sped up with more CPU cores, confirmed, not assumed.

**Current best numbers (90m, both fixes applied, before any GCN250 test)**: QF at ~77% of WWF-SIPA in aggregate (r=0.57, per-class spread 0.55-1.37×, a 2.5× factor — plausible as an ordinary Kc/CN difference). B at ~151% of WWF-SIPA, wrong direction from QF, correlates weakly (r=0.38), per-class spread 0.71-6.0× — real, unexplained, confirmed not an artifact of anything fixed this session. One coherent partial explanation found: Kc feeds AET not QF, and this project's lower EVI-based forest Kc vs. her flat 1.0 shows up as elevated B specifically in forested classes (Closed Forest 1.7×, Open Forest 1.8×, Mangrove 2.9×) — consistent, not proof, since B's overall pattern has other open questions too.

**Report** (`docs/reports/swy/swy_status_report.qmd`) fully rewritten this session as a clean current-state memo (no run-by-run history in the reader-facing text, calibrated to Becky/Rich as domain experts, per direct user feedback — see `feedback-report-vs-log-separation` memory) — then the user made direct edits and left inline bracketed questions, all now addressed (GCN250 citation, the tropical-forest-CN-patch status, the CN raster-vs-table mechanism now corrected per point 6 above, the Kamble/Rich/EVI attribution, how the Kc finding shows up in B not QF, a confusing transition sentence). Rendered clean, zero remaining bracketed comments as of this writing. **Still needs**: folding in the GCN250 test result once it exists, and the map-caption fix for the toggle-misalignment perception (point 5a above).

**Next steps, in order**: (1) check on the 30m run; (2) fold the GCN250 result (below) into the
report, and decide whether to feature it as a real secondary comparison or a footnote; (3) once the
30m run finishes, compare its QF/B ratios against the 90m-aggregated numbers to close the
resolution question; (4) user posts the GitHub issues to Rich manually, or asks for `gh` CLI setup
first; (5) B's core puzzle (direction reversal, weak correlation) remains the single biggest open
scientific question, not yet close to explained.

**UPDATE, same session, later — GCN250-direct result in, a real bug found and fixed on the way,
30m run still going**:

The first GCN250-direct attempt (`workspace_becky_inputs_90m_gcn250`, now archived with a
`_nodata_bug_ARCHIVE` suffix) hit a **sixth real bug**: `geoprocessing.warp_raster()` (called
inside `_reclassify_or_clip()`'s raster-override path) has no `nodata` parameter at all and doesn't
propagate the source's real nodata through — 1.17% of valid model pixels came out with a literal
CN of 255 (GCN250's nodata sentinel), concentrated in a thin fringe around every coastline where
GCN250's own coverage doesn't quite match this project's DEM/LULC land mask. Fixed by pre-filling
GCN250's nodata gaps with `scipy.ndimage.distance_transform_edt`'s nearest-valid-neighbor fill
before clipping (`data/swy/philippines/inputs/gcn250_arcii_ph_filled.tif`) — confirmed clean on
retry (`swy_ph_90m_gcn250v2`, zero pixels above CN 100). Added as a fourth sub-item to GitHub Issue
3 in `docs/swy/inspring_github_issues_draft.md`.

**The actual GCN250-direct result, a genuinely mixed picture, not a clean win**:

| | Table-based CN (current report numbers) | GCN250-direct |
|---|---:|---:|
| QF ratio (NCP/WWF-SIPA) | 0.766 | 0.698 |
| QF correlation | 0.567 | 0.568 |
| QF per-class spread | 0.55–1.37 (2.5×) | 0.34–1.15 (3.4×, wider) |
| B ratio | 1.510 | 1.557 |
| B correlation | 0.376 | **0.486** |
| B per-class spread | 0.71–6.00 (8.5×) | 0.80–5.88 (7.3×) |

GCN250 doesn't resolve B's core puzzle (same magnitude, same wrong direction) but tracks its
*spatial pattern* meaningfully better (r 0.376→0.486). It does modestly worse on QF, apparently
because GCN250's own land-cover crosswalk doesn't map WWF-SIPA's specific 12-class typology onto
water/wetland/built-up classes as cleanly as this project's hand-built one (Fishpond, Built-up,
Inland Water all moved notably further from the baseline). **Not yet folded into the report** —
this needs a real judgment call on presentation (side-by-side comparison vs. secondary footnote),
raised with the user right as this session ran low on context. Comparison computed via a one-off
inline script (not `12_scatter_comparison.py`, to avoid disturbing the file the report's current
numbers depend on) — logic: same `_resample_to_ref`/`run_pair` pattern, `NCP_WORKSPACE =
workspace_becky_inputs_90m_gcn250`, `NCP_SUFFIX = gcn250_direct`, baseline B via the same masked
file as everywhere else in this session.

**30m run status at the point this session ran low on context**: `swy_ph_30m_final` still running,
~21 hours elapsed, CPU active (not stalled), no crash. Milestones so far: pit-filling done ~1hr in,
the full flat-region/plateau-resolution sequence (flat_region_mask, plateu_drain_mask,
plateau_distance) done by ~9hr — matching the entire slow stage that took the *previous* aborted
30m attempt 24h+ to even reach — but `flat_region_mask.tif` was seen being rewritten again at the
~16hr mark, meaning it's ambiguous whether flow-direction is genuinely finished or still internally
iterating; flow accumulation (the next real milestone) had not appeared as of the last check.
**Do not assume this needs restarting** — check `docker ps --filter "name=swy_ph_30m_final"` first;
Docker runs independent of any Claude Code session, so it kept going through this session ending.
If it finished or crashed, the workspace at `data/swy/philippines/workspace_becky_inputs_30m` has
the answer either way (look for `aggregated_results_swy_ph_becky_inputs_30m.shp` for a real
completion, or `docker ps -a` for a crash).

**To resume in a fresh session**: paste this whole `HANDOFF.md` file in, as the maintenance note at
the top says. The fresh session should, in order: (1) check `docker ps` for `swy_ph_30m_final`
(and confirm nothing else is stuck running); (2) if it finished, run the same comparison pattern
used for GCN250 above against its QF/B outputs vs. the WWF-SIPA baseline, and compare its ratios to
the 90m-aggregated numbers already in the report — this closes the resolution-validation question
that was the whole reason this run exists; (3) decide with the user how to present the GCN250
result in the report; (4) re-render `docs/reports/swy/swy_status_report.qmd` and confirm it's
still clean before treating anything as final.

## LATEST (2026-09-16) — DEM-resolution confound fixed, alignment scare closed out, three inspring bugs fixed, Becky status update sent — read this first

**Full detail in `docs/swy/research_notes.md`'s new 2026-09-15/16 entry** — this is the compact
version. In order: caught that the Becky-inputs run reused an unrelated 90m DEM even though her
`.ini` targets 30m (a real, previously-unnoticed confound on the whole isolated-Kc/CN-variable
design); fetched SRTMGL1 and re-ran (**still running as of this writing, ~24h+ in** — a much
bigger raster than anything run before, not stuck, just genuinely big — check `docker ps` and this
file's own "Docker runs" note below before assuming anything). A water-body misalignment scare on
the interactive map was investigated properly (not assumed either way) and closed out clean — a
whole-domain cross-correlation found 99%+ agreement at zero shift; the visual artifact was
resolution-driven quantization on one complex lake shape, not a real registration bug. Found and
fixed three real upstream `inspring` bugs in the `user_defined_rain_events_dir` path (all three
patches now in `Python_scripts/swy_borneo_run/Dockerfile`, image `swy_borneo_run:rainfix2`) — a
second, isolated test run (90m DEM, her real rain events enabled, fast) is running in parallel to
the 30m resolution-fix run, to separate the two variables. A status-update message (not final
results) went to Becky 2026-09-16, archived at
`docs/archive/message_becky_ph_comparison_status_2026-09-16_sent.md`.

**Docker runs, both still going as of this writing** — check `docker ps` before assuming either
finished: (1) `upbeat_cohen` (or whatever name `docker ps` shows for the original container) — the
30m SRTMGL1 resolution-fix run, workspace `data/swy/philippines/workspace_becky_inputs/`; (2)
`swy_ph_90m_rainfix2` — the rain-events-fix isolated test, 90m DEM, workspace
`data/swy/philippines/workspace_becky_inputs_90m_rainfix/`. **Next session: check both for
completion first.** If either changes the comparison numbers, regenerate the maps
(`10b`/`10c`/`11_build_output_map_html.py`), scatter plots (`12_scatter_comparison.py`), and
re-render `docs/reports/swy/swy_status_report.qmd` before sending Becky anything further — don't
reuse the numbers already in that report, they're from the old 90m run.

**New paper task, not started, for a fresh session**: Quarto review-comment blocks in the paper
draft are rendering visibly into the shared `.docx` export — confusing for external readers (the
downside of authoring in `.qmd` but sharing as `.docx`). Fix: keep comments out of the `.docx`
render entirely, and instead of deleting them, move them into a **separate new `.qmd` file**
alongside a pointer to which section of the paper each one belongs to, so the review/discussion
context isn't lost, just kept out of the shared document. Not investigated yet which comment
mechanism the draft actually uses (HTML comments, a custom Quarto callout, margin notes via
`{.aside}`, etc.) — check `docs/manuscript/paper_draft_5service.qmd` first thing.

**Also still pending on the paper, from before this session** (not touched today, still true as of
the last time it was checked): Becky's 14 numbered review comments on
`docs/manuscript/paper_draft_5service.pdf` (gitignored) still need a response — status of whether
that response was ever actually sent was last recorded as **unconfirmed**, so verify before
assuming either way. One specific flagged item within that: comment BC6 says a 1.8×
income-intensity stat "was already removed from Results" but it's still visibly present in Section
3.2 (Figure 7, Table 4) — the user deliberately deferred checking this personally; don't raise it
again until they have, but don't assume it's resolved either.

## LATEST (2026-09-15 evening) — mermaid diagrams fixed for real; manuscript folder cleaned up

**Mermaid, closed out**: both SWY diagrams now use Quarto's native `{mermaid}` executable-cell
syntax (matching `docs/manuscript/paper_draft_5service.qmd`'s own already-working figure — that
was the answer, not any of the custom client-side-loader or static-SVG approaches tried earlier
today; see `reference-mermaid-local-testing` memory for the full history and a reusable
Deno-based test harness if this ever breaks again). **User confirmed both diagrams now render in
all viewers, including VS Code's internal browser.** Just fixed a formatting pass on top: font
size and node/rank spacing bumped via `%%{init: ...}%%`, a color legend subgraph added (green
"Done" / yellow "In progress" / red "Blocked" — the diagrams had no legend before, and subgraph
title labels were being visually crowded by child nodes). Applied to all three files
(`docs/swy/workflow_diagram.qmd`, `workflow_philippines_diagram.qmd`,
`docs/reports/swy_status_report.qmd`), re-rendered, no errors.

**User checked, real formatting bugs remain — fix next session, not done yet**:
1. **Subgraph background boxes are too dark** — currently default mermaid gray, and the subgraph
   title labels (black text) are barely visible against it. Needs an explicit lighter
   `clusterBkg`/`clusterBorder` themeVariable (or similar) in the `%%{init: ...}%%` block, not
   just the font-size bump already applied.
2. **Text inside the colored (done/progress/blocked) node boxes overflows the box width** — the
   box itself isn't sizing to fit its own label text. Likely needs explicit node width/padding via
   `themeVariables` (or shorter label text) rather than relying on mermaid's auto-sizing, which
   isn't accounting for the multi-line `<br/>` content properly at the current font size.

Same three files affected. Start here next session — quick, bounded CSS/themeVariable tuning, not
a re-open of the earlier rendering-mechanism saga (that part is confirmed working).

**`docs/manuscript/` cleaned up**: archived the pre-5-service-redesign paper
(`paper_draft.qmd`/`.html`/`.docx`, `index.tex`, standalone `workflow.qmd`/`.html`) to
`docs/archive/manuscript_pre_5service/` (README included there explaining what/why), deleted a
stray Word lock file. **Left alone, pending user confirmation on live/dead status**:
`agu_abstract_2026.md` (submission was blocked by a portal crash back in August, resolution
unknown), `becky_reply_ratios_clarification_2026-09-07.draft.md` (may be the same "sent?
unconfirmed" item already below), `becky_open_questions_2026-09-03.draft.md`,
`justin_ee_correspondence_provenance_2026-08-31.draft.md`.

**Also today**: `docs/reports/` and `docs/applications/` reorganized (Colombia/Phase4 reports into
their own subfolders with assets; `colombia_capability_portfolio.md` rewritten as an evergreen
pitch doc, old dated version + the fully-executed `clec_sandra_sprint_plan.md` archived); a new
Philippines pipeline diagram built; the SWY status report stripped of "AI-tell" process-narration
language per direct user feedback (see `feedback-report-vs-log-separation` memory) and given a
proper References section. Full detail in each section below and in today's memory updates —
this top entry is intentionally compact given session length.

## LATEST (2026-09-14 late evening) — Becky-inputs comparison run COMPLETED successfully — read this first

**The run finished while the user was asleep — full spatial coverage, exit code 0, real, plausible
numbers.** It just took 37+ minutes with zero console output because the whole time was spent in
the GDAL LULC-reprojection step (genuinely slow on her large raster, not stuck — don't assume a
silent long step has hung without checking `docker ps` first). The wall of threading tracebacks at
the very start of the log (`Joining executor thread ... would have caused a deadlock`,
`currentThread() is deprecated`) is harmless `taskgraph`/Python-3.13 teardown noise, unrelated to
correctness — expect it on every run using this `taskgraph` version, don't mistake it for failure.

**Headline results** (full detail, water-balance sanity check, and caveats in `swy_methods.qmd`'s
"Test design: Philippines" → "Result" subsection):

| Variable | Mean (mm/yr) |
| --- | --- |
| P (precip) | 2399.5 |
| QF (quickflow) | 257.3 |
| AET | 834.6 |
| L (local recharge) | 1359.2 |
| B (baseflow) | 1333.6 |
| Aggregate `qb` | 1321.0 |

All physically plausible for a wet tropical archipelago; `QF+AET+L` ≈ `P` within ~2% as an
aggregate sanity check; `qb` (1321mm/yr) is the same order of magnitude as Borneo's own `qb`
(1705mm/yr) — a sensible basin-to-basin difference, not a red flag. **Getting AET's real number
took a second pass**: the first, naive whole-raster mean came out as a misleadingly low
224mm/yr/median-0, diluted by legitimate zero-value ocean pixels outside the actual land mask;
re-masking to `QF`'s own valid-pixel extent (the correct mask) gave the real 834.6mm/yr figure —
worth remembering as a general gotcha for any future stats pulled from these output rasters
directly, not just this one number.

**Two things to actually look at next session, not urgent, not blockers**:
1. The Borneo `L_sum` flow-accumulation anomaly recurs here, worse: 11.5% of pixels exceed 100,000
   (vs. Borneo's ~5%). Consistent with — not proof of — the flat-tidal-coastal-terrain hypothesis
   already documented for Borneo, since an archipelago has far more coastline per unit area than a
   single landmass. Same item as pending #6 below, now with a second data point.
2. **New, unexplained**: the aggregated-results shapefile's `vri_sum` field reads exactly `0.0`,
   where it should read closer to 1 (`Vri` is `L` normalized by AOI-wide total `L`). Not
   investigated tonight — could be a real bug, an archipelago-geometry quirk, or a pre-existing
   `inspring` reporting issue nobody happened to check for Borneo either. Worth a look, then either
   fix, explain, or note as another open item.

**Update, 2026-09-15 morning — the real her-vs-our comparison, prompted by the user catching that
the first draft to Becky didn't make clear whose numbers were whose.** Becky's own baseline output
was already sitting in the repo (`data/swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_
historical_climate/` — `QF`, `B`, `B_sum`, `L_avail`, `L_sum_avail`), never previously compared
pixel-for-pixel against this run. Built: a real side-by-side numbers table, an interactive
comparison map (her vs. our QF/B, matched color scales), and a pixel-wise scatter plot (her
rasters area-averaged onto our coarser grid, colored by her own LULC class) — all now in
`swy_status_report.qmd` and `swy_methods.qmd`'s new comparison subsection.

**The real finding**: her/our QF ratio is 0.35 in aggregate (we're lower), but swings from 0.195
to 1.31 depending on land-cover class — too wide a spread for the rain-events placeholder alone
(spatially flat by construction) to explain; the CN/Kc crosswalk's real class-dependence is
clearly contributing too, not confirmed as the sole cause either. **B (baseflow) flips direction
entirely** — ours runs *higher* than hers almost everywhere, opposite of QF — genuinely not
understood, flagged as real follow-up, not resolved. Scripts:
`Python_scripts/swy_philippines_run/10c_render_comparison_maps.py`,
`11c_build_comparison_map_html.py`, `12_scatter_comparison.py`.

**Report re-rendered and re-copied** to `data/swy/shared/swy_status_report.html` (the distributed
location — moved 2026-09-15 from `data/swy/borneo/inputs/`, see Key file locations below) with
all of the above included — do this again if the source `.qmd` changes.

**Also fixed 2026-09-15**: the workflow.md mermaid diagram embedded in this report was never
actually rendering — it showed as inert code, not a diagram, in every browser, in both this report
and the standalone `workflow_diagram.qmd`/`.html`. Root cause: Pandoc correctly tags the block, but
nothing was loading the Mermaid.js library to draw it — a documentation claim that this "already
renders to SVG" was never actually verified (same pattern as the raster-override error found
2026-09-14). Fixed in both files' frontmatter via `include-in-header`, loading mermaid.js from the
same CDN already used for the Leaflet maps. Confirmed present in both rendered outputs.

**Becky message redrafted** at `docs/swy/message_draft_becky_ph_comparison_results.md` — now
correctly attributes every number to "her baseline" vs. "our comparison run" explicitly, leads
with the real comparison rather than our numbers alone, and states both real caveats (rain-events
placeholder, the unexplained B reversal) plainly. **Borneo dropped from the headline of this
message at the user's direction** — still mentioned as background context where genuinely
relevant (e.g. cross-basin sanity checking), not removed from the project, just not co-equal
framing for a Philippines-focused message. **Not yet sent — check with the user before sending.**

**Update, 2026-09-15 afternoon — mermaid diagrams actually work now, in every viewer, for real this
time.** Took three real attempts, all now recorded in the `reference-mermaid-local-testing`
memory so the next mermaid problem here doesn't repeat the same dead ends. (1) Nothing loaded
mermaid.js at all — fixed by adding a client-side script. (2) That script loaded mermaid via a CDN
ES-module import — worked in this session's own checks, but the user hit the exact same bomb/
"Syntax error in text" icon specifically in **VS Code's internal browser**. (3) Vendored
`mermaid.min.js` locally to remove the CDN dependency — **still failed in VS Code's internal
browser**, meaning that webview's CSP/sandboxing rejects client-side mermaid execution regardless
of where the script comes from, not specifically the network fetch. Confirmed at every step that
the diagram *content* was never the problem: parsed and fully rendered it through a real mermaid
engine twice — once via Deno+jsdom (bundled inside the Quarto install, no separate Node needed;
needs hand-built layout polyfills jsdom doesn't provide) for a fast syntax check, and once via a
genuine headless Chrome (this machine already has Chrome and Edge installed; drove it with
`puppeteer-core`, no download needed) for a real, properly-laid-out render — both succeeded
cleanly every time.

**Actually-final fix, found by checking how `docs/manuscript/paper_draft_5service.qmd` does it**
(the user pointed out its own workflow figure renders correctly, everywhere, without any of this
trouble): it uses Quarto's own **native `{mermaid}` executable-cell syntax** (curly braces), not a
plain ```mermaid fence. That's not a pre-rendered static image either — inspecting its rendered
HTML directly showed it's the *same fundamental mechanism* (mermaid.js inlined + an init script,
client-side) — but it's Quarto's own bundled, tested `mermaid-init.js` runtime, not any hand-rolled
loader. Converted all three files (`workflow_diagram.qmd`, `workflow_philippines_diagram.qmd`,
`swy_status_report.qmd`) to use this same native syntax instead of the custom static-SVG pipeline
built earlier today (now removed, along with the vendored `mermaid.min.js` and the pre-rendered
`.svg` files — none needed anymore). Verified all three rendered outputs now contain the exact
same `mermaid-init.js` / `class="mermaid mermaid-js"` pattern the manuscript's own already-working
figure uses. The `.md` source files (`workflow.md`, `workflow_philippines.md`) keep their plain
```mermaid fences unchanged, so GitHub/VS Code's own Markdown preview still shows a live diagram
there too — the `.qmd` files now carry a second copy of the same diagram source in `{mermaid}`
cell form, kept in sync by hand when a diagram changes (same bounded tradeoff as before, just via
a supported mechanism instead of custom tooling). **Not yet re-confirmed by the user in VS Code's
internal browser** — the Puppeteer/Deno render harness built along the way is still real, useful,
general-purpose diagnostic tooling (see the memory), just no longer the thing doing the actual
rendering in these three files.

**Update, 2026-09-15 late morning — three real fixes the user caught, all applied:**

1. **Report location was wrong.** Had been copying the rendered report into
   `data/swy/borneo/inputs/` (and briefly `data/swy/shared/`) — but `data/` is entirely gitignored
   (`.gitignore:52`), so anything placed there is invisible to git. Fixed: the report now lives
   only at its natural Quarto output location, `docs/reports/swy_status_report.html`, which *is*
   tracked. No more copy-to-`data/` step.
2. **"Her"/"our" language was unprofessional for a shared technical document.** Replaced
   throughout (report, `swy_methods.qmd`'s comparison section, and the three map/scatter-
   generating scripts) with grounded technical names: **WWF-SIPA baseline** (the original run,
   named for its own `.ini` section header) and **NCP Kc/CN run** (this project's isolated-
   variable run, matching the `ncp_kc_cn` suffix already used in its own output filenames).
   Verified zero remaining occurrences of "her"/"our"/"Becky" in the rendered report HTML. The
   Becky email draft keeps natural "your/mine" address, since it's a personal message directly to
   her — that's a different, correct register, not the same issue.
3. **Borneo dropped from the status report** (not from the project — still fully documented in
   `research_notes.md` and `swy_methods.qmd`), per the user's call that it was distracting from
   the Philippines comparison now that that's the focus. Removed: the Borneo input-status table,
   the whole Borneo "test plan" section (AOI/DEM build, run attempts, L_sum detail), the Borneo
   interactive map, and the Borneo mermaid pipeline diagram. Kept: brief, self-contained
   cross-basin mentions where genuinely useful (e.g. "qb lands in the same order of magnitude as
   the earlier Borneo run's own aggregate"). Retitled the report itself: "Seasonal Water Yield:
   Philippines Kc/CN comparison." "Findings for Rich" and "Open items" also rewritten to be
   current (added the two rain-events bugs found 2026-09-14/15, dropped stale items already
   resolved by the WWF-SIPA-inputs approach).

Also found and fixed while doing this: a **second instance of the same "unverified documentation
claim" pattern** as the raster-override error — `workflow_diagram.qmd` and this report both
claimed the mermaid pipeline diagram "actually renders to SVG," but neither one actually loaded
the Mermaid.js runtime; it was inert code in every browser. Fixed via `include-in-header` in both
files' frontmatter (loading mermaid.js from the same CDN already used for the Leaflet maps),
confirmed present in the freshly rendered output of both. Worth remembering: this project has now
found two separate "sounds right, was never actually checked" documentation claims in two days —
worth a slightly more skeptical read of older unverified claims generally, not just these two.

**With a real result now in hand, the next real decision is what to do with it**: decide
whether/how this becomes part of the paper or stays a methods-comparison side result, once
WWF-SIPA has actually seen and responded to it. Not decided; genuinely the user's call.

## LATEST (2026-09-14, earlier in the evening) — the comparison built and debugged, before the run finished

**What happened this session, in order** (this replaces "Becky's INPUTS folder is in hand" below,
which was the start of this same thread): Becky's `INPUTS_SP` folder (1.7GB, inventoried but not
yet opened at that point) got fully worked through — read in full, methodology built, run attempted
four times, three real bugs found and either fixed or deliberately deferred. Full technical detail
lives in `docs/swy/swy_methods.qmd`'s new "Test design: Philippines — the Becky-inputs comparison"
section and `docs/swy/research_notes.md`'s new 2026-09-14-evening entry — this is the summary:

1. **Confirmed her LULC is a custom 12-class WWF-SIPA typology, not ESA CCI.** Built a hand-matched
   crosswalk to this project's own ESA-keyed CN table (`gcn250_esa_lc_cn_table.csv`) — full table
   in `swy_methods.qmd`. Open/Barren's CN came out an exact match to hers, a good sanity check.
2. **A real, substantive Kc finding**: recomputed via this project's own EVI-regression method
   instead of her flat per-class values, Closed Forest Kc comes out at 0.52–0.70 (monthly-varying)
   against her flat 1.0 — independent evidence in the same direction as Corbari et al. (2017)'s
   real forest Kc measurements (see the long-running Kc/NDVI-saturation thread in `swy_methods.qmd`
   and prior HANDOFF entries). Not a validation of either number, but a real, citable data point.
3. **A real documentation error, unrelated to the Philippines work itself, caught while re-reading
   `swy_methods.qmd`**: it claimed `inspring` supports direct CN/Kc raster overrides, "confirmed by
   reading the code directly." Re-reading `seasonal_water_yield.execute()`'s actual source (found
   no such parameters at all) showed that claim was simply wrong — corrected in `swy_methods.qmd`
   and `docs/reports/swy_status_report.qmd` both. Likely origin, not confirmed: Becky's own `.ini`
   for her Philippines run does reference `CN_A_PATH` etc., but also uses several other parameter
   names that don't match this project's actual `inspring` build either — her workflow is most
   likely a different wrapper/fork, not evidence this project's own build ever had the feature.
   Worth asking Rich directly.
4. **Building the run script surfaced four real technical problems**, each fixed or deliberately
   deferred (full detail in `swy_methods.qmd`):
   - Two real upstream `inspring` bugs in the `user_defined_rain_events_dir` path (meant to ingest
     her real spatially-distributed rain events instead of a flat placeholder). One patched
     (`Python_scripts/swy_borneo_run/Dockerfile`, a source patch applied during the image build,
     same pattern as the existing `setup.py` fix). The other — bare filenames reused as both
     alignment input *and* output target, meaning a naive fix risks overwriting Becky's original
     files — deliberately **not** patched today; real feature-completion work, not a rushed fix.
     This run therefore uses the flat 18-events/month placeholder instead of her real product, a
     real, temporary loss from the original ask, documented rather than silently accepted.
   - CRS mismatch: her LULC is UTM 32651 (meters), everything else is WGS84 (degrees) —
     `inspring`'s alignment step doesn't reproject across CRSs itself. Fixed via GDAL Warp
     (mode resampling) before the run — **this is the step the current run may be stuck in**.
   - ET0 filename casing bug (`et0_v3_0X.tif` vs `et0_V3_0X.tif`, months 05-09 vs. the rest) would
     have silently scrambled month order via `execute()`'s plain-string sort — fixed via a
     normalized staging copy.
5. **Three real, clean upstream `inspring` bugs now found across this project's SWY work** (the
   `setup.py` packages-list omission from 2026-09-11, plus the two rain-events bugs today) — good
   candidates for a batched PR back to Rich's repo once the comparison work settles. Confirmed
   still relevant when asked directly this session — not urgent, not started.
6. **Decided at the user's explicit direction, not a default assumption**: use this project's own
   already-fetched SRTMGL3 DEM, since neither her `INPUTS_SP/` folder nor her `.ini` includes a DEM
   path at all — flag this assumption to Becky in the next communication rather than treating it as
   silently resolved. Also decided: don't spend more time on the mangrove/marshland CN
   simplification (collapsed to the same ESA analog as Closed Forest, mirroring what Becky's own
   table already does) — SWY's typical downstream-beneficiary/hydropower use case cares about CN
   accuracy only where something is actually downstream, and mangrove sits at the tidal/estuarine
   end of the watershed by definition. Explicitly noted as not necessarily true for flooded
   grasslands/savannas (the Llanos/Orinoquía analogue), which don't share mangrove's structural
   coastal position — check per-AOI geography before reusing this reasoning elsewhere.

**Concrete next steps for the next session, in order**: (1) check the run from point 4 above —
resume, restart with a faster reprojection approach, or investigate a real hang; (2) once a run
completes, sanity-check the actual output numbers and write them up; (3) draft the next Becky
communication covering the DEM assumption, the rain-events downgrade, and (if the run succeeded)
the headline Kc/CN comparison numbers; (4) the deferred rain-events bug and the raster-override
documentation question are both good things to ask Rich about, batched with the other PR-candidate
bugs — not urgent on their own.

## LATEST (2026-09-14 evening) — Becky's INPUTS folder is in hand, richer than expected — read this first

**The actual next SWY action, replacing everything below it in this section**: Becky's `INPUTS`
folder downloaded and sitting at **`data/swy/philippines/INPUTS_SP/`** (renamed from `INPUTS` —
Windows is case-insensitive, it collided with the already-existing `data/swy/philippines/inputs/`).
1.7GB. Not yet inspected in detail (deliberately stopped here to write this down before the
session ends) — inventory only, from a directory listing:

- **`biophysical_template_PH_revised.csv`** — looks like her **actual, real biophysical table**
  for this run (CN_A-D + Kc, presumably per-lucode), not the generic template of the same rough
  name found earlier in Rich's `swy_global` repo (`base_data/biophysical_template_PH.csv` —
  confirm these are actually different files, the "_revised" suffix suggests they are, but check).
  **This is the single most valuable file here** — it's her real Kc and CN choices, directly
  comparable to this project's own, not just her raw inputs.
- **`ph_baseline_lulc_md5_7f29da.tif`** — her actual LULC raster. **Check its classification
  scheme before assuming it's ESA CCI-compatible** — this project's own CN crosswalk
  (`gcn250_esa_lc_cn_table.csv`) is keyed to ESA CCI codes; if hers differs, it won't drop in
  directly. The biophysical template CSV above should reveal the lucode scheme either way.
- **`reclassified_ph_baseline_lulc_md5_7f29da_biophysical_template_PH_revised_CN_A/B/C/D.tif`** —
  her CN_A-D already reclassified to **rasters** from the LULC + biophysical table above. She's
  using the raster-override path directly, not the CSV-table route this project's own Borneo run
  used. Real, usable precedent for the raster-override approach this project has been treating as
  "the architecturally preferable long-term path, not yet done."
- **`Global-ET0_v3_monthly_tifs/`** — her ET0 source is **Global-AI_PET_v3** (CGIAR-CSI/Trabucco &
  Zomer's Global Aridity Index/PET database, per the bundled readme PDF), a **different product**
  than this project's own TerraClimate choice. Worth understanding the difference before treating
  ET0 as equivalent between the two runs.
- **`precip_PH_historical_climate_50/`** and **`n_events_PH_historical_climate_50/`** — monthly
  precip and monthly rain-event-count rasters, both named `historical_1985_2014_..._p50` — this is
  the real CMIP6 climatology (30-year span, "p50" = 50th percentile/median across ensemble
  members), and critically, **a real, spatially-distributed rain-events derivation** — a genuine
  upgrade over this project's own flat 18-events/month placeholder (see
  `08_build_rain_events_table.py` in both pipeline directories).
- **`wwf_PH_baseline_historical_climate.ini`** — almost certainly her exact `inspring` run
  configuration (routing parameters, file paths, `alpha_m`/`beta_i`/`gamma`, etc.) — read this
  first in the next session, it likely answers most remaining "what exactly did she do"
  questions in one file.

**First concrete steps for the new session**: (1) read the `.ini` file and the biophysical CSV in
full before touching anything else; (2) confirm the LULC classification scheme; (3) decide, now
that her real Kc/CN are visible, whether the plan is still "swap in our Kc/CN onto her other
inputs" or whether seeing her actual numbers changes that plan.

**Philippines EVI landed 2026-09-14 evening** — `data/swy/philippines/inputs/mod13a3_evi_2020/`
(12 EVI + 12 VI_Quality files; AppEEARS bundles the QA layer even though only EVI was requested,
same as the NDVI fetch). Downloaded via AppEEARS' manual web UI, landed loose in `inputs/` root,
moved into its own subfolder to match the `mod13a3_ndvi_2020/` convention. **Borneo EVI still not
landed** — that request was submitted separately (via Git Bash directly, which still 403'd — see
below) and would need the same manual-UI resubmission the Philippines one got, if not already
done. Check status before assuming both regions are ready.

**Real security incident, resolved safely, worth remembering**: while working through the AppEEARS
manual-download workaround, a fake "verify you're human" popup (the "ClickFix"/fake-CAPTCHA
malware-delivery pattern — instructs the victim to open Windows+R and paste/execute a command) got
this session's user close to running an unknown command. **Nothing executed** — the machine's own
UAC admin-rights prompt blocked it. No remediation needed, but worth being aware this pattern
exists and specifically targets people mid-technical-task, since a plausible "human verification"
step doesn't stand out in that context.

## LATEST (2026-09-14, earlier) — Becky gave a concrete SWY path forward; two WWF Colombia messages sent

**SWY: the actual next action is waiting on this session to get a Drive folder.** Becky's
2026-09-11 meeting went well, but afterward a real, unresolved Kc methodology gap surfaced (see
below) and blocked the Philippines comparison. Becky's response, 2026-09-14: she put every input
from her own Philippines baseline run into a new **`INPUTS` folder in the shared SWY Drive
workspace**, and wants this project to **reuse her exact inputs (her DEM, her land-use map instead
of ESA CCI, her CMIP6 climate stack) but swap in this project's own Kc and CN** — an isolated-
variable comparison, not a from-scratch climatology rebuild. **Blocking on: getting that `INPUTS`
folder downloaded/located** — ask the user for the Drive link, or confirm they've downloaded it,
before doing anything else on SWY. Two things to check once it's in hand, not yet known: (1) what
classification scheme her land-use map uses — this project's CN table is keyed to ESA CCI codes,
and if hers differs the crosswalk needs rebuilding before it can be used; (2) whether her original
shared baseline output (`data/swy/philippines/rich_shared/`, 8.6GB, still there — was briefly
"lost" after the 2026-09-11 reorg moved it, since re-confirmed) is still the comparison target or
superseded by this new approach.

**The Kc/NDVI gap, in brief** (full detail in `docs/swy/swy_methods.qmd`, live reasoning in
`docs/swy/research_notes.md`'s 2026-09-11 evening entry): Rich asked directly whether Kamble et
al. (2013)'s NDVI-Kc regression — this project's method for non-cropland Kc, used in the completed
Borneo run — is valid for forest, not just the agricultural crops it was actually calibrated on.
Checked rather than reassured: confirmed agricultural-only, found Corbari (2017, real forest Kc
measurements, lower than assumed) and Glenn et al. (2011, confirms NDVI saturation in dense
canopy, recommends EVI, cites a still-unread tropical precedent — Negrón Juárez et al. 2008,
Amazonia, paywalled). **Decision made, not yet executed: switch the Kc pipeline from NDVI to
EVI** — same MOD13A3.061 product already fetched for both Borneo and the Philippines, just a
different layer. AppEEARS request files for both regions' EVI already built
(`data/swy/borneo/inputs/borneo_evi_full_request.json`,
`data/swy/philippines/inputs/ph_evi_full_request.json`) — **check with the user whether these were
submitted over the weekend** (NASA's AppEEARS API started blocking programmatic task
submission/download mid-session on 2026-09-11 — public metadata endpoints stay open, `/task` and
`/bundle` return a blanket 403 regardless of payload size or browser-like headers tried; worked
around via AppEEARS' own web UI, uploading a hand-built request JSON — same workaround needed for
these EVI requests if not already done).

**WWF Colombia — real progress, two messages sent 2026-09-14, both awaiting replies.** Camila
Cammaert's 2026-09-11 meeting went very well — informally invited into a CIAT sustainable-cacao
meeting also attended by César Freddy Suárez and Carlos Mauricio Herrera; four ministers
(Agriculture, Environment, Transportation, Hacienda-adjacent) doing a regional flight that week as
part of a transformation agenda; WWF Colombia has a deliberately diplomatic relationship with the
new administration; the "no former C-level holdovers" rule doesn't affect the user (wasn't
in-country under the prior administration). Two follow-ups drafted and sent: to Camila (glad to
finally meet, found the CIAT session valuable, offered Altillanura expertise for future meetings,
asked to stay in the loop and see the slides); to César/Carlos Mauricio, reframed around having
also been at that same CIAT meeting together rather than as a stale follow-up to their earlier
email exchange — picked the Smurfit land-cover accuracy-verification work specifically as the
concrete next step (direct match to LC_orinoquia background), also offered a technical session
showing this project's ecosystem-services workflow. **Both sent — waiting on responses, nothing
else to do here until they reply.**

**Session hygiene**: a commit was made 2026-09-14 covering the SWY data reorg, the Philippines
pipeline, and all the methodology doc updates (see git log). Not pushed. The message drafts
(`docs/swy/message_draft*.md`) were deleted after sending — don't look for them.

## Where things actually stand

- **Contract**: postdoc ends **2026-10-24**. The 2026-09-09 13:30 meeting with Becky went well —
  extension through end of year requested, she's checking, "good chances" per the user. She's
  actively helping build the case (internal or contractor/consultant), citing SWY progress, the
  WWF Colombia integration, and the `global_invest_dev` contribution work specifically. Also asked
  the user to be added to the NatCap Alliance staff directory. Full detail:
  `project_capability_portfolio_pitch.md` memory.
- **NatCap Alliance directory**: bio finalized (land systems science framing, political ecology +
  economics explicitly named per user request, no em dash, no specific-project detail like SWY —
  broad research arc only), publications picked (2024 GIScience & Remote Sensing paper leading,
  2021 IJRS paper second; older/off-topic ones deliberately excluded). **Still blocking: no
  headshot chosen** — user's local candidate photos didn't work out, was going to check Google
  Photos (not accessible from this session). Email to `elanak@stanford.edu` (cc Becky) drafted,
  not sent — waiting on the headshot.
- **LinkedIn About section**: rewritten once (dropped the stale "transitioning to industry"
  framing, cut generic skills-list filler, led with current WWF role). User's verdict: "reads
  comically LinkedIn" — wants another pass, not done, not urgent.
- **The paper**: sent to Becky/Steve 2026-09-07. **Becky replied with 14 numbered review comments**
  on `docs/manuscript/paper_draft_5service.pdf` (gitignored — has her comments in it, not for git
  history). Comments range from quick fixes (remove internal draft notes before circulating,
  terminology tweaks) to a real, large lift (Section 3.1 needs substantial expansion, "a third of
  the paper" in Becky's words) to two new figures and two discussion-section reframes. Full
  categorized breakdown with effort estimates: this conversation's own history (not yet moved into
  a repo doc — worth doing if this thread doesn't carry forward). **A response to Becky was
  drafted, corrected by the user (fixed a typo), and given as final — sending status not
  confirmed. Check with the user before assuming it went out.** That response also covers the
  paper AND, per explicit sequencing instruction, folds in the WWF Colombia updates below (draft
  sequencing was: paper response first, only then resume devstack work).
  - **One flagged discrepancy needs the user's own verification before responding further**:
    Becky's comment BC6 says a 1.8× income-intensity stat "was already removed from Results for
    the spatial-dependency reason discussed" — but that pattern is still visibly present in
    Section 3.2 (Figure 7, Table 4) as drafted. The user deliberately dropped this from the sent
    response to check it themselves first — don't raise it again until they have.
- **WWF Colombia — see LATEST above for current status** (Camila meeting detail, César/Carlos
  Mauricio thread, both messages sent 2026-09-14). Full background: `project_capability_portfolio_pitch.md` memory.
- **SWY — see LATEST above for the current blocker (Becky's `INPUTS` folder) and the Kc/EVI
  situation.** Stable historical facts, still accurate background:
  - Borneo pivot decided with Becky 2026-09-09; every raw input downloaded and content-verified
    2026-09-10. `data/swy/borneo/inputs/` (~1.1GB) has the full package + README with data
    dictionary. Real architecture fact: `inspring` accepts CN/Kc as **direct raster overrides**,
    not only a lucode CSV — the raster-override path is architecturally preferable long-term but
    the completed Borneo run used the simpler CSV path instead (documented, deliberate).
  - **The Borneo run itself completed successfully, 2026-09-11, second attempt after a first one
    was cut short by the laptop sleeping mid-run** (fixed: `standby-timeout-dc 0` + an active
    keep-awake process). QF mean 406.8mm/yr, AET mean 1288.0mm/yr — both physically plausible for
    tropical rainforest. **One genuine, still-open numerical issue, unrelated to the Kc question**:
    ~5% of `L_sum` (flow-accumulation) pixels show a runaway anomaly, clustered at one coastal
    river-mouth location, consistent with (not confirmed as) SRTM noise in flat tidal terrain —
    reproduced independently across both run attempts, not a crash artifact. Full detail:
    `docs/swy/research_notes.md`'s 2026-09-11 entries.
  - Whole pipeline consolidated into real, re-runnable scripts: `Python_scripts/swy_borneo_run/`
    (01-11 + Dockerfile + README, including a "Findings for Rich" section on real `inspring`
    packaging bugs) and, added 2026-09-11, `Python_scripts/swy_philippines_run/` (mirrors the
    Borneo structure; AOI, DEM, NDVI, and LULC all landed for the Philippines — see that
    directory's own README for exactly what's done vs. still needed).
  - Data package uploaded to Drive and Becky notified, 2026-09-11 — done, not an open item.
- **Devstack pilot restructure — done and verified, 2026-09-09.** `calculate_bitemporal_change.py`
  split into `run_/tasks_/functions_` files, verified two ways: 5 new pytest tests
  (`Python_scripts_tests/`) all pass, and — the real test — original vs. refactored script outputs
  diffed byte-for-byte in Docker against the real 924MB production input: zero mismatches.
- **Contribution-PR candidate for `global_invest_dev`, found but explicitly deprioritized.** A new
  `gep_summary_crosstabs`-style function in `global_invest/utilities.py`, used to finish migrating
  `coastal_protection_results.qmd` off its hand-rolled duplicate of shared groupby logic. Two real
  bugs in `pollination_tasks.py` as a secondary option. **Sequencing per explicit user
  instruction**: respond to Becky on the paper first, only then resume this — check whether that's
  actually happened yet before starting.
- **Archived 2026-09-10**: `docs/swy/swy_becky_meeting.qmd` + `.html` and
  `docs/swy/tuesday_meeting_script.md` — materials for the 2026-07-21 Becky meeting, superseded by
  everything since. Moved to `docs/archive/`, not deleted.

## Drafts — status as of 2026-09-14

All message drafts from the 2026-09-11/14 SWY and WWF Colombia threads (Becky, Camila, César/
Carlos Mauricio) were sent and deleted — don't look for `docs/swy/message_draft*.md`, they're gone
on purpose. Still outstanding from earlier:

- **Paper-comments response to Becky** — drafted and corrected 2026-09-11, **sending status still
  unconfirmed as of 2026-09-14** (three days of SWY/WWF Colombia work happened in between without
  this coming up again — genuinely check with the user, don't assume either way).
- `docs/swy/rich_swy_status_and_asks_2026-09-08.draft.md` — **not sent, stale**, superseded by the
  live Slack conversation with Rich that already happened 2026-09-11. Probably dead; confirm with
  the user before deleting.
- `docs/justin_devstack_outreach_2026-09-08.draft.md` — **not sent**, still deprioritized behind
  the devstack contribution PR.

## Pending — in priority order

1. **Get Becky's `INPUTS` Drive folder and start the Philippines re-run using her inputs + this
   project's Kc/CN** — see LATEST above. The actual next SWY action.
2. **Switch the Kc pipeline from NDVI to EVI** — decision made, not executed. EVI AppEEARS request
   files already built for both regions; confirm whether submitted, then build EVI into the
   biophysical-table script (`07_build_biophysical_table.py` in both `swy_borneo_run/` and
   `swy_philippines_run/`).
3. **Read Negrón Juárez et al. (2008)** (Amazonia, tropical rainforest EVI-ET) — paywalled,
   couldn't get past Taylor & Francis; needs the user's institutional access.
4. **Confirm whether the Becky paper-response was actually sent** — see Drafts above.
5. **Check for replies from Camila and César/Carlos Mauricio** — both messages sent 2026-09-14,
   nothing to do until they respond.
6. **L_sum flow-accumulation anomaly** (Borneo, ~5% of pixels) — real, diagnosable, not blocking
   the "it runs" finding, still uninvestigated.
7. **Devstack contribution PR** — resume only after the Becky paper-response sequencing above is
   actually confirmed done. Fork already exists (user's own GitHub); local remote + branch setup
   not yet done.
8. **NatCap directory + LinkedIn** — headshot still needed; LinkedIn About needs a less-generic
   rewrite pass.
9. **Book figure regeneration** — `docs/pipeline_reference.md` row G. No rush.
10. Mangrove/flooded-grasslands CN patch — not a blocker, still needs an eventual communicated
    position (`docs/swy/research_notes.md`).
11. **Broader `data/` cleanup**, user-requested 2026-09-11, not started beyond the SWY reorg. Not
    urgent; don't start unprompted.
12. **Push the 2026-09-14 commit** — made locally, not pushed to `origin/feature/devstack-compat`.
    Ask before pushing.

## Key file locations

- `docs/devstack_compat_research_notes.md` — devstack comparison (corrected against `develop`),
  verdict, contribution-PR candidate detail, adoption checklist (pilot restructure done).
- `docs/swy/research_notes.md` — SWY research log, "Open questions" is the live checklist.
- `docs/swy/workflow.md` — **new 2026-09-10**, mermaid pipeline diagram with live status coloring
  (done/in-progress/blocked) for the whole Borneo input chain. Update alongside this file rather
  than letting it drift.
- `docs/swy/swy_methods.qmd` — **new 2026-09-10, replaces `model_specification.md`** (archived to
  `docs/archive/model_specification_2026-07-16.md`, not deleted). The permanent conceptual/methods
  document: precedent review, CN and Kc methodology with real formulas, the raster-override
  architecture note, open research questions. Deliberately **not** tied to any specific meeting's
  framing — that's what `swy_status_report.qmd` is for. Cites `docs/swy/references.bib` (22
  entries, converted from `literature_review.ris` via `pandoc -f ris -t biblatex` with hand-cleaned
  citation keys — pandoc reads `.ris` bibliographies directly, no conversion is strictly required,
  but the auto-generated keys from RIS are unwieldy multi-author strings, not worth using as-is).
  Keep both `.ris` and `.bib` in sync when adding sources — `.ris` is the reference-manager-style
  master, `.bib` is what Quarto actually cites from.
- **A three-way documentation split, decided 2026-09-10**: `workflow.md` = current status (the one
  place that should claim this — others should point to it, not duplicate it), `research_notes.md`
  = chronological reasoning log (why decisions were made, never "current state"), `swy_methods.qmd`
  = permanent conceptual/methods reference, `swy_status_report.qmd` = disposable, meeting-tied
  snapshot memo, regenerated fresh rather than kept permanently current. **Folder-level note added
  2026-09-16, since the split confused a fresh read**: the first three live in `docs/swy/`
  (working materials); the status report lives separately in `docs/reports/swy/` (shareable
  rendered output), matching the same `docs/reports/<topic>/` convention already used for
  `colombia_clec/` and `phase4_beneficiary/` — the two folders sharing the name "swy" is
  coincidental, not a sign one is stale.
- `docs/reports/swy/swy_status_report.qmd` — SWY status memo **source** (own
  `swy_report_styles.css`, one `_output_map_ph.html` interactive-map include, and the two
  `swy_ph_comparison_scatter_*.png` scatter plots all alongside it in the same folder). The
  rendered, standalone copy that actually goes out is `docs/reports/swy/swy_status_report.html` —
  just re-render in place if the source changes, no separate copy step (an earlier
  `data/swy/shared/` copy step was dropped 2026-09-15 morning since `data/` is gitignored).
  **Moved into its own `swy/` subfolder 2026-09-15 evening**, mirroring the `colombia_clec/` and
  `phase4_beneficiary/` report-folder pattern — docs/reports/ top level had accumulated loose
  per-report qmd/html/css/png files and the user asked for the same self-contained-folder
  treatment here. The Python scripts that generate the map/scatter assets had their hardcoded
  output paths updated to match; prose mentions of the old top-level path elsewhere
  (`research_notes.md`, `swy_methods.qmd`, `workflow_philippines.md`, `WORKLOG.md`) were **not**
  swept — low-value churn given time spent this session, harmless since
  they're either historical-log entries or still findable by filename.
- `docs/swy/workflow_diagram.qmd`/`.html` — **new 2026-09-11**, a thin Quarto wrapper that
  `{{< include >}}`s the live `workflow.md` so its mermaid diagram actually renders to SVG (a bare
  `.md` render leaves it as an inert code block — Quarto's mermaid engine needs real `.qmd`
  context). `workflow.md` stays the single edited source; this is just how to view it properly
  rendered, standalone.
- `Python_scripts/swy_philippines_run/` — **new 2026-09-11**, mirrors `swy_borneo_run/`'s
  structure (numbered scripts + README). `01_build_aoi_from_rich_mask.py`'s docstring documents a
  real false start worth reading before reusing the pattern: building an AOI from a partner's
  *routing domain* (watershed_subset files) overshoots badly (includes upstream contributing area
  with zero retained output) — build it from the *output raster's valid-data footprint* instead.
- Two literature PDFs added to `docs/swy/`, both read in full and cited in `swy_methods.qmd`:
  Corbari et al. (2017, *Sensors*) and Glenn et al. (2011, *Hydrological Processes*) — both were
  paywalled everywhere WebFetch tried (MDPI, Wiley, ResearchGate all 403'd); the user pulled them
  via institutional access. Same will likely be needed for Negrón Juárez et al. (2008, still
  unread, pending item #3 above).
- `data/swy/` — **reorganized 2026-09-11**, all SWY data now lives here: `shared/` (cn_tables,
  soil_hydrologic_group, hydrobasins — genuinely cross-region), `borneo/{inputs,raw_downloads,lulc,
  workspace}/`, `philippines/{inputs,rich_shared}/`. Replaces the old scattered top-level
  `swy_shared_package/`, `borneo_lulc/`, `dem_borneo/`, etc. — all script path references updated
  and reverified. `data/swy/borneo/inputs/` is the full Borneo data package, own README with data
  dictionary. `data/` still has real clutter beyond SWY (other-service data, `raw/`/`processed/`/
  `interim/`/`external/`) — noted as a future cleanup item in Pending, not done.
- `docs/manuscript/paper_draft_5service.pdf` — Becky's reviewed draft with her 14 comments
  (gitignored, not in git history).
- `docs/pipeline_reference.md` — step-by-step tracker; row G is the book-figure-regeneration list.
- `docs/runbook.md` — narrative/gotcha detail, paired with `pipeline_reference.md`.
- `docs/methodology.md` — durable analytical/scientific framework reference.
- `docs/archive/` — superseded materials kept for reference, not deleted (codex_context files,
  the old hotspot-redesign docs, and now the 2026-07-21 SWY meeting materials).
- `Python_scripts_tests/` — first real test coverage in this repo (pytest, local venv).
- Memory worth reading first in a fresh session: `project_capability_portfolio_pitch.md` (contract,
  WWF Colombia, NatCap directory, LinkedIn), `project_swy_model_integration.md` (Session 9 entry is
  the Borneo pivot), `project_natcap_tools_crash_course.md`, `project_mehrabi_covariate_doubt.md`
  and `project_beneficiary_mask_finding_pending.md` (both still queued for a future Becky
  conversation).
