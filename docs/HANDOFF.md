# CONTEXT: global_NCP rolling handoff

**Maintenance note**: this file is updated in place, not superseded by a new dated file each
session. When resuming or wrapping up, edit this doc directly — update stale sections, fold in
new findings, don't create `HANDOFF_<date>.md`. Paste this whole file into a fresh Claude Code
session in `c:\projects\global_NCP` to resume.

*Last updated: 2026-09-14 (evening).*

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
  snapshot memo, regenerated fresh rather than kept permanently current.
- `docs/reports/swy_status_report.qmd` — SWY status memo **source** (own `swy_report_styles.css`
  alongside it). The rendered, standalone copy that actually goes out is
  `data/swy/borneo/inputs/swy_status_report.html` — re-render and re-copy if the source changes.
  Now includes a Philippines section and the full Kc/EVI caveat, not just Borneo.
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
