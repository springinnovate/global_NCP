# CONTEXT: global_NCP rolling handoff

**Maintenance note**: this file is updated in place, not superseded by a new dated file each
session. When resuming or wrapping up, edit this doc directly — update stale sections, fold in
new findings, don't create `HANDOFF_<date>.md`. Paste this whole file into a fresh Claude Code
session in `c:\projects\global_NCP` to resume.

*Last updated: 2026-09-10.*

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
- **WWF Colombia — two live threads, real openings, nothing scheduled yet.**
  - César Freddy Suárez Pacheco + Carlos Mauricio Herrera: Bogotá land-cover mapping (Secretaría
    Distrital de Ambiente, Corine methodology), a Smurfit convenio needing classification-accuracy
    verification (**strongest capability fit**, direct match to LC_orinoquia background), an
    ongoing IUCN NbS-criteria discussion.
  - **Camila Cammaert** (WWF Colombia, Sustainable Food Systems/Agriculture Coordinator) — meeting
    was scheduled for 2026-09-10 (today, as of this writing). Outcome not yet known to this
    session — ask the user how it went. Her focus (agricultural supply chains, food-systems
    sustainability) connects directly to the user's own oil-palm/agricultural-frontier dissertation
    work, not just the general pipeline — that overlap was the prep angle going in.
  - Full detail: `project_capability_portfolio_pitch.md` memory, 2026-09-09 entries.
- **SWY — Borneo pivot decided with Becky 2026-09-09; every raw input downloaded and
  content-verified as of 2026-09-10.** **Meeting with Becky and Rich together confirmed
  2026-09-11 (tomorrow) ~11am** — the earlier "Friday 2026-09-12" recorded elsewhere was wrong,
  corrected by the user directly 2026-09-10. **Also tomorrow: Camila Cammaert meeting, 8am**
  (before the SWY one) — user is prepping that one themselves (reading her paper), not delegated
  to this session. Real goal for the SWY meeting, per explicit user framing: a clear report + data
  uploaded to Drive + one honest attempt at the actual run — success isn't required, but
  documenting clearly why it didn't work (if it doesn't) is, so Rich can pick it up with the
  shared data. Paper edits to Becky's 14 comments are a soft target, not a hard deadline — no
  need to show anything on that tomorrow.
  DEM, NDVI, precip, and ET0 all landed and were actually sanity-checked (not just "task says
  done") this session. `data/swy_shared_package/` is ~1.1GB, README rewritten with the full data
  dictionary. Real architecture correction found: `inspring` accepts CN/Kc as **direct raster
  overrides**, not only a lucode CSV — shrinks the earlier "lucode master table" gap down to a
  small 3-way land-cover mask. **Full detail — task IDs, exact verification numbers, gotchas
  (curl redirects, `.netrc`, R/GDAL segfaults) — lives in `docs/swy/research_notes.md`'s
  2026-09-09/10 entries, not here.** Current status: `docs/swy/workflow.md`. Permanent
  conceptual/methods reference: `docs/swy/swy_methods.qmd`.
  - **Data package uploaded to Drive and Becky notified, 2026-09-11 (user's own action, confirmed
    done).** No longer an open item — see the corrected `Pending` list below.
  - **2026-09-10/11, the whole pipeline consolidated into real, re-runnable scripts**:
    `Python_scripts/swy_borneo_run/` (numbered 01-09, plus `Dockerfile`, `appeears_common.py`,
    and its own `README.md`) — AOI build through the actual model call, no longer only ad-hoc
    interactive commands. See that README's "Findings for Rich" section for concrete,
    reproducible bugs found in `inspring`'s packaging (not vague "it didn't work").
  - **Two run attempts, 2026-09-11 — the second completed cleanly, and it changed the diagnosis.**
    First attempt: laptop sleep killed Docker mid-run (battery sleep was still on a 60-min timer
    despite AC sleep already being off); the `L_sum` routing step was cut off almost exactly at
    the equator, and the aggregated `qb=1705.005` from that run was suspect. Fixed properly before
    rerunning: `standby-timeout-dc 0` plus an active `SetThreadExecutionState` keep-awake process
    for the run's duration (belt-and-suspenders, given this is an AzureAD-managed laptop where
    `powercfg` couldn't even confirm the lid-close-action setting). Old incomplete workspace moved
    aside, not deleted (`data/swy_borneo_workspace_INCOMPLETE_run1_2026-09-10/`), so `taskgraph`
    couldn't silently reuse the broken cached `L_sum`.
    **Second run: exit code 0, full spatial coverage confirmed.** QF/AET identical to the first
    run (406.8mm/yr, 1288.0mm/yr — expected, don't depend on routing). **The real finding: the
    L_sum flow-accumulation anomaly (~5% of pixels, values into the billions) persisted at the
    same order of magnitude with full coverage — it is a genuine, reproducible issue, not a crash
    artifact.** Visually (via the new interactive map — see below) it clusters most visibly at one
    specific coastal river-mouth location, consistent with (not confirmed as) SRTM noise in flat
    tidal terrain. Separately: the aggregated `qb` value came out **numerically identical** between
    the broken and complete runs (1705.005), suggesting the AOI-wide aggregate itself is more
    robust than the pixel-level anomaly implies. Full detail: `docs/swy/research_notes.md`'s two
    2026-09-11 entries.
  - **Report, map, and shared package all re-rendered/re-copied with the complete-run numbers** —
    but **the version the user uploaded to Drive last night reflects the incomplete run's
    framing and needs re-uploading.**
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

## Drafts — status as of 2026-09-10

- `docs/becky_global_invest_dev_update_2026-09-08.draft.md` — **sent 2026-09-09** (SWY thread
  reply + separate devstack/meeting-ask message, trimmed by the user before sending).
- **Paper-comments response to Becky** — drafted and corrected in-conversation this session, not
  yet saved as a file, **sending status unconfirmed**. If picking this up fresh, ask the user
  directly rather than assuming either way.
- `docs/swy/rich_swy_status_and_asks_2026-09-08.draft.md` — **not sent, stale** (Peru/Myanmar/
  Perrine paragraph no longer applies after the Borneo pivot). The two technical questions
  (ecoshard version, `root_depth`) are still real — probably better raised live at Friday's
  meeting with Rich than as a Slack message now. Needs a user decision, not another silent edit.
- `docs/justin_devstack_outreach_2026-09-08.draft.md` — **not sent**, shouldn't be until the
  contribution PR happens — deprioritized along with it.

## Pending — in priority order

1. **Confirm whether the Becky paper-response was actually sent** — check with the user first,
   don't assume.
2. **All SWY raw-input acquisition is done** (2026-09-10) — AOI, DEM, soil, routing, CN base,
   NDVI, precip, ET0 all downloaded and content-verified. Nothing left to acquire; see the
   "SWY — every raw input" note above and `docs/swy/workflow.md` for what's actually left
   (Kc/CN raster construction, rain events derivation, then the model call itself).
3. ~~Upload `data/swy_shared_package/` to the shared Google Drive~~ — **done, 2026-09-11**, user
   confirmed uploaded and Becky notified.
4. **The Borneo SWY run itself — see the detailed status above** (first attempt completed but
   `L_sum`/baseflow was cut short by a Docker crash; a clean rerun was in progress as of the last
   session, outcome to confirm before the 11am meeting).
5. **Today (2026-09-11): meeting with Becky and Rich together, ~11am** — SWY (Borneo results),
   paper, devstack, all converging here. Camila Cammaert meeting is earlier the same morning,
   ~8am, user prepping separately.
6. **How did the Camila Cammaert meeting go?** — ask, fold in whatever came of it.
7. **Decide what to do with the stale Rich draft** — resend trimmed, fold into Friday's live
   conversation, or something else. Ask, don't pick unilaterally.
8. **Devstack contribution PR** — resume only after the Becky paper-response sequencing above is
   actually confirmed done. Fork already exists (user's own GitHub); local remote + branch setup
   not yet done.
9. **WWF Colombia** — César's three openings, Camila's food-systems angle, both unscheduled.
10. **NatCap directory + LinkedIn** — headshot still needed; LinkedIn About needs a less-generic
    rewrite pass.
11. **Book figure regeneration** — `docs/pipeline_reference.md` row G. No rush.
12. Mangrove/flooded-grasslands CN patch — not a blocker for Becky, still needs an eventual
    communicated position (`docs/swy/research_notes.md`).

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
  `data/swy_shared_package/swy_status_report.html` — re-render and re-copy if the source changes.
- `data/swy_shared_package/` — the full Borneo data package, own README with data dictionary.
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
