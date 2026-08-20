# Handoff: Orinoquía dissertation research → Sandra Valenzuela / Colombia proposal

**Purpose of this doc:** `sandra_valenzuela_colombia_case.qmd` (this same folder) already
references Jerónimo's PhD dissertation on the closing "La oferta" slide, deliberately
without specific numbers ("no mencionar cifras específicas todavía, el análisis aún no
está en su forma final" — see that slide's speaker notes). This doc gives the current,
real state of that dissertation research, from the working repo
(`~/personal/LC_orinoquia`, private, separate from this one), so that whoever next edits
this deck or the broader WWF Colombia proposal has grounded context instead of having to
guess. **This is a status snapshot as of 2026-08-19** — check `LC_orinoquia/docs/WORKLOG.md`
(newest entries at top) for anything more recent before relying on it.

---

## What the dissertation actually is

PhD, Geography, Temple University, defended 2025. Subject: agricultural-frontier land
cover change in the **Piedmont and Altillanura subregions of the Orinoquía**, Colombia.

**Precision worth keeping in the proposal**: this is NOT the whole Orinoquía region. It
explicitly excludes Casanare/Arauca's flooded savannas — a real methodological
distinction, not a simplification. If the proposal or deck ever names the study area
more specifically than "Orinoquía," it should say Piedmont/Altillanura, not the full
region.

### Chapter structure (dissertation author's own priority ranking)

| Chapter | Topic | Status / priority |
|---|---|---|
| Ch.1 | LCoverFlow package/methodology | Moderate contribution; possible JOSS-style software paper |
| **Ch.2** | **Frontier suitability/accessibility dynamics** | **Strongest chapter, top priority. Target: submission-ready by end of 2026.** This is the chapter the deck's "La oferta" slide is actually pointing at. |
| Ch.3 | Land control / political ecology | Weakest, most sensitive. Deliberately last priority, not part of the current push. |

## Current empirical status (Ch.2 rebuild) — as of 2026-08-19

The dissertation's land cover classification pipeline is being rebuilt from scratch for
reproducibility (it wasn't, originally — that's explicitly one of the things being fixed).
It's a 6-step pipeline; here's where each step stands:

1. **Data acquisition — ~95%+ done.**
   - GLAD Landsat ARD (Piedmont, years 2003/2011/2021/2024 + gap-fill windows):
     nearly complete, final stragglers being mopped up now.
   - JAXA PALSAR/PALSAR-2 (radar): acquisition code built and **verified against real
     downloaded data** (band-file pattern matching + calibration formula both confirmed
     correct, 2026-08-19). Real, confirmed coverage gap: no PALSAR data exists for
     2011–2014. The dissertation's own documented fix (directly from the Ch.2 text) is
     followed here: train parallel with/without-PALSAR Random Forest models on a
     reference year to quantify what PALSAR actually adds, then apply accordingly per
     target year.
2. **Predictor stack** (spectral statistics + phenology curve features): not started.
3. **Classification** (Random Forest): not started — and **blocked** on a real, unresolved
   risk (next section).
4–6. **Post-processing, trajectory extraction, accuracy validation**: not started.

### The actual blocker: training data

The dissertation's validation dataset (4,284 points, matches the published manuscript
number) is safe and git-tracked. **The training shapefiles used to fit the original
Random Forest models only exist on a Temple University OneDrive account that's been
inaccessible since graduation** (~1 year logged out). Several recovery paths exist
(re-auth attempt, asking a former collaborator, Temple IT alumni policy, an unconnected
external backup drive) — as of this doc, **none have been attempted yet**. This is the
critical-path item for Step 3, not compute or download time.

## What this means for the proposal / deck, concretely

**Don't cite any new numbers from this rebuild yet.** Nothing past Step 1 has run, so
there are no new classified maps, no new accuracy figures, no new trajectory statistics
to point to. The deck's existing discipline on this ("no mencionar cifras específicas
todavía") is correct and should stay in place until Step 3 actually produces something.
Anything citable right now is limited to what's already in the defended dissertation text
itself — not this rebuild.

**A genuinely strong, evidence-based integration point** (stronger than the deck's current
generic "my own research is relevant" framing): the Sandra deck's own hotspot analysis
*independently* flags the same geography this dissertation studies —

- "Concentración más fuerte en el eje cafetero, los Andes centrales y **el piedemonte de
  la Orinoquía**" (hotspot map slide)
- "Polinización concentra en **sabanas/Llanos (1,33×)**... presión sobre paisajes
  agrícolas y de pastizal **en la frontera de la Orinoquía**" (biome slide)

That's WWF's own global ecosystem-service pipeline landing on the Piedmont/Llanos
agricultural frontier as a concentration zone for pollination-service change — using a
completely independent method and dataset from the dissertation. Worth stating plainly
in the proposal: two unrelated analyses, WWF's global NCP hotspot detection and a
dissertation built from ground-level classification, converge on the same geography as a
priority zone. That's a much more concrete hook than "I also study this region."

**Timeline reality check**: end-of-2026 for a submission-ready Ch.2 manuscript is
~4.3 months out from this doc's date. Step 1 (data) is in good shape; Steps 2–6 haven't
started, and Step 3 is gated on a training-data recovery effort that hasn't begun. If the
proposal timeline assumes new Orinoquía-specific figures will exist soon, that assumption
should be checked against this before it's stated to Sandra or anyone else.

## Where to look for more detail

- `~/personal/LC_orinoquia/docs/WORKLOG.md` — full running log, newest entries first.
- `~/personal/LC_orinoquia/docs/methodology.md` — per-pipeline-step status.
- That repo is private and separate from `global_NCP`; this doc is a one-way status
  export, not a live link — re-check WORKLOG.md rather than assuming this stays current.
