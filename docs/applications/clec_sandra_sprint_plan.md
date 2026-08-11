---
title: "Sprint plan — CLEC abstract, Tremie response, Sandra materials"
status: "STRICT — Mon 9AM to Wed 11AM, 8h/day (Wed: 9-11AM only, 2h)"
date: "2026-08-07"
---

## Why this exists

Three deliverables converge on Wednesday (2026-08-12): the CLEC congress abstract deadline, a
reply to Tremie on the course-design ask, and the Sandra Valenzuela / WWF Colombia meeting
(same day, right after a colleague lunch used as a practice run). Full background:
`docs/applications/colombia_capability_portfolio.md` (source of truth for strategy/context —
read that first if picking this up cold) and `docs/hotspots_rasters_data_dictionary.md` (the
category matrix + diagram, needed to talk about the methodology correctly).

**Governing strategy**: one core deliverable — a Colombia-scoped, Spanish-language narrative of
the hotspot/beneficiary-exposure methodology — feeds all three outputs, built once and adapted,
not three separate builds.

**What's already in hand, no new pipeline work needed**:
- Colombia per-service disproportionality: `data/processed/tables/regional_subsets/nev_name/
  hotspot_area_stats_Colombia.csv` — Pollination 1.83×, Sediment export 1.51× are the headline
  numbers (Coastal Risk's 0.12× needs a denominator check before use, see caveat below).
- Colombia beneficiary population reach: `data/processed/tables/colombia_beneficiary_population.csv`
  — combined cross-category 75.8% / 38.1M people, 4+ tier 35.5% / 17.9M people (defensible
  numbers; 1+/access-only are inflated by buffer reach, don't lead with those).
- Verified buffer parameters: 50km downstream (DEM flow-path, not a radius) + 1-hour travel-time,
  confirmed directly against Rich's configs — safe to state precisely.

## Monday — Foundation + Tremie response

**9:00–10:00 — Decisions before building anything**
- [ ] Confirm scope: Becky's exact 3 categories (combined-cross, 3+, 4+) for paper-consistency,
      or the fuller set (water-only/access-only likely tell a better *roads* story specifically)?
- [ ] Resolve the Coastal Risk 0.12× denominator issue (same-land-cell-denominator artifact
      flagged this week) before it can appear in anything external.
- [ ] Decide oral vs. poster for CLEC.

**10:00–12:00 — Colombia content build**
- [ ] Colombia-zoomed hotspot map (crop of `make_lac_hotspot_map.R`'s approach), colored by
      Pollination and/or Sediment export specifically — the two standout disproportions.
- [ ] Colombia-zoomed beneficiary/exposure map (combined-cross or 4+ tier + population overlay).
- [ ] Percent-vs-percent framing for the headline stat (not bare "1.83×" — same fix already
      applied to the LAC deck, see `project_relative_intensity_reframing` memory).

**13:00–15:00 — Core narrative outline (Spanish)**
- [ ] WHERE (disproportionate concentration) → WHO (beneficiary reach) → WHY IT MATTERS FOR ROADS
      (sediment export is directly road-construction/runoff relevant — the clearest bridge to a
      road-ecology audience) → the ask/offer (standing Colombia screening capability).

**15:00–17:00 — Tremie reply**
- [ ] Short email: (a) confirm intent to submit a CLEC abstract, (b) respond to the course-design
      ask — express interest in Módulo 5 ("Priorización de intervenciones: hotspots, modelos
      predictivos y conectividad") and/or the open "Docente invitado(a) de la región" slot,
      without over-committing to full course design by Wednesday — that's explicitly a mid-term
      item, not due this week.

## Tuesday — Build the deliverables

**9:00–11:00 — CLEC abstract draft**
- [ ] Write the abstract itself from Monday's narrative outline. Note: submission portal
      (`clec-lactwg.org/borradores/`) blocks automated fetches — word limit/format need to come
      from a manual visit or from Tremie directly if not already known.

**11:00–12:00 — Send to Tremie for her offered review**
- [ ] She explicitly offered ("I would be happy to review any submission") — send early enough
      Tuesday to get feedback back before Wednesday's deadline, not Wednesday morning.

**13:00–16:00 — Sandra presentation build**
- [ ] Adapt/translate from the existing IADB deck (`docs/presentations/
      idb_wwf_workshop1_case_study.qmd`, already Becky-reviewed) + Monday's Colombia content +
      Phase 4-style disproportionality framing where it strengthens the pitch.
- [ ] Spanish throughout — this is the in-person, Spanish-language, user-led meeting.

**16:00–17:00 — Self-review pass**
- [ ] Read both the abstract and the deck fresh; cut anything that overclaims (no causal language
      on the disproportionality findings — co-occurrence, not causation, same discipline as the
      Becky report).

## Wednesday — Final polish only (9:00–11:00, 2h)

**9:00–10:00**
- [ ] Fold in any overnight Tremie feedback; final abstract polish; **submit before the deadline**.

**10:00–11:00**
- [ ] Final deck polish; prep talking points for the colleague lunch (the practice run).

**Then**: lunch with colleague (practice run) → Sandra Valenzuela meeting.

## Open items, not blocking, worth a look if time allows

- CLEC submission format (word count, tracks) — currently unknown, portal is bot-blocked.
- Whether a road-network data layer for Colombia is worth sourcing (OSM/INVIAS) for the
  road-ecology-specific audience — bigger lift, explicitly deferred unless Monday's time allows.
- `sandra_email_draft.md` (referenced in memory as sent 2026-07-16) still not located in this
  repo — worth finding for tone/content continuity before drafting new Sandra material.
