# Seasonal Water Yield: Biophysical Table Research Notes

Status: research in progress, not started implementation. First pass 2026-07-10.

## Critical framing, clarified by user 2026-07-16 — read this before anything else below

**This is fundamentally different work from the other 8 services, not an incremental addition
to the same kind of task.** The other 8 services' InVEST model runs were never done by this
project — Becky and collaborators ran those models in earlier, separate research, and handed
over the finished output rasters "as-is." This repo's actual job (hotspot identification,
change detection, synthesis, everything documented elsewhere in this repo) is post-processing
of pre-computed outputs — it never needed the original biophysical tables, LULC master lookup,
or climate inputs, which is why none of that exists in this checkout (confirmed by direct
repo search, 2026-07-16).

**SWY has never been run before, globally, by anyone on this project — precisely because a
single global calibration isn't feasible**, which is the whole reason a subregional/stratified
approach is even under discussion. Producing an SWY output means **actually running the InVEST
SWY model from scratch, globally, for the first time** — not reprocessing an existing raster.
That requires assembling every real input (DEM, LULC at model resolution, soil hydrologic
group, monthly precip/ET0, rain events, biophysical table) and running the model, which is a
qualitatively bigger undertaking than anything done in this project so far. GCN250 (see below)
would remove the single hardest piece of that (CN), but does not remove the rest of the model
run itself. **This distinction needs to be explicit with Becky — the honest framing for Tuesday
is "here's what a new model run requires and what we've de-risked so far," not "here's a table
update."**

## Context

Priority #2 after the IDB deck (see `docs/presentations/idb_wwf_workshop1_case_study.qmd`): add InVEST
Seasonal Water Yield (SWY) as a 9th service. The blocker isn't the pipeline architecture — it's that SWY's
biophysical table needs curve numbers (CN) and monthly crop coefficients (Kc) per LULC class, and both are
known to vary regionally in ways the other 8 services' single global biophysical table doesn't have to deal
with. Becky's steer: build subregional tables and assemble them into global coverage, rather than one
universal table.

Full data requirements (DEM, LULC, soil group, precip/ET0, rain events, model params) are in the InVEST docs:
https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/seasonal_water_yield.html

## CN — Curve Number

- **Origin**: SCS/NRCS-USDA, developed for the United States. Primary reference: NRCS TR-55 (1999),
  *Urban Hydrology for Small Watersheds*. Full CN methodology in NRCS-USDA (2007) *National Engineering
  Handbook*.
- **InVEST's own guidance is explicit that there is no global shortcut**: "area-specific values are
  preferred" over applying TR-55 defaults everywhere. CN is a function of hydrologic soil group (HSG),
  cover type, treatment, hydrologic condition, and antecedent runoff condition — CN=99 for
  stream-connected water bodies/wetlands; wet-condition (ARC-III) values for flood modeling.
- **Practical implication**: CN genuinely needs literature-sourced, region-specific values. This is the
  harder of the two parameters and the one that actually justifies Becky's "subregional tables" framing.
- **SWAT checked, 2026-07-16 (see below) — no shortcut there.** SWAT's own global land cover
  databases (Abbaspour & Ashraf Vaghefi 2019) only crosswalk land cover classes to SWAT's own
  crop/landuse definitions, not curve number values. Useful negative result, not a building block —
  don't re-check this.
- Also checked: whether any published InVEST SWY case study had already assembled a multi-region
  CN table — yes, Hamel et al. 2020 (see below), by borrowing from existing calibrated regional
  studies rather than building one from scratch. This became the actual operating strategy.
- Source citations: NRCS TR-55 (https://www.nrc.gov/docs/ML1421/ML14219A437.pdf); CN tables also at
  https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/cn-tables

## Kc — Crop/Plant Coefficient (monthly, kc_1...kc_12)

Two very different sub-problems here — cropland and everything else.

### Cropland
- **Standard reference**: Allen et al. (1998), FAO Irrigation and Drainage Paper 56 — InVEST cites this
  directly. FAO's online Kc resource: http://www.fao.org/3/X0490E/x0490e0b.htm
- NatCap provides a ready-made weighted-average Kc calculator spreadsheet:
  https://github.com/natcap/invest.users-guide/raw/main/data-sources/kc_calculator.xlsx (not yet
  downloaded/reviewed).
- Kc timing (which month is green-up vs. die-down) is genuinely region/hemisphere-dependent — needs local
  crop-calendar/phenology knowledge, which is where regional variation actually bites for crops.

### Non-crop vegetation (forest, grassland, shrubland — the bulk of global land area)
- **This may not need a hand-built regional table at all.** InVEST's own docs point to an NDVI/LAI-based
  satellite method as an alternative to literature lookup for non-crop vegetation.
- Confirmed in the literature: Kamble, Kilic et al. (2013), *Remote Sensing* 5(4):1588 — "Estimating Crop
  Coefficients Using Remote Sensing-Based Vegetation Index." Validated linear regression:
  **Kc = 1.457 × NDVI − 0.1725**, r² = 0.90–0.91 against AmeriFlux eddy-covariance flux tower measurements
  (MODIS NDVI, multiple crop/vegetation types).
- Why this matters: NDVI is already globally available at consistent resolution and (unlike CN) is
  inherently spatially and temporally resolved — using it sidesteps the "assemble N regional tables"
  problem entirely for whatever fraction of global LULC this regression is valid for. **This is the
  single most promising lead so far for keeping the Kc side of this manageable at global scale.**
- Not yet checked: how well this specific regression generalizes outside the original study's vegetation
  types/climate; whether more recent/better-validated versions of this regression exist since 2013.

### Non-vegetated (open water, wetlands, bare soil, built/impervious)
- InVEST's own guidance gives typical Kc ranges directly (0.3–1.2) — this is the easy part, no research
  needed.

## Soil Hydrologic Group (separate missing input, not yet in the pipeline)

SWY needs a raster of hydrologic soil group (A/B/C/D, encoded 1-4) — this input doesn't exist anywhere in
the current pipeline (all 8 existing services are soil-group-agnostic). Candidate: **HYSOGs250m** (Ross et
al. 2018), a global 250m hydrologic soil group product. **Confirmed still live and available**
(2026-07-16, see below) via ORNL DAAC (DOI 10.3334/ORNLDAAC/1566) and NASA Earthdata — this input is
unblocked, just not yet downloaded (see the "Open questions" checklist near the end of this file).

## Proposed approach (2026-07-10 draft — superseded, kept for history only)

**Superseded 2026-07-16 by the GCN250 milestone below** — item 1's "build subregional tables from
scratch, stratified by biome/Köppen" plan was replaced by "use GCN250 as the global baseline
everywhere, patch only the ~3 biomes GCN250's own authors flag as least validated." Items 2–4
(Kc) held up and are still the actual plan. Don't build against item 1 as written — see the
"Open questions / next steps" section near the end of this file for the actual current checklist.

1. ~~**CN**: build subregional tables, stratified by some combination of biome (WWF_biome, already in this
   pipeline's grid attributes) and/or Köppen climate zone.~~
2. **Kc, cropland fraction**: FAO-56 tables, crop-calendar-adjusted by hemisphere/climate zone at minimum.
3. **Kc, non-crop vegetation fraction**: NDVI-derived via the Kamble et al. regression (or a better
   updated one, if found) — likely avoids needing a regional lookup table at all for most of the land
   area modeled.
4. **Kc, non-vegetated**: fixed literature values per InVEST's own guidance, no further research needed.

## Update 2026-07-16 — three open questions resolved

- [x] **HYSOGs250m confirmed still available and current.** Actively archived at ORNL DAAC
  (DOI: 10.3334/ORNLDAAC/1566) and mirrored on NASA Earthdata/data.nasa.gov, openly licensed,
  250m GeoTIFF, derived from SoilGrids250m texture/depth-to-bedrock. Confirmed available as of
  2024-2025, not an orphaned/stale product. **This unblocks the soil hydrologic group input —
  no longer a research question, just an acquisition/integration task.**
- [x] **Kamble et al. (2013) NDVI-Kc regression holds up.** Not a one-off: follow-up studies
  through 2020-2023 replicate the same approach across other crops/vegetation types, with some
  (e.g. a 2023 Sentinel-2 wheat study) reporting even higher fit (r² > 0.93) than the original.
  This is a genuinely active, corroborated method, not a single dated citation — **raises
  confidence in this being the right call for the non-crop-vegetation Kc fraction.**
- [x] **Checked SWAT's global land cover/crosswalk datasets (Abbaspour & Ashraf Vaghefi 2019,
  "Global Land Cover for SWAT") as a possible CN shortcut — confirmed it is NOT one.** That
  dataset only crosswalks land cover classes to SWAT's own landuse/crop definitions; it does not
  carry curve number values or an HSG-stratified CN lookup. **This is a real, useful negative
  result: it confirms Becky's original "no global shortcut, build subregional tables" instinct
  was correct** — there's no existing global CN product to borrow instead of doing the work.

## Stratification scheme — decision made given the timeline

Going with **biome-based stratification** (WWF_biome, already present in this pipeline's grid
attributes) rather than Köppen climate zones, purely for delivery reasons: biome requires no new
spatial join, Köppen would be a new input to acquire and integrate. Flagging Köppen as a
possible future refinement if biome-level CN strata prove too coarse once real values are
assembled — not deciding that now, revisit after the first table pass.

## Major update 2026-07-16 (later same day) — GCN250 changes the CN plan

Found while double-checking whether SWAT had a CN shortcut: it doesn't, but a **dedicated,
purpose-built global gridded curve number product does** — **GCN250** (same research group as
HYSOGs250m, published as a follow-on). This was not on the radar before today.

- **What it is**: global CN rasters at 250m (7.5 arc-second, WGS84), built by crosswalking ESA
  CCI-LC 2015 land cover (36 classes) into the official NRCS NEH-630 land-cover categories, then
  combined with HYSOGs250m soil groups — i.e., it's the same lookup-table logic we were about to
  build ourselves, already done, peer-reviewed, and published. Delivered as **three rasters**
  for dry/average/wet antecedent runoff conditions (ARC), not one.
- **Public and downloadable now**: Figshare, DOI 10.6084/m9.figshare.7756202, R script for
  regeneration included.
- **The honest caveat, worth flagging explicitly to Becky**: GCN250's regional differentiation
  comes from *land-cover-type granularity + wetness scenario*, not from *biome/region-specific
  values for the same land-cover type*. A given land-cover class gets the same base CN everywhere
  in the world (modulated only by which ARC scenario is selected) — closer to "one well-built
  global scheme" than to the "subregional tables" framing Becky originally asked for. Authors'
  own caution: "hydrologists should proceed with caution... and should always compare generated
  runoff with observed values whenever possible," and note the underlying CN method itself was
  built for small US agricultural watersheds, with the most uncertainty in forested/humid
  environments — exactly the tropical forest biomes most relevant to this project's LAC focus.
- **What this means for the plan**: this is a real decision point, not an obvious win. Option A —
  adopt GCN250 directly (fast: an integration task, not a table-building research project, same
  pattern as using HYSOGs250m directly). Option B — still build the originally-planned
  biome-stratified table, using GCN250 as a validation/starting reference rather than a
  replacement, if Becky's original concern about regional specificity within a land-cover class
  is judged to matter more than shipping speed. **Not deciding this alone — this is exactly the
  kind of call to bring to Tuesday's meeting**, since it trades off timeline against fidelity to
  Becky's original methodological instinct.

## Update 2026-07-16 (evening) — kc_calculator.xlsx reviewed, remaining inputs sourced

**`kc_calculator.xlsx` downloaded and reviewed.** Confirms the same regional-transferability
problem exists for Kc as for CN, and confirms why the NDVI-regression route for non-crop
vegetation is the right call. Four sheets:
- `Kc calculator` — a working template with pre-filled monthly Kc profiles for corn, forest,
  grassland, stagnant water, wetlands, urban, soy, wheat, cotton, fallow, alfalfa/hay,
  switchgrass. **These profiles are baked to a Northern Hemisphere temperate growing season**
  (bare Jan-Apr, canopy peak Jul-Sep, harvest Oct-Nov) — e.g. "forest" is hardcoded as dormant
  in winter and full-canopy in summer, which is wrong for tropical evergreen forest (no
  dormant season) and inverted for the Southern Hemisphere. Useful as a Northern-temperate-biome
  reference/starting point, not a global table.
- `FAO tables` — proper FAO-56-style parameters (Kc_ini/mid/end, stage lengths, a default
  planting date) for ~17 crops (corn, soy, wheat, rice paddy, rye, oats, sorghum, cereals,
  potatoes, sugarcane, sugarbeets, oil seed, cotton, legume, vegetable, durum wheat, etc.) and
  the formula that converts stage-Kc + planting date + stage lengths into a monthly curve. This
  is directly usable for the cropland fraction — but the built-in planting dates are single
  defaults (also Northern-Hemisphere-biased) and need to be re-run per region/hemisphere with
  locally correct planting dates, not used as-is globally.
- `references` — source list for the ET/Kc values used (FAO-56, several US extension/irrigation
  sources, wetland ET literature — e.g. Lafleur & Rouse 1988, Lafleur 1990, Lott & Hunt 2001,
  Rosenberry et al. 2004 — all Northern temperate/subarctic wetland studies, another
  regional-transferability flag for wetlands specifically).
- `crops_inches day` — a temperature-indexed corn growth-stage lookup (US Midwest, source cited
  as a NetafimUSA corn manual) — not globally relevant, skip.

**Remaining inputs — sourcing options identified:**
- **NDVI**: MOD13A3 (MODIS monthly NDVI/EVI, 1km, global, 2000-present, no use restrictions),
  via NASA LP DAAC or Google Earth Engine. Not yet downloaded/integrated.
- **Global watersheds/AOI**: HydroBASINS (part of HydroSHEDS — note, a WWF-associated product),
  15 arc-second (~500m), global, hierarchical sub-basin polygons, shapefile format, freely
  available. Good candidate for the SWY "Area of Interest" input.
- **Monthly precipitation**: CHIRPS (0.05°, ~5.5km, daily-to-monthly, 1981-present) is the
  strongest global candidate, but **CHIRPS only covers 60°N-60°S** — misses boreal/arctic
  biomes (Boreal Forests/Taiga, Tundra) entirely, a real gap for a truly global run. CHELSA
  (monthly climatology, 1979-2013, full global coverage) is a fallback for those latitudes, but
  it's a fixed climatology, not era-specific (1992 vs. 2020) — same open provenance question
  already flagged for Becky in `analysis/WORKLOG.md`. May need a blended approach (CHIRPS +
  CHELSA for high latitudes) rather than one single source.
- **Rain events table** (count of days >0.1mm rain per month): not directly available from any
  source above as a pre-computed product — would need to be *derived* from a daily precipitation
  product (CHIRPS has daily granularity) by counting threshold-exceedance days per month. A
  real derivation step, not a download.

## Update 2026-07-17 — literature review expanded, 9 to 16 references

Added 7 references to `docs/swy/literature_review.ris`, found by searching specifically for
literature on the ~3 biomes flagged for the CN patch, updated NDVI-Kc validation, and proper
citations for the newly-identified data sources (NDVI, watersheds, precipitation):

- **Calero Mosquera et al. 2021** (tropical CN evaluation, *Earth Sciences Research Journal*)
  and **Fábrega et al. 2012** (measured CN in a Panama Canal tropical rainforest microbasin) —
  both directly support the tropical-forest patch with real evidence, not just GCN250's own
  caution. Calero Mosquera et al. found the SCS-CN method genuinely overestimates runoff under
  tropical conditions specifically because initial abstraction coefficients were calibrated on
  US watersheds — direct empirical confirmation, not just a theoretical caveat.
- **Mangroves and flooded grasslands/savannas — searched specifically, found nothing usable.**
  No literature surfaced giving CN or runoff-coefficient values specific to mangrove/saturated
  wetland systems. This is a real, still-open gap in the biome-patch plan, not yet solved —
  worth flagging to Becky directly rather than assuming it'll be easy once started.
- A 2025 grapevine Sentinel-2 Kc paper — recent evidence the NDVI-Kc approach keeps validating
  in current literature, though not itself a source of new global parameters (different crop,
  author list unconfirmed — flagged in the .ris entry to verify before formal citation).
- Proper citations added for the candidate data sources identified this week: MOD13A3 (NDVI,
  Didan 2021), HydroBASINS/HydroSHEDS (Lehner & Grill 2013 — a WWF-associated product), CHIRPS
  (Funk et al. 2015), CHELSA (Karger et al. 2017).
- Two entries (Fábrega et al. 2012, the grapevine 2025 paper) have incomplete author/venue
  metadata — search couldn't fully confirm them, marked "VERIFY BEFORE FORMAL CITATION" in the
  .ris file rather than guessed at.

## Update 2026-07-17 (later) — Hamel et al. 2020 obtained and read in full

User obtained the paywalled PDF directly. Changes the framing in three ways, not just fills in
the precedent citation:

1. **How they actually built CN/Kc tables, answering the "how do we operationalize subregional
   tables" question**: neither case study derived values from literature tables cold. Peru
   reused CN straight from an existing calibrated SWAT model of that exact basin (Uribe et al.
   2013, built years earlier for a payments-for-ecosystem-services program). Myanmar reused
   CN/Kc from an existing national ecosystem-service assessment (**Mandle et al. 2017** — same
   paper set aside earlier in the IDB deck work, now relevant again for a different reason:
   possibly usable as a real parameterization source, not just a citation). **Operational
   strategy going forward: search for an existing calibrated hydrologic study or ecosystem
   assessment covering a representative watershed in each flagged biome, and borrow its
   parameterization, rather than deriving from TR-55 cover-type tables from scratch.** Tropical
   moist broadleaf forest already has candidates (Calero Mosquera et al. 2021, Fábrega et al.
   2012). Flooded grasslands/savannas: worth searching Pantanal or Llanos/Orinoquía hydrology
   literature specifically (both named WWF priority landscapes; Llanos also personally relevant
   given the user's own research background there). Mangroves: still an open gap, not yet
   searched with this specific framing.
2. **CN sensitivity is dramatic, not theoretical, per Hamel's own sensitivity analysis**:
   quickflow changed by **factors of 10.1-13.1x** between antecedent-moisture-condition CN
   settings in the Chindwin basin. A concrete, citable number for why the CN decision matters,
   not just a conceptual concern.
3. **Model reliability appears to degrade at large basin scale, and Hamel's largest basin was
   only 114,000 km².** Their own conclusion: use with caution for absolute values "in new
   geographies," and that basins **under 10,000 km²** are best supported by their validation
   data. A genuinely global run is planetary scale — orders of magnitude beyond anything
   validated in this paper or found anywhere else in this review. **This is a real, separate
   caveat worth raising with Becky alongside the CN decision** — not just a CN-table problem,
   but a question of whether "one global run" is the right framing at all versus a basin-by-basin
   or regional-mosaic approach (closer to what Hamel et al. actually did: two separate
   basin-scale applications, not one global one).

New reference to add: **Guswa et al. 2018** (*J. Hydrologic Engineering*) — the actual paper
behind InVEST-SWY's monthly CN-based quickflow method, cited directly in Hamel et al., more
specific than the general user guide.

## Routing vs. parameterization — two separable layers (clarified 2026-08-24)

Worth stating explicitly, since it clarifies what's actually still open: this problem splits into
two layers that get assembled together, not one undifferentiated "run SWY globally" task.

1. **Routing** (basin-dependent): quickflow/baseflow accumulation only makes physical sense within
   a real, DEM-coherent watershed — this is why HydroBASINS/basin-by-basin execution is the plan
   (see open item below), not a single continuous global domain.
2. **Parameterization** (basin-independent): CN is assigned per pixel from the land-cover-keyed
   lookup table (`gcn250_esa_lc_cn_table.csv`, ESA LC class + soil group → CN_A/B/C/D), with a
   narrower biome-specific correction layered on top for the ~3 flagged biomes (tropical moist
   forest, mangroves, flooded grasslands/savannas — see open item below). This assignment has
   nothing to do with which basin a pixel falls in.

Assembly = the global CN/Kc raster (base table + biome corrections) gets clipped to each basin's
extent, then each basin is routed independently using those already-correct pixel values —
TaskGraph/ecoshard being the mechanism for running that per-unit at global scale. Sent to Rich
(and Becky, same channel) 2026-08-24 for confirmation.

**Confirmed by Rich (2026-09-03, Slack, verbatim "Yes!")**: the *mechanical* framing is correct
— `run_swy_global.py` really does batch the watershed vector into per-basin jobs, route each
independently through TaskGraph, stitch into one continuous global mosaic, and the CN/Kc raster
paths really do bypass the lookup table when supplied directly. Combined with the independent
code-level confirmation from reading `swy_global` directly the same day (see below), this half
of the architecture question is closed.

**Genuinely still open, confirmed NOT asked in the message that got sent**: the message as
actually sent only asked the mechanical question above — it dropped the Hamel et al. 2020
validation-scale nuance (whether stitching many small basin runs together satisfies what Hamel's
basin-size caution would consider a defensible "global" result, as opposed to just being
mechanically possible) that was in an earlier draft. Rich's "Yes!" cannot be read as covering a
question he was never asked. This remains a real open question, not just unconfirmed — worth a
separate, explicit ask before treating "global run" as scientifically validated rather than
merely mechanically executable.

## `swy_global` repo studied (2026-09-03) — confirms the routing/parameterization split at the code level

Cloned `github.com/springinnovate/swy_global` (Rich's prior global SWY framework, pointed to
2026-08-21) and read `README.md` + `run_swy_global.py` directly — not a summary from Rich's
description, the actual code. Findings:

- **The TaskGraph/ecoshard mechanism from the section above is now confirmed, not presumed.**
  `run_swy_global.py` imports `ecoshard.taskgraph` and `ecoshard.geoprocessing`, and calls
  `inspring.seasonal_water_yield.execute()` (a NatCap "inspring" package — an extended/research
  SWY implementation, not stock InVEST) once per watershed job.
- **Basin batching is real and already solved, not something to design from scratch.**
  `_batch_into_watershed_subsets()` loops over the global watershed vector (`.shp` files under
  `WATERSHEDS_VECTOR_PATH`, a HydroSHEDS-derived product per `swy_global.ini`), groups small
  watersheds into degree-separated tiles capped at 1000 features per job (large watersheds get
  their own job), reprojects each job to its local UTM zone, and schedules them largest-first for
  parallel execution via `multiprocessing` + `TaskGraph`. Results get stitched back into one
  continuous mosaic via a queue-based worker (`N_TO_BUFFER_STITCH`), with per-job workspaces
  cleaned up once stitching confirms completion. **This resolves the "one global run vs. regional
  mosaic" framing as a false binary** — the tool does both simultaneously: real per-basin
  computation (respecting Hamel et al. 2020's basin-scale validation caution) assembled into a
  seamless global output. Worth stating this precisely in any reply to Rich, not just "seems to
  work."
- **Continuous CN/Kc rasters bypass the lookup table entirely — confirmed at the `model_args`
  level.** `run_swy_global.py` builds one `model_args` dict per job containing *both*
  `lulc_raster_path` + `biophysical_table_path` (the standard InVEST route) *and* optional
  `cn_a_path`/`cn_b_path`/`cn_c_path`/`cn_d_path`/`kc_1_path`...`kc_12_path` overrides, commented
  "these keys are optional rasters that would replace lulc biophysical parameters." This is
  exactly the integration point for this project's plan: precompute CN_A-D and Kc_1-12 rasters by
  blending the GCN250 lookup table with the biome-specific literature corrections (tropical moist
  forest, mangroves, flooded grasslands/savannas), then feed them here directly — no lookup-table
  step needed at all.
- **`calculate_average_monthly_events.py` (the rain-events calculator) uses Google Earth Engine +
  CHIRPS DAILY, not ERA5.** There's a commented-out ERA5 dataset line (`# ('ERA5', ...)`,
  disabled) sitting next to the active CHIRPS entry — meaning **the CHIRPS 60°N/60°S coverage gap
  is NOT resolved by this script as shipped**; ERA5 blending was evidently tried or planned but is
  currently inactive scaffolding, not a working fallback. This answers the open question from the
  2026-08-21 Rich outreach ("does his calculator already blend ERA5 for high latitudes") — no, it
  doesn't, currently. Re-enabling it would need real work, not just an undocumented existing
  feature to switch on.
- **`base_data/biophysical_template_PH.csv`** — a sample biophysical table (Philippines), confirms
  the repo also supports the traditional LULC+table route as a fallback/reference format, useful
  for understanding the expected table schema even though this project plans to use the direct-
  raster route instead.

**Not yet done**: haven't traced through `inspring.seasonal_water_yield.execute()` itself (that
package isn't in this cloned repo — it's a separate dependency, `pip`-installed presumably from
NatCap's own package index or a private index; would need locating separately if the actual
model internals matter, as opposed to just the orchestration layer covered here).

## Open questions / next steps (remaining)

**Updated 2026-09-03/04**: proposed to Becky, not yet confirmed — start with a real test run on
one representative Colombian Llanos basin rather than jumping to anything global. That scope
changes several items below from "must resolve first" to "doesn't apply at this scale, revisit
before going global": the precipitation-source blend question doesn't need resolving (Llanos is
well inside CHIRPS' coverage, no need for the CHELSA fallback yet), and the Hamel et al.
validation-scale question (still an open ask to Rich, see the section above) doesn't block a
single sub-10,000 km² basin either. What the Llanos scope can't dodge, and shouldn't: it *is* the
flooded-grasslands/savannas biome, so it's the concrete test case for that patch decision, not
something to defer further.

- [x] Download and review NatCap's `kc_calculator.xlsx` tool — done, see above
- [x] Confirm the routing-vs-parameterization architecture — done 2026-09-03, both at the
      `swy_global` code level and by Rich directly (see section above)
- [ ] **Mangrove / flooded-grasslands CN patch — leaning toward "structural limitation," not yet
      formally decided or communicated to Becky.** No usable literature found after three separate
      search angles; both are flood-pulse/tide-dominated systems where CN's local rainfall-runoff
      premise may just not apply (same treatment as GCN250's own "Water/wetlands" PFT row, CN≈100).
      The proposed Llanos test run is the concrete case to resolve this against, not an abstract call.
- [ ] Tropical moist broadleaf forest CN patch — this one has real candidate literature (Calero
      Mosquera et al. 2021, Fábrega et al. 2012, pending verification) and is a genuine "build the
      correction" task, unlike the item above.
- [ ] Acquire HYSOGs250m, MOD13A3 NDVI, and HydroBASINS — **scope to the proposed Llanos test
      basin first**, not a global pull, per the update above.
- [ ] Resolve the precipitation source (CHIRPS+CHELSA blend vs. single source) with Becky — only
      actually blocking once/if this moves beyond the Llanos test to anything outside CHIRPS'
      60°N/60°S coverage.
- [ ] Derive the rain-events table from daily CHIRPS via `calculate_average_monthly_events.py`
      (confirmed working, Earth Engine + CHIRPS daily) once the Llanos basin boundary is pulled.
- [ ] FAO-56 Kc tables for cropland fraction, re-run with region-correct planting dates
      (not the spreadsheet's Northern-Hemisphere defaults)
- [ ] Locate `inspring.seasonal_water_yield` (the actual model package `swy_global` depends on —
      not in the cloned repo, a separate dependency) before any real run is attempted.

## Sources consulted so far

- InVEST SWY User Guide: https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/seasonal_water_yield.html
- InVEST Appendix 1 (data sources/parameter guidance): https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/data_sources.html
- Kamble, B., Kilic, A., et al. (2013). Estimating Crop Coefficients Using Remote Sensing-Based Vegetation
  Index. *Remote Sensing*, 5(4), 1588-1602.
- Allen, R.G., et al. (1998). Crop evapotranspiration - Guidelines for computing crop water requirements.
  FAO Irrigation and Drainage Paper 56.
- NRCS-USDA (2007). National Engineering Handbook.
- NRCS TR-55 (1999). Urban Hydrology for Small Watersheds.
