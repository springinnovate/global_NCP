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

**Update 2026-09-09 — Becky meeting, real decision, supersedes the Hamel-basin-first plan below.**
Test basin is now **Borneo**, not Peru/Myanmar. Two things worth being precise about, since they
change the shape of the plan below rather than just the location:

1. **Becky is not treating the mangrove/flooded-savannas CN gap as a blocker.** The whole
   "resolve the biome patch before testing" framing in this file (and in the Indonesia-question
   thread) was this project's own caution, not a condition she imposed — she's comfortable testing
   without it resolved first.
2. **Rigorous validation-against-published-output is not the priority she cares about here either**
   ("even the validation does not bother that much"). This is a real shift from the reasoning below
   (Hamel's basins chosen specifically *because* they have published output to check against) — the
   Peru/Myanmar validation-test framing, and the Nash-Sutcliffe-efficiency comparison plan, was this
   project's own rigor standard, not hers. Keep the reasoning below for what it's worth
   scientifically, but don't present the Borneo run as still needing to clear that bar unless asked.

**Practical plan from the meeting**: get the data already in hand (CN tables, GCN250, HYSOGs250m,
HydroBASINS — see below) onto a shared Google Drive. Attempt the actual Borneo run in-house first;
Rich runs it if that doesn't work. **Meeting Friday with both Becky and Rich — be ready.**

**What this doesn't change**: the required-inputs list, the CN/Kc strategy (GCN250 + biome patches,
FAO-56 + Kamble NDVI regression), and the `swy_global`/`inspring` architecture findings below are
all still accurate and still needed for Borneo — only the target basin and the validation framing
changed, not the underlying mechanics.

**Update 2026-09-07 — test-basin question resolved (user decision), two open threads reconciled.**
This file had been carrying two different test-basin proposals in parallel without ever explicitly
choosing between them: the Llanos idea below (2026-09-03/04) and the separate Hamel-basin-
replication proposal sent to Becky 2026-09-04 (see the SWY section of `docs/HANDOFF_2026-09-07.md`
— replicate on Hamel's own Peru/Myanmar basin(s), compare against her actual published output).
**Resolved: Hamel's basin(s) first.** Reasoning: Llanos would be a real test-basin run, but there is
no existing SWY output for the Llanos to validate the result against — a coherent trial with nothing
to check it against. Hamel's basins have exactly that (her own published quickflow output), so they
validate the parametrization, not just prove the mechanism runs. Llanos stays a real, wanted next
step — it's still the concrete test case for the flooded-grasslands/savannas CN patch, and
personally motivated for the user — just sequenced after the Hamel-basin validation, not before it.

**Update 2026-09-08 — validation-test year question resolved, simply.** The test year(s) should
just be whatever period Hamel et al. 2020's own original SWY run used for the chosen basin (Peru or
Myanmar) — not 2020, not 1992, not a project convention. Match the validation target exactly, since
the point of this test is a direct comparison against her actual output. Asked Rich directly
(`docs/swy/rich_swy_status_and_asks_2026-09-08.draft.md`) for the exact year(s), alongside the
granular basin output itself. The 2020-vs-2000 decision below still applies separately, but only
once/if this moves beyond the validation test into this project's own multi-temporal SWY run.

**Same update — 1992 NDVI unavailability accepted as a real limitation, not a blocker.** MOD13A3
only covers 2000-present (see "Remaining inputs" below); MODIS didn't exist in 1992. Decided: don't
chase a cross-sensor 1992 substitute (e.g. GIMMS AVHRR) for now. Use **2020** as the anchor year
whenever NDVI is actually needed for this project's own comparative pipeline (matching the other
four services' 2020 snapshot) — separately decide later whether pulling **~2000** (the earliest
MOD13A3 year) as a second time point is worth it, once/if this moves beyond a validation test into
an actual multi-temporal SWY run. Note this doesn't apply to the Hamel-basin validation test itself,
which should use whatever period Hamel et al. 2020's own study covered, not 2020 or 1992 — check her
methods for the actual years before pulling anything for that test specifically.

**Previously (2026-09-03/04, kept for context)**: proposed to Becky, not yet confirmed — start with a real test run on
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
      **Asked Becky directly instead (2026-09-07, sent)**: whether NatCap's existing Indonesia SWY
      run touches mangroves or flooded/seasonal wetlands — would resolve this gap directly if so,
      without more searching. Not yet answered.
- [ ] Tropical moist broadleaf forest CN patch — this one has real candidate literature (Calero
      Mosquera et al. 2021, Fábrega et al. 2012, pending verification) and is a genuine "build the
      correction" task, unlike the item above.
- [x] **HYSOGs250m acquired — 2026-09-09.** Downloaded the full global, already-reclassified
      Cloud-Optimized GeoTIFF (`HYSOGs250m_Soil_Groups_reclassified.tif`, ~388MB) from NatCap's own
      Data Hub (`data.naturalcapitalalliance.stanford.edu`) rather than the raw ORNL DAAC source
      (DOI 10.3334/ORNLDAAC/1566) — same underlying data (Ross et al. 2018), but pre-formatted for
      InVEST use and no Earthdata login required. Pulled the **global** file, not basin-clipped —
      a one-time global download is simpler than re-fetching per basin; clip locally per-basin as
      needed once a basin is chosen. Saved to `data/raw/soil_hydrologic_group/`.
- [x] **HydroBASINS acquired — 2026-09-09.** Downloaded South America and Asia (all 12
      Pfafstetter levels each) directly from `hydrosheds.org` — the two continents covering every
      candidate basin actually discussed (Peru, Llanos in South America; Myanmar, Indonesia in
      Asia), not a full global pull. Saved to `data/raw/hydrobasins/hybas_sa/` and `hybas_as/`.
- [ ] Acquire MOD13A3 NDVI — **scope to the chosen test basin first**, not a global pull.
- [ ] Resolve the precipitation source (CHIRPS+CHELSA blend vs. single source) with Becky — only
      actually blocking once/if this moves beyond the Llanos test to anything outside CHIRPS'
      60°N/60°S coverage.
- [ ] Derive the rain-events table from daily CHIRPS via `calculate_average_monthly_events.py`
      (confirmed working, Earth Engine + CHIRPS daily) once the Llanos basin boundary is pulled.
- [ ] FAO-56 Kc tables for cropland fraction, re-run with region-correct planting dates
      (not the spreadsheet's Northern-Hemisphere defaults)
- [x] **Locate `inspring.seasonal_water_yield` — done 2026-09-08.** Public GitHub repo,
      `github.com/springinnovate/inspring` (the `Dockerfile`/`setup.py`'s `therealspring/inspring`
      URL is just Rich's old GitHub account name — same org, redirects, not a fork). No PyPI
      package, no NatCap credentials needed. **No maintained install manifest** — the repo's own
      `requirements.txt` was deleted in 2022 and never replaced; the `Dockerfile` is stale (pins
      InVEST 3.9.0, a specific old `ecoshard` fork commit, even a vestigial `torch` dependency).
      Real install path: `git clone` + `pip install .` (ships compiled Cython extensions —
      `seasonal_water_yield_core.pyx` is where the actual routing math lives — needs a C++
      compiler, `cython`, `numpy`, `setuptools_scm` at build time). One thing worth asking Rich
      directly: the Dockerfile pins a specific fork commit of `ecoshard`
      (`therealspring/ecoshard@b9b4580...`), not the stock PyPI `ecoshard` (currently 0.7.0) —
      unconfirmed whether PyPI's `routing` submodule API (`fill_pits`, `flow_dir_mfd`,
      `flow_accumulation_mfd`, `extract_streams_mfd`, `detect_lowest_drain_and_sink`) matches, or
      whether his fork is still required.

## `inspring.seasonal_water_yield.execute()` — full required-inputs list (2026-09-08, code-read)

Several of these were not previously compiled anywhere in this file:

- Always: `workspace_dir`, `dem_raster_path`, `aoi_path`, **`threshold_flow_accumulation`** (stream
  extraction threshold — was in `swy_global.ini`, never listed here), **`alpha_m`, `beta_i`,
  `gamma`** (the three SWY routing-partition parameters), `lulc_raster_path` (conditionally —
  only needed if any biophysical factor isn't directly raster-overridden).
- Unless `user_defined_local_recharge=True`: `et0_dir` (monthly reference-ET rasters, globbed +
  **alphabetically sorted** — filenames must sort Jan→Dec or months get silently misassigned;
  PET is computed inside the model as `PET_m = Kc_m × ET0_m`), `precip_dir` (same sorting
  convention), `soil_group_path` (HSG raster, 1–4).
- Rain events: exactly one of `rain_events_table_path` (CSV), `climate_zone_table_path` +
  `climate_zone_raster_path`, or `user_defined_rain_events_dir` — `swy_global` uses the last.
- Biophysical: `biophysical_table_path` (CSV — code requires a `root_depth` column too, not just
  `lucode`/`CN_A-D`/`Kc_1-12` as the docstring implies) OR direct raster overrides per factor
  (`root_depth_path`, `cn_a/b/c/d_path`, `kc_1...12_path`).
- Also real, previously unlisted: `max_pixel_fill_count` (caps pit-fill flood extent),
  `single_outlet` (forces one lowest-drain/sink pixel as sole outlet — `run_swy_global.py` sets
  this automatically for single-watershed jobs).

**Real finding: `root_depth` is required to satisfy the pipeline but is dead code in this fork** —
traced through `seasonal_water_yield.py` and the `.pyx` core: it's computed but never consumed in
the actual water-balance equations (not passed into `calculate_local_recharge`, not referenced in
quickflow/baseflow routing). Stock InVEST SWY uses root depth for AET; this variant doesn't appear
to have it wired in yet. Practical implication: still need *some* root-depth input to avoid an
error, but it's low-priority to source carefully — its values currently don't affect output.

**CN/Kc resolution/alignment question, resolved precisely — this was a real open question, now
answered**: **no, CN/Kc/root_depth override rasters do NOT need to be pre-aligned to the job's
grid.** `inspring`'s own `_reclassify_or_clip()` automatically warps any supplied override onto
the DEM's aligned grid via bilinear resampling — unconditional, regardless of the `prealigned`
flag. (Worth knowing: bilinear on curve number, a bounded 0–100 quantity, will blur values across
LULC-class/soil-group boundaries — not nearest-neighbor, which might be expected for a categorical-
origin variable.) Separately, `run_swy_global.py`'s per-basin worker always sets
`prealigned=True` and does its own complete pre-warp of every input to the watershed job's local
CRS before calling `execute()` — so in the actual `swy_global` pipeline, `inspring`'s internal
CN/Kc warp is a redundant-but-harmless second pass; it's the load-bearing mechanism only if
`inspring.seasonal_water_yield.execute()` is ever called directly, without the `swy_global`
wrapper.

**Guswa et al. 2018 confirmed at the code level, not just by citation-inference**:
`_calculate_monthly_quick_flow()` implements the exact closed-form stochastic-rainfall quickflow
expression (`Si = 1000/CN − 10`, mean rain depth per event, `scipy.special.expn(1, ...)` for the
exponential-integral E₁ term) — Guswa's analytical solution under exponentially-distributed daily
rainfall, matching InVEST User Guide Eq. [1].

Clone paths (session-scoped scratch, will not persist — cheap to re-clone from
`github.com/springinnovate/{swy_global,inspring}` if gone):
`.../scratchpad/swy_research/swy_global` and `.../scratchpad/swy_research/inspring`. Files worth
returning to directly: `inspring/src/inspring/seasonal_water_yield/seasonal_water_yield.py`
(`execute()`/`_execute()`, `_reclassify_or_clip`), `seasonal_water_yield_core.pyx` (the Cython
`calculate_local_recharge`/`route_baseflow_sum` MFD-routing kernels), `swy_global/run_swy_global.py`
(~line 640-805 for the per-basin pre-warp worker, ~873-912 for `model_args` construction).

## Sources consulted so far

- InVEST SWY User Guide: https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/seasonal_water_yield.html
- InVEST Appendix 1 (data sources/parameter guidance): https://storage.googleapis.com/releases.naturalcapitalproject.org/invest-userguide/latest/en/data_sources.html
- Kamble, B., Kilic, A., et al. (2013). Estimating Crop Coefficients Using Remote Sensing-Based Vegetation
  Index. *Remote Sensing*, 5(4), 1588-1602.
- Allen, R.G., et al. (1998). Crop evapotranspiration - Guidelines for computing crop water requirements.
  FAO Irrigation and Drainage Paper 56.
- NRCS-USDA (2007). National Engineering Handbook.
- NRCS TR-55 (1999). Urban Hydrology for Small Watersheds.

## 2026-09-09 — Borneo pivot, decided with Becky

Real pivot at the 13:30 call: test basin is **Borneo**, not Peru/Myanmar (the original
Hamel-basin validation plan) or the Colombian Llanos. Becky isn't treating the mangrove/
flooded-savannas CN gap or rigorous validation-against-a-published-result as blockers for this
round — both were this project's own rigor standard, not a condition she set. Meeting with Becky
and Rich together set for Friday 2026-09-12. The stricter Hamel-basin validation framing is kept
as a reference, explicitly not abandoned, for a later round — see `swy_methods.qmd`'s "Test
design" section.

## 2026-09-10 — full raw-input acquisition, a real architecture correction, and documentation restructuring

**Every raw input SWY needs is now downloaded and content-verified** (not just "request says
done" — actually opened and sanity-checked with `rasterio`/`geopandas`), closing out what had been
the last real acquisition gaps:

- **DEM**: SRTMGL3 (`SRTMGL3_NC.003`, 90m, via AppEEARS) — the earlier call to not source a DEM
  independently was about not duplicating Rich's *global* one; it doesn't hold for an in-house
  Borneo-only attempt, which needs its own AOI-scoped file. Verified: range -91–4041m against Mt.
  Kinabalu's real 4095m high point — a close, correct match. 149MB.
- **Precipitation, 2020**: CHIRPS v2.0, `data.chc.ucsb.edu`, no auth, global monthly GeoTIFFs
  (~14.5MB/month) clipped locally with the project's own geopandas/rasterio (CHIRPS isn't
  NASA-distributed, not reachable via AppEEARS). Verified: July mean 138.9mm, plausible for
  equatorial Borneo.
- **Reference ET0, 2020**: **TerraClimate `pet`** — a genuinely new source, not previously
  vetted anywhere in this project before today, needed because CHIRPS is precipitation-only.
  "Reference Evapotranspiration," ASCE Penman-Monteith corrected for CO2, ~4km
  (`thredds.northwestknowledge.net`, no auth, one global netCDF per year). Verified: monthly means
  97–118mm, plausible for tropical reference ET. Citation added: @abatzoglou2018 (Crossref-verified
  DOI `10.1038/sdata.2017.191`).
- **NDVI**: MOD13A3.061, both a 2020-only task and the full 2000–2026 record submitted and
  verified (636 GeoTIFFs for the full record). The full record is kept locally
  (`data/mod13a3_borneo_full_record/`) but deliberately **not** shipped in the shared Drive
  package — package scope is one test year, to stay manageable; 2020 was chosen to match the
  Copernicus C3S LULC anchor year and as a reasonable guess (unconfirmed) for the year the other
  8 pre-computed services actually used.

**A real correction to the CN/Kc plan**, found by re-reading this file's own 2026-09-08 code-read
entry above more carefully: `inspring.seasonal_water_yield.execute()` accepts CN and Kc as
**direct raster overrides** (`cn_a/b/c/d_path`, `kc_1...12_path`, `root_depth_path`), not only via
a lucode-indexed biophysical CSV — and `lulc_raster_path` itself becomes unnecessary once every
factor is raster-overridden. Since GCN250 and the Kamble NDVI regression are both inherently
per-pixel, the plan is now to build CN/Kc as rasters and feed them through that path directly,
rather than force either through a shared lucode scheme. This shrinks the earlier "no lucode
master table exists" gap substantially — only a small 3-way land-cover mask
(crop/non-crop-veg/non-vegetated) is actually needed for the Kc split, not a pipeline-wide table.
Worth being honest about what is and isn't novel here (a question the user raised directly):
continuous, gridded parameterization is the default for most physically-based distributed
hydrology models generally; it's InVEST's table-based convention that's the special case, suited
to its usual land-use-scenario comparison use case. Whether this raster-override path is a
pattern used elsewhere or bespoke engineering Rich built for his own global-run ambitions is an
open question, not yet asked of him directly.

**Real gotchas hit today, worth keeping**:
- AppEEARS's bundle-file download endpoint (`/api/bundle/{task_id}/{file_id}`) returns an HTTP
  redirect to a pre-signed S3 URL — `curl` needs `-L` or it silently saves the tiny HTML redirect
  page instead of the real file (a uniform ~3.3KB file size across "downloaded" files is the
  giveaway).
- Calling `curl` from *inside* a Python `subprocess` picked up a different, Windows-native
  `curl.exe` that couldn't read `.netrc` (return code 26) — had to call Git-Bash's `curl` directly
  from bash, not wrapped through Python subprocess.
- `Rscript`/`sf`/GDAL segfaults reading `borneo_aoi.gpkg` in this environment (same underlying
  crash noted in `project_phase4_status.md` memory's Session 18 entry) — used the project's
  Python `.venv` (`geopandas`) instead, which worked cleanly.
- One transient run where every downloaded filename picked up a stray trailing underscore
  (`....tif_`) — did not reproduce on retry with the identical script; root cause not identified,
  not currently a live issue.

**Documentation restructured today**, after the user flagged real fragmentation risk (three
different places were tracking "current status" in overlapping ways):
- `docs/swy/workflow.md` (new) — a mermaid pipeline diagram with live status coloring, now the
  one place "current status" should be tracked. Other docs point to it rather than duplicating it.
- `docs/swy/model_specification.md` archived to
  `docs/archive/model_specification_2026-07-16.md` (not deleted) and replaced by
  **`docs/swy/swy_methods.qmd`** — a permanent conceptual/methods reference, deliberately not tied
  to any single meeting's framing (unlike the old file, which was explicitly written "for Becky,
  Tuesday" and unlike `docs/reports/swy_status_report.qmd`, which stays a disposable, meeting-tied
  status memo, regenerated fresh rather than kept permanently current). Includes real typeset
  formulas (the Kamble Kc regression, $ET_c = K_c \times ET_0$), the raster-override architecture
  note above, and an open-questions section.
- **`docs/swy/references.bib`** (new) — 22 entries, generated from `literature_review.ris` via
  `pandoc -f ris -t biblatex` (confirmed: Quarto/pandoc can read `.ris` bibliographies directly,
  no conversion is strictly required — but RIS's auto-generated citation keys are unwieldy
  multi-author strings, e.g. `Hamel_Valencia_Schmitt_Shrestha_Piman_Sharp_Francesconi_Guswa_2020`,
  and two USDA entries collided on `United_*` — hand-cleaned to short keys like `hamel2020` after
  conversion). Two new sources added and Crossref-verified: `abatzoglou2018` (TerraClimate) and
  `farr2007` (the SRTM mission paper — was previously used in this project without ever being
  formally cited). Keep `.ris` and `.bib` in sync going forward: `.ris` is the reference-manager
  master, `.bib` is what Quarto actually cites from.

**The shared Drive package** (`data/swy_shared_package/`) grew from 892MB to ~2.1GB (adding
DEM/NDVI/precip/ET0) then back down to **~1.1GB** after deliberately trimming NDVI to 2020-only.
README.md fully rewritten with the complete data dictionary. **Uploaded to Drive and Becky
notified, 2026-09-11** (user's own action, confirmed done).

**Not yet done**: the CN/Kc rasters themselves (masking + regression + merge logic — all inputs
are in hand, this is implementation work now, not acquisition), the rain-events derivation
(script exists, CHIRPS is in hand), and the actual `inspring` model call.

## 2026-09-10/11 — first real run attempt: local LULC turned out corrupted, Docker path found, run in progress

**Real data-integrity finding**: `data/raw/LandCovers/landcover_gl_1992.tif`, an input this
project's other pipeline apparently depends on, turned out to be an **empty/corrupted stub** —
zero valid pixels found anywhere on Earth (checked Borneo, the Amazon, Colombia, the Congo — all
NaN), despite having correct-looking metadata (CRS, bounds, dtype). The user had removed the
real data for storage reasons at some point. Real gotcha worth generalizing: verify raster
*content*, not just file existence/metadata, before trusting an "this input already exists"
assumption. The real 2020 Copernicus C3S land cover (the year actually needed here, not 1992)
was located on the user's WWF OneDrive as a 2.3GB netCDF and extracted directly via a windowed
read (no need to copy the full global file) — see `Python_scripts/swy_borneo_run/06_extract_lulc_2020.py`.
Real validation: 48.1% valid fraction (matches every other Borneo raster exactly), and a
physically sensible class breakdown (58.3% broadleaf evergreen forest, real mangrove/swamp-forest
and oil-palm-consistent cropland fractions) — the C3S data and the clip are both correct.

**GCN250 gap found**: only the derived CN lookup CSVs were ever downloaded, never the actual
GCN250 per-pixel raster — irrelevant for this run (the CSV path was used, see below) but a real
gap if the raster-override path is attempted later.

**Docker/`inspring` investigation — tried Rich's own path first, per explicit user instruction,
before falling back.** Concrete, reproducible findings (not vague "it didn't work"):
1. `inspring`'s own `Dockerfile` fails immediately and reproducibly on `pip3 install -r
   requirements.txt` — that file was deleted from the repo in 2022 and never replaced. Confirmed
   by actually running the build.
2. `inspring`'s `setup.py` omits `inspring.seasonal_water_yield` from its `packages` list — a
   normal `pip install .` compiles the Cython extension but leaves the Python module itself
   unimportable. **Worked around**, not fixed upstream: `python setup.py build_ext --inplace` +
   `PYTHONPATH` pointed directly at the source tree.
3. **Good news**: the *current* upstream `ecoshard` (not the old pinned
   `therealspring/ecoshard@b9b4580` fork commit) has a working `ecoshard.geoprocessing` (with
   `routing`) and `ecoshard.taskgraph` — resolves the open question flagged 2026-09-08 about
   whether the old fork was still required. It isn't.
4. This project's *own* existing Docker image (`therealspring/global_ncp-computational-
   environment`, built from the repo's own `Dockerfile`/`environment.yml`) already had everything
   `inspring` actually needs (gcc/g++/cython/GDAL, a fresh unpinned `ecoshard`) — building
   `inspring` on top of that image (rather than debugging `inspring`'s own broken one) is what
   actually worked. `natcap.invest` itself was also tried directly via pip as a possible fallback
   and failed for an unrelated, more fundamental reason: **GDAL has no PyPI wheels on any
   platform** (confirmed on both native Windows and a plain Linux container) — this is a
   well-known GDAL-maintainer decision, not an environment bug. `conda-forge` (via
   `condaforge/miniforge3`) installs `natcap.invest` + `gdal` cleanly with no compilation, and
   would have been the fallback path if the `inspring`-on-existing-image approach hadn't worked.
5. Items 1 and 2 are small, well-scoped, non-scientific packaging fixes — real candidates for a
   PR back to `springinnovate/inspring`, not attempted yet (would need an actual GitHub fork,
   deferred until after the immediate Borneo test).

**Whole pipeline consolidated into real scripts**, not just interactive commands — see
`Python_scripts/swy_borneo_run/README.md` for the full pipeline table and known compromises in
this specific run (placeholder rain-events table, NDVI-regression Kc applied uniformly including
cropland, placeholder `root_depth`, CSV biophysical-table path rather than raster-overrides).

**Run submitted, outcome not yet known as of this entry** — check `docker ps`/the run's actual
output before assuming either success or failure.

## 2026-09-11 — the run actually finished (crash was infrastructure, not the model): real, mostly plausible results, one genuine diagnosable issue

**What actually happened**: the laptop went to sleep mid-run, which killed Docker Desktop's
backend (`error waiting for container: unexpected EOF`), which the harness reported as the run
"failing" (exit code 4). That's a real, legitimate confusion to have from the task notification
alone — but checking the actual workspace directory tells a very different story: **every
standard SWY output file exists and is non-empty**, including the 12 monthly `qf_*` rasters, AET,
CN, `L`/`L_avail`/`L_sum`/`L_sum_avail`, `Vri`, and — critically — the final
`aggregated_results_swy_borneo_2020_test.shp` with a real, non-null computed baseflow value
(`qb = 1705.005`, `vri_sum = 0.998`). Aggregation is one of the model's last computational steps,
so this strongly indicates the actual scientific computation completed; what died afterward was
some post-computation Python/taskgraph cleanup step, hit right as Docker's Linux backend
disappeared out from under it (the traceback that actually landed in the log is 100% thread-join/
deprecation-warning noise from an unclean interpreter shutdown, not a computation error).

**Real content check, not just "files exist"**:
- **QF (annual quickflow)**: clean stats (excluding nodata and 14 stray fill-value pixels out of
  182M, a negligible 0.00% artifact) — mean 406.8mm/year, range 2.3–4837.2mm. Plausible for
  tropical rainforest, where most precipitation infiltrates rather than running off quickly.
- **AET (actual evapotranspiration)**: mean 1288.0mm/year, range up to 1566mm. Squarely in the
  expected range for warm, humid, year-round tropical conditions.
- **L_sum (local recharge, accumulated downstream)**: a real, genuine anomaly — while the median
  (4646.87) and most of the distribution look reasonable, ~4.6% of valid pixels (1.76M of 38.3M)
  have runaway values into the millions/billions (p99.9 = 812M, max = 10.86B). This is the
  classic signature of a flow-accumulation blow-up — almost certainly an unresolved DEM
  sink/pit-fill issue or a flow-routing anomaly interacting with the placeholder rain-events
  table, not random file corruption (the bulk of the distribution is fine; this is localized).
  **Worth investigating before trusting L_sum-derived outputs (including the aggregated `qb`)
  quantitatively** — but doesn't change the headline finding that the pipeline runs end-to-end
  and QF/AET look right.

**Bottom line for tomorrow's meeting**: the actual answer to "does it run, and is our
parametrization plausible" is a real yes on both counts, with one specific, honestly-flagged
follow-up item (the L_sum anomaly) rather than a clean, unqualified success — which is a more
credible thing to bring to Becky and Rich than either "it's perfect" or "it crashed."

**Also worth logging**: `data/swy_borneo_workspace/` (gitignored) has the full output set if
anyone wants to inspect it directly — not copied into the shared Drive package (output, not
input; the package is scoped to inputs per its own README).

## 2026-09-11 (later) — rerun completed cleanly; the L_sum anomaly is real, not a crash artifact

Fixed the actual sleep cause before rerunning: `powercfg` showed AC sleep was already disabled,
but DC (battery) sleep was still on a 1-hour timer — almost certainly what caught the first run,
since it ran close to an hour before dying. Set `standby-timeout-dc 0` and, as a second layer,
started a `SetThreadExecutionState`-based active keep-awake process for the rerun's duration (the
same mechanism video players use — works under most managed-device policies since blocking it
would break legitimate business software). Moved the incomplete first workspace aside
(`data/swy_borneo_workspace_INCOMPLETE_run1_2026-09-10/`, preserved not deleted) before
rerunning, since `taskgraph`'s file-existence-based caching could otherwise have silently reused
the incomplete `L_sum` instead of genuinely recomputing it.

**The rerun completed cleanly (exit code 0, no crash).** Real, decisive results:

- **QF and AET are byte-identical to the first run** (mean 406.8mm/yr and 1288.0mm/yr
  respectively) — expected, since neither depends on the routing step that got interrupted before.
- **`L_sum` now has full spatial coverage** — 48.2% valid fraction matching the stream/quickflow
  rasters exactly, full latitude range (was cut off at the equator before). Confirms the crash
  really was the cause of the earlier coverage gap.
- **The flow-accumulation anomaly persists at the same order of magnitude with full coverage**:
  5.36% of valid pixels > 1,000,000 (vs. 4.58% in the incomplete run) — this is the real finding.
  It is **not** an artifact of the interrupted run; it reproduced independently once there was
  full coverage to check it against. Visually (via the interactive map — genuinely useful here,
  not just presentational), the clearest concentration sits at one specific river-mouth/estuary
  location on the island's east coast, not random scatter — consistent with a real hypothesis
  (SRTM's known difficulty resolving flat, tidally-influenced coastal terrain), not confirmed.
- **The aggregated `qb` (baseflow) value came out numerically identical between the incomplete
  and complete runs**: 1705.005127 both times. This is genuinely informative: it suggests the
  AOI-wide aggregate was already robust to the coverage gap (plausibly because the outlet-level
  routing calculation was already complete in-memory before the crash, even though the full
  raster hadn't finished flushing to disk) — worth being precise that this doesn't mean the
  *pixel-level* L_sum anomaly is resolved, just that the one aggregate number tested didn't
  visibly depend on it.

Status report (`docs/reports/swy_status_report.qmd`), its embedded interactive map, and the
shared Drive package have all been updated and re-rendered to reflect the complete run — the
version uploaded to Drive last night reflects the *incomplete* run's framing and should be
re-uploaded.
