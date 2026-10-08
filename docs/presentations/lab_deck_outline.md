# Lab deck outline (lab meeting Tue 2026-10-13, 11:00 Bogotá)

About 30 minutes plus discussion. Built from the paper as sent 2026-10-05 and the 10-06 revision.
Audience: NatCap lab (modellers who know InVEST and the 2019 Science paper). Register: explore the
findings and open them for discussion, not only describe them. Every figure is the paper's own file.
Replaces the current `presentation.qmd` structure (four questions incl. WHY/land cover).

## Opening (~3 min)

1. **Title.**
2. **Three questions.** Services are declining globally; the open questions are where decline
   concentrates and who is exposed. WHAT changed (1992-2020), WHERE it concentrates, WHO is exposed.
   Continues Chaplin-Kramer et al. 2019 from a snapshot to change.
3. **Approach on one slide.** Five InVEST services, 1992 vs 2020; 300 m pixels summed to regions,
   biomes and income groups (Path A); 1.3 M 10 km cells for hotspots (Path B); SPC; hotspot = top 5%
   adverse change per service. Workflow diagram from the paper.

## WHAT: change depends on how you measure it (~6 min)

4. **Global trends and Figure 1 (biome).** N export +1.6%, sediment +1.4%, nature access −8.7%,
   pollination +6.9%, coastal risk ~0. Explore: the leading biome changes with the measure
   (sediment: Boreal relative vs Tropical moist absolute).
5. **Absolute vs relative maps.** Amazon sediment: modest in absolute terms, the largest signal in
   SPC. Explore: what proportional loss rewards and penalizes; why it is the hotspot basis.
6. **A "gain" that signals conversion.** Pollination is realized pollination on agriculture. Flooded
   Grasslands & Savannas has the steepest nature-access loss and the largest pollination gain:
   cropland expanding next to remaining habitat. Explore: favourable-direction change is not always
   good news.
7. **Coastal risk at the shoreline.** Zoom figure (New York, Nile delta, Istanbul, Manila; Pearl
   River, Gulf of Kutch). Small changes, spread on most coasts.

## WHERE: frequency and severity (~7 min)

8. **Hotspot map.** 189,932 cells; clustering in Latin America & Caribbean and East Asia & Pacific.
9. **Frequency vs magnitude by biome (new 10-06).** Tropical moist broadleaf: most frequent and
   most severe sediment hotspots. Deserts: rare nature-access hotspots (0.5) but the most severe,
   42% total loss. Explore: two different prioritization logics.
10. **Coastal risk.** Low-income coasts and South Asia: more hotspots and larger increases; Sub-Saharan
    Africa over-represented but not more severe.
11. **Sensitivity: SPC vs absolute hotspots.** Overlap 32-97% by service. Explore: the two criteria
    select different places because they answer different questions.

## WHO: inequality, not just poverty (~8 min)

12. **Income groups.** Hotspots ~2.4x as frequent per unit area in lower-middle-income countries;
    nature-access losses deeper there (median −109% vs −62%; 22% vs 8% total loss).
13. **Inequality distinguishes the services.** Population and GDP are higher in all hotspots (Cliff's
    δ +0.52 to +0.64) so they do not separate services; Gini does (+0.22 to +0.30 for the four land
    services); HDI adds little. Coastal risk is the exception (higher HDI). Explore: why inequality
    and not development level? Candidate mechanisms (frontier expansion, land tenure, governance),
    none tested. This is where the discussion should go.
14. **Exposure reaches beyond the hotspots.** 14.6% of land, ~7.6 B connected beneficiaries;
    connected beneficiaries outnumber residents ~2.5:1 at every overlap level. Explore: exposure is
    not impact; what a 50 km downstream / 1 h travel link means. (Uses the BC12 bar charts if done.)
15. *(Only if the phase 4 rerun is done)* **Beneficiary areas and inequality.** Gini skew grows at
    4+ services. Marked preliminary.

## What the analysis cannot say yet (~4 min)

16. **Land cover does not explain hotspots cell by cell.** 63% of hotspot cells have no intense
    conversion; the null model shows part of the overlap is regional co-location. Why: flow paths
    and travel distances. Next: distance-decay test; reruns holding land cover or climate fixed.
    Ask the lab how they would attribute change.
17. **Answers depend on metric, baseline, grouping and scale.** The framework makes these choices
    explicit; reusable for regional analyses.

## Discussion (~5 min + Q&A)

18. **Questions for the lab** (draft):
    - Is proportional loss the right lens for prioritization, or should absolute change lead?
    - What mechanisms could link inequality to ES decline, and how could we test them?
    - How should change be attributed when models propagate it along flow paths?
    - Which regional application would be most useful next?

## Appendix

Data sources and model notes; KS heatmap; regional breakdowns (World Bank region); hotspot metric
sensitivity table; ratios figure; workflow details.

## Build notes

- Reuse the paper's figure files only; drop images from book render folders.
- Numbers from the paper qmd (verify against it, not against the old deck).
- Footer: keep "DRAFT, not for distribution".
- Render and screenshot each slide before calling it done.
