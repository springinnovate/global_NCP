Strategic Feedback & Technical Manifesto: Global NCP v1.4.0
Source: Review Meeting (2026-04-30) with Steve Polasky, Becky Chaplin-Kramer, and Natalia Aristizabal.
Purpose: This document serves as a persistent context for AI code assistance to ensure all repository modifications align with the project leads' scientific requirements.  

1. Terminology & Branding (The "No Collapse" Rule)
Observation: The term "collapse" implies non-linear threshold crossings that our current two-point (1992 vs 2020) analysis does not verify.  

Mandatary Action: Audit all scripts (.R, .qmd, .py) for labels, titles, and legends using "collapse." Replace with "Multi-service Hotspots" or "Multi-service Decline".  

2. Visual Standardization & Scannability
Observation: Color ramps in boxplots and intensity charts are currently inconsistent, leading to confusion regarding directionality (Good vs. Bad).  

Mandatory Action:

Standardize all plots to: Green = Improvement/Gain and Red = Decline/Loss/Detriment.  

Remove biome-specific or income-specific color ramps in distribution plots if they conflict with the red/green logic.  

3. The Attribution Gap & Scatterplot Deep-Dive
Observation: The lack of local (10km) overlap between Land Cover Change (LCC) and Ecosystem Service (ES) loss is a key finding, not a failure. It indicates "Spatial Attribution" or "Invisible Degradation".  

Mandatory Action:

Refine Labels: Change "Unexplained" in attribution maps to "Spatial Attribution / Degradation" to reflect Becky's theory of upstream drivers or off-pixel effects.  

Scatterplot Diagnostics: Generate new scatterplots faceted by Biome. Specifically, for Pollination, ensure plots only consider pixels with a baseline agricultural presence to prevent diluted correlations.  

The Buffer Test: Keep the 10km grid but include code comments acknowledging it acts as a spatial buffer for interconnected processes.  

4. Handling Outliers & Supplemental Data
Observation: The "High income: non-OECD" group (e.g., Saudi Arabia, Greenland) significantly skews global averages and obscures the main narrative.  

Mandatory Action:

Filter this group out of Main Report visualizations.  

Direct all filtered-out data to a New Supplemental Plotting Pipeline.  

Restriction: Do not overwrite full-dataset outputs; maintain separate files for Main vs. Supplemental.  

5. Land Cover Logic Updates (Rangelands)
Observation: Transitions from Forest to Grassland/Rangeland are currently not being captured as a "Natural to Transformed" loss in the simplified binary classification.  

Mandatory Action: Update the reclassification contingency matrices in the LCC transition scripts to explicitly flag Forest-to-Grassland conversion as a detrimental habitat loss.  

6. Future "Phase 2" Integration (Pending)
Downstream Beneficiaries: Prepare the pipeline to mask hotspots and calculate downstream population impact.  

Country Reports: Prepare for an automated report-generation loop that produces a single-page summary per country for the online supplement.