Wed, 22 Jan 26

## Completed Tasks
- **Hotspot Intensity Analysis**:
    - Created `analysis/hotspot_intensity.qmd`.
    - Calculated % area coverage and share of global hotspots by region/biome.
    - Implemented "Top 15" filtering for countries and canonical service ordering.
- **Multi-service Analysis**:
    - Created `analysis/hotspot_multiservice.qmd`.
    - Analyzed hotspot overlap ("hotness") and identified regions with high multi-service coincidence.
- **Visualization Updates**:
    - Refactored signed change bar plots in `hotspot_extraction.qmd` (flipped axes).
    - Added `country_name` to `plt_long`.
    - Excluded non-terrestrial regions (Seven Seas, Antarctica) from summary plots.
- **KS Tests**:
    - Updated `analysis/KS_tests_hotspots.qmd` with sanity-check violin plots (balanced sampling).
    - Fixed data loading to ensure socioeconomic variables are available.
    - **Status Check**: Implemented "Median spots" sampling (5% at median change) in `analysis/KS_tests_hotspots.qmd`.
- **Violin/Box Plots**:
    - **Updated**: `R/hotspot_violins.R` now generates **box-and-whisker plots** alongside violins.
    - **Method**: Boxplots use pre-calculated stats (median, IQR) from the full dataset but plot without outliers to provide a "zoomed-in" view of the distribution body, addressing the "fat tail" visibility issue.

## Next Steps
- **Interpretation**:
    - Review `ks_results_hot_vs_non.csv` and new violin plots to validate socioeconomic findings.
    - Analyze `hotspot_area_stats.csv` and `hotspot_multiservice_stats.csv` for key geographic takeaways.
- **Documentation**:
    - Assemble "Living PowerPoint" with the new canonical figures.
- **Analysis**:
    - **KS Analysis Refinement**:
        - Current state: Compares Hotspots vs. Random sample of Non-Hotspots (`match_hot`).
        - Next step: Implement "Median Spots" comparison.
        - Logic: Identify pixels where change is "typical" (e.g., within the interquartile range or a tight window around the median change).
        - Goal: Verify if hotspots differ specifically from "business as usual" areas.
        - Action: [COMPLETED] Updated `analysis/KS_tests_hotspots.qmd` to define a `median_df` subset.
    - **Statistical Interpretation (Future)**:
        - **Objective**: Move beyond distribution shape (KS) to comparing central tendencies (Means) to facilitate narrative (e.g., "Hotspots are $X poorer").
        - **Methods**:
            - **t-tests**: For binary comparisons (Hotspot vs. Median-Spot) per service.
            - **ANOVA**: For comparing Hotspot characteristics across multiple groups (e.g., "Does the socioeconomic profile of hotspots vary significantly by Biome or Region?").
- **Admin**:
    - Mid-year evaluation.

---

Wed, 14 Jan 26
Technical Issues Resolution
Sediment Retention Ratio: [RESOLVED]
Status: successfully recalculated ratio rasters and difference maps. Zonal statistics have been extracted and validated.
Output: New plots generated; currently refining visualization (sorting/faceting) and filtering logic.
Action: Removed requirement for Becky/Big Boi run.
Data Analysis Progress & Methodology
Switched to equal area grid (from 10km grid) - fixed previous sign flipping issues
Pixel-level calculations vs spreadsheet calculations showing consistent results
Error bars minimal but present in pixel-level analysis
Nitrogen retention showing comparable values between methods
Trimming approach causing artificial distribution shapes in violin plots
Thresholding creates fat tails at both ends —low end because it's top 5% of values, high end because we're trimming at 99.9%
Need to either remove trimming and rescale axes instead of capping data values
Violin Plots: [RESOLVED] Refactored code to remove hard trimming (which caused "fat tails"). Moving to a dynamic axis-rescaling approach (using 1st/99th percentiles for view limits only).
Percent Area Calculations: [RESOLVED] Developed code to calculate the percentage of land area classified as "Hotspots" within each spatial grouping.

Hotspot Analysis Results
Violin plots completed for all variables across different groupings
Continent, income group, biome classifications
Absolute and percent change distributions
Key findings from distributions:
South America consistently stands out across multiple variables
Pollination showing universal decline (negative values globally)
Some concerning distributions (e.g., negative 200% values exceeding theoretical limits).Sediment export showing unusual bi-modal distributions
Distribution Limits Resolved: The extreme values and clustering at $\pm 200%$ are confirmed to be a feature of the Symmetric Percentage Change (SPC) metric, which is bounded between $-200%$ (Total Loss) and $+200%$ (New Emergence). The bi-modal distributions (e.g., Sediment export) are noted.
Socioeconomic Relationships (KS Test)
GDP analysis showing hotspots concentrated in lower? (or higher?)-GDP areas
If lower, that's a reversal from previous findings that caused concern
Need validation through violin plots comparison so we can see what the mean (and distributions) of GDP are in hotspots and not-hotspots
Population and built environment showing similar patterns
Sample size imbalance issue: 5% hotspots vs 95% non-hotspots
Plan to subsample non-hotspots for fair comparison
Solved. Re analysis run on a subset. [COMPLETED]
Try both random sampling and median spots (5% at median change) [TODO - Not yet implemented]
Next Steps
Priority: Create hotspot summary statistics [COMPLETED]
Percent area that is hotspot by region/biome/income group [COMPLETED]
Average number of hotspots per location [COMPLETED]
“Hotness” intensity analysis for existing hotspots [COMPLETED]
Secondary tasks: [COMPLETED]
Fix violin plot trimming issues (remove data trimming, cap axes only) [COMPLETED]
Create equal sample size KS test comparisons [COMPLETED]
Establish living PowerPoint with current figures (not static PDF)
Share sediment retention file for debugging
Schedule mid-year evaluation for next meeting
Upcoming group meeting end of month + all science meeting Jan 22nd

Detailed action items
Sediment retention bug


Jeronimo: Share the problematic sediment retention ratio file.
Becky: Run sediment model on Big Boi with Rich’s code and debug missing/-Inf/NA values.

Hotspot summary statistics (priority)


Jeronimo: Use equal-area hotspot maps to quantify:
Calculate percent area that is hotspot by region/biome/income group (and other existing groupings). [COMPLETED]
Action Taken: Developed and executed the script to calculate the spatial extent of hotspots.
Outcome: Generated summary tables quantifying the percentage of land area classified as "Hotspots" for all spatial groupings (Biomes, UN Regions, Sub-regions).
Metric: (Hotspot Area / Total Group Area) * 100


Calculate average number of hotspots per location (“hotness”) - Distribution of “number of services” per hotspot pixel (how many services each pixel is hotspot for) - [COMPLETED] Solved, charts added to the pptx
Consider simple summaries/visuals showing where hotspots are most multi-service and which regions/biomes have the most multi-service hotspots. [COMPLETED]

Socioeconomic KS tests & comparisons


Jeronimo:
Create sanity-check violin (or similar) plots comparing GDP (and other covariates) in hotspots vs non-hotspots to verify KS-test interpretation. [COMPLETED]
Address sample-size imbalance by: [COMPLETED]
Subsampling non-hotspots (random 5% to match hotspot share). [COMPLETED]
Optionally sampling around the median (5% of “typical change” pixels). [TODO]
Re-run KS tests with these balanced samples. [COMPLETED]

Hotspot distributions & violin plots


Jeronimo:
Remove percentile trimming of underlying values; instead fix axis limits (no capping data, only rescaling axes). [COMPLETED]
Re-generate violin/box plots with these fixed axes. [COMPLETED]
Test alternative visualizations if needed (e.g., box-and-whisker, zoomed-in versions). [COMPLETED - Added boxplots]

Absolute vs pixel-level aggregation check (lower priority)


Jeronimo: Later, compare results computed:
At pixel level from rasters vs
From grid-aggregated values (spreadsheet-style)
 to document how similar/different they are (likely for appendix).

Sediment retention ratio integration


Jeronimo & Becky:
Once sediment issue is fixed, integrate sediment retention ratio into same summary/figure framework as other services (risks across top, retention ratios, pollination, etc.).
Ensure consistent ordering of locations across variables.

Figure management


Jeronimo:
Create and maintain a living PowerPoint (or similar) with the current “canonical” figures for the paper (replacing scattered PDFs).
Keep it updated as figures are revised.

Admin / collaboration / evaluation


Jeronimo:
Share updated hotspot figures and summaries before next meeting.
Prepare for mid-year evaluation discussion (complete Workday reflection).
Becky:
Schedule time in next meeting for:
Reviewing hotspot area/hotness summaries.
Doing the mid-year evaluation conversation.

Integration at new host institution


Jeronimo:
Continue outreach and integration (follow up with colleagues like Carter; explore opportunities to present work and connect with grassland-related efforts).
Appointment next week wiot the coordinator of Governanze and SES at instituto humboldt

Chat with meeting transcript: https://notes.granola.ai/t/a295059a-f927-4a9e-9c22-4b8a2d595b60-00demib2
