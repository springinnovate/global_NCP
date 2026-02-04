# Mon 03 Feb 2026

**Status Update: Hotspot Intensity & Multi-service Analysis**

*   **Hotspot Intensity (Area Correction)**: [COMPLETED]
    *   Refactored `analysis/hotspot_intensity.qmd` to calculate hotspot coverage against the **total spatial unit area** (canonical grid), ensuring regions with sparse data (e.g., Tundra) are accurately represented.
    *   Implemented **Enrichment** metric (Observed Share / Expected Share) to highlight regions with disproportionate hotspot concentrations.
*   **Multi-service Analysis**: [COMPLETED]
    *   Finalized `analysis/hotspot_multiservice.qmd` (fixed setup/paths).
    *   Standardized visualization: Plots are now ordered alphabetically for consistent comparison across metrics.
*   **Documentation**: Updated `README.md` and notebook overviews to reflect the new metrics and workflow.

# Tues 27 Jan 2026


Per hectare instead of per-pixel results for all but ratio metrics (they're already standardized and will lose meaning if divided by area)/ extracting the per area summaries,
Calculate coastal risk reduction ratio in new spatially joined gpkg and rerun pipeline on vector not raster for coastal metrics. It is still not working. As is, the pipeline only accepts the input data as raster. I already asked Rich for help, raised the issue in github, waiting for him to reach back to me.
GDP comparison analysis (hotspot vs non-hotspot average values)
Recalculated hotspot percentages using total zone areas, not just valid data extent [corrected, added  enrichment calculation]
Revised KS test results !

# Thu, 22 Jan 2026

Backup evaluation process using Slack-archived goals from biodiversity team chat
FY26 goals tab contains: lead author global time series paper, regional liaison Columbia LATAM, pipeline automation
Self-reflection questions: 2 things going well/proud of, 2 areas needing improvement
Timeline: Complete by next week for January wrap-up discussion

# Pixel-Level Ecosystem Service Analysis
Technical pipeline improvements and data quality
Absolute change analysis showing 0.5-1 kg per pixel changes using corrected pipeline
Previous week’s artifacts completely resolved through full raster rebuild
Ratios, differences, and calculations now running without errors or anomalous values
Pipeline successfully handling both raster and vector data processing
Critical methodology concerns requiring immediate attention
Current per-pixel reporting problematic due to variable grid sizes globally
Pixels range from ~300m at equator to much smaller at high latitudes
Boreal/taiga regions showing disproportionate impacts due to larger cell sizes
Solution: Convert all metrics to per-hectare using pixel area raster
Becky has existing area map showing hectares per pixel
Formula: service value ÷ pixel area = service per hectare
Essential for meaningful cross-regional comparisons
Comparative analysis findings (10km grid vs pixel level)
Substantial differences in nitrogen retention ratios across scales
Pollination results more consistent: rock/ice and tundra showing negative values both scales
Sign flips persisting at 10km aggregation level, requiring pixel-level verification
Mangroves showing negative nitrogen export at pixel level vs neutral at 10km
Sediment patterns more similar between scales
Coastal risk data processing challenges
Vector processing preferred over raster conversion for coastal risk reduction ratio
Existing gpkg may contain required vectors with proper spatial joining (previous FID alignment issues resolved in new vector file)
Risk and service data available, but risk reduction ratio may need to be calculated in that new gpkg
Data Analysis Results and Visualization Strategy
Statistical methodology for extreme values
Implemented symmetric percentage change method addressing division-by-zero issues
Range capped at -200% to +200% instead of traditional -100% to +100%
-200% represents complete service disappearance (zero endpoint)
Method specifically chosen to handle infinite values and NAs in temporal comparisons
Literature-validated approach for ecosystem service change analysis
Visualization effectiveness comparison
Box-and-whisker plots superior to violin plots for pattern recognition
Violin plot tails dominated by extreme values, obscuring central tendencies
Box plots clearly highlight range differences and outlier patterns
Recommendation: Use box-and-whisker for main paper, violin plots for supplement
Hotspot analysis findings and data quality concerns
Biome-level distribution vs intensity showing distinct patterns
Critical data quality issues requiring correction:
Coastal risk showing nearly 100% hotspot coverage (clearly erroneous)
Tundra pollination reporting 30% hotspot rate (implausible given limited agriculture above 60°N)
Current calculations using only valid data areas, not total biome extent
Methodology fix needed: Calculate hotspot percentages against total zone area
Include areas with no service provision as non-hotspots
Particularly important for pollination service limited to agricultural areas
Will provide more realistic and interpretable regional comparisons
Supplementary analyses completed but not yet integrated
KS test sample size analysis finished, showing varied separation by service type
GDP comparison framework designed but not yet executed
Plan: Compare average GDP values inside vs outside hotspot areas using stratified sampling
Research Direction and Publication Strategy
Regional aggregation level decision pending
Continental vs World Bank regional groupings both viable
World Bank regions provide more specificity, separate Americas and Asia
Continental groupings more intuitive for general audience
Decision criteria: Choose based on whether finer resolution reveals meaningful distinctions
Alternative groupings (UN regions) ruled out due to overly broad Americas categorization
Narrative development priorities
Focus on pixel-level results as primary findings
10km comparisons valuable for supplement showing scale-dependency
Emphasize sign flip resolution and methodological improvements
Highlight regional patterns in ecosystem service change trajectories
Next Steps and Timeline
Immediate tasks (by Tuesday meeting)
Becky: Share pixel area raster for per-hectare conversions
esa_pixel_area_ha_md5_1dd3298a7c4d25c891a11e01868b5db6.tif
Becky: Review all of the results so far, think about any narrative to pull out
Jeronimo: Divide all rasters by area of pixel raster shared above; re-run results on per hectare instead of per-pixel rasters (for all but ratio metrics; they're already standardized and will lose meaning if divided by area)
Jeronimo: Calculate coastal risk reduction ratio in new spatially joined gpkg and rerun pipeline on vector not raster for coastal metrics
Jeronimo: Complete GDP comparison analysis (hotspot vs non-hotspot average values)
Jeronimo: Recalculate hotspot percentages using total zone areas, not just valid data extent
Jeronimo: Integrate completed KS test results into presentation materials
Meeting schedule adjustments
Tuesday follow-up meeting scheduled (Monday unavailable due to weekend family birthday)
Thursday retained as separate performance check-in discussion
Tuesday focus: Technical results review and narrative development
Goal: Have all analytical components ready for manuscript drafting phase

Chat with meeting transcript: https://notes.granola.ai/t/adb4f3bf-3724-4353-8029-78f02ed85c59-00demib2

Wed, 14 Jan 2026
Technical Issues Resolution
Sediment Retention Ratio: [RESOLVED] Status: successfully recalculated ratio rasters and difference maps. Zonal statistics have been extracted and validated.
Output: New plots generated; currently refining visualization Pending to add C_prtection Ratio.

Need to either remove trimming and rescale axes instead of capping data values
Violin Plots: [RESOLVED] Refactored code to remove hard trimming (which caused "fat tails"). Moving to a dynamic axis-rescaling approach (using 1st/99th percentiles for view limits only).


Data Analysis Progress & Methodology
Switched to equal area grid (from 10km grid) - Pixel-level calculations vs spreadsheet calculations showing mostly consistent results
Error bars minimal but present in pixel-level analysis
Nitrogen retention showing comparable values between methods
Trimming approach causing artificial distribution shapes in violin plots
Thresholding creates fat tails at both ends —low end because it's top 5% of values, high end because we're trimming at 99.9%
Need to either remove trimming and rescale axes instead of capping data values
Violin Plots: Refactoring code to remove hard trimming (which caused "fat tails"). Moving to a dynamic axis-rescaling approach (using 1st/99th percentiles for view limits only). [RESOLVED] Refactored code to remove hard trimming (which caused "fat tails"). Moving to a dynamic axis-rescaling approach (using 1st/99th percentiles for view limits only)
Percent Area Calculations: Percent Area Calculations: [RESOLVED] Developed code to calculate the percentage of land area classified as "Hotspots" within each spatial grouping.
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
Plan to subsample non-hotspots for fair comparison: Re analysis run on a subset. [COMPLETED]
Try both random sampling and median spots (5% at median change)
Next Steps
Priority: Create hotspot summary statistics
Percent area that is hotspot by region/biome/income group [completed]
Average number of hotspots per location
“Hotness” intensity analysis for existing hotspots
Secondary tasks:
Fix violin plot trimming issues (remove data trimming, cap axes only)
Create equal sample size KS test comparisons
Establish living PowerPoint with current figures (not static PDF)
Share sediment retention file for debugging
Schedule mid-year evaluation for next meeting
Upcoming group meeting end of month + all science meeting Jan 22nd

Detailed action items
Sediment retention bug


Jeronimo: Share the problematic sediment retention ratio file. [completed]
Becky: Run sediment model on Big Boi with Rich’s code and debug missing/-Inf/NA values  [COMPLETED]

Hotspot summary statistics (priority)


Jeronimo: Use equal-area hotspot maps to quantify:
Calculate percent area that is hotspot by region/biome/income group (and other existing groupings). [COMPLETED]
Action Taken: Developed and executed the script to calculate the spatial extent of hotspots.
Outcome: Generated summary tables quantifying the percentage of land area classified as "Hotspots" for all spatial groupings (Biomes, UN Regions, Sub-regions).
Metric: (Hotspot Area / Total Group Area) * 100
Calculate average number of hotspots per location (“hotness”) - Distribution of “number of services” per hotspot pixel (how many services each pixel is hotspot for) -Solved, charts added to the pptx
Consider simple summaries/visuals showing where hotspots are most multi-service and which regions/biomes have the most multi-service hotspots. [COMPLETED]

Socioeconomic KS tests & comparisons


Jeronimo:
Create sanity-check violin (or similar) plots comparing GDP (and other covariates) in hotspots vs non-hotspots to verify KS-test interpretation.
Address sample-size imbalance by:
Subsampling non-hotspots (random 5% to match hotspot share).
Optionally sampling around the median (5% of “typical change” pixels).
Re-run KS tests with these balanced samples.

Hotspot distributions & violin plots


Jeronimo:
Remove percentile trimming of underlying values; instead fix axis limits (no capping data, only rescaling axes).
Re-generate violin/box plots with these fixed axes.
Test alternative visualizations if needed (e.g., box-and-whisker, zoomed-in versions).

Absolute vs pixel-level aggregation check (lower priority)


Jeronimo: Later, compare results computed:
At pixel level from rasters vs
From grid-aggregated values (spreadsheet-style)
 to document how similar/different they are (likely for appendix).
Charts preodeuced and made closer, pending intyepretation)

Sediment retention ratio integration


Jeronimo & Becky:
Once sediment issue is fixed, integrate sediment retention ratio into same summary/figure framework as other services (risks across top, retention ratios, pollination, etc.). [COMPLETED]
Ensure consistent ordering of locations across variables. [COMPLETED]

Figure management


Jeronimo:
Create and maintain a living PowerPoint (or similar) with the current “canonical” figures for the paper (replacing scattered PDFs). Located here: Hotspot_analysis_JRE.pptx
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


**Wed, 22 Jan 26**

### Completed Tasks

*   **Hotspot Intensity Analysis**:
    *   Created `analysis/hotspot_intensity.qmd`.
    *   Calculated % area coverage and share of global hotspots by region/biome.
    *   Implemented "Top 15" filtering for countries and canonical service ordering.
*   **Multi-service Analysis**:
    *   Created `analysis/hotspot_multiservice.qmd`.
    *   Analyzed hotspot overlap ("hotness") and identified regions with high multi-service coincidence.
*   **Visualization Updates**:
    *   Refactored signed change bar plots in `hotspot_extraction.qmd` (flipped axes).
    *   Excluded non-terrestrial regions (Seven Seas, Antarctica) from summary plots.
*   **KS Tests**:
    *   Updated `analysis/KS_tests_hotspots.qmd` with sanity-check EKCD plots (balanced sampling).
    *   Fixed data loading to ensure socioeconomic variables are available.

### Next Steps

*   **Interpretation**:
    *   Review `ks_results_hot_vs_non.csv` and new violin plots to validate socioeconomic findings.
    *   Analyze `hotspot_area_stats.csv` and `hotspot_multiservice_stats.csv` for key geographic takeaways.
*   **Documentation**:
    *   Assemble "Living PowerPoint" with the new canonical figures.
*   **Admin**:
    *   Mid-year evaluation.
The updates above address the following detailed action items (for your reference in updating the existing list):
Hotspot summary statistics (priority) is largely complete.
Socioeconomic KS tests & comparisons is in progress (plots created, next step is interpretation).
Hotspot distributions & violin plots is in progress (visualizations refactored).
Figure management is a next step (assemble "Living PowerPoint").
Admin / collaboration / evaluation is a next step (Mid-year evaluation).
