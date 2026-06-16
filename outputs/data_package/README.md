# Global Ecosystem Service Time Series: Data Package

This directory contains the core processed tabular data supporting the "Global NCP" spatial analysis, extracting 28-year biophysical trajectories (1992-2020) and localized hotspot metrics.

## 1. Hotspot Socioeconomic Profiling & Exposure (The "WHO")

- **`hotspot_pop_exposure.csv`**: Contains the baseline demographic exposure counts for people living *directly* within identified ecosystem service decline hotspots (Local Residents).
- **`ks_results_hot_vs_non.csv`**: Results of the Kolmogorov-Smirnov (KS) tests and Cliff's Delta effect sizes, comparing the socioeconomic profiles (Population, GDP, HDI) of hotspot grid cells against a matched baseline background.
- **`exposure_comparison_compiled.csv`**: A comprehensive grid-level dataset linking Local Residents to "Connected Beneficiaries" (Hydrological/Downstream populations and Access-based travel footprints).
- **`multiplier_summary_*.csv`** (e.g., `_WWF_biome.csv`, `_income_grp.csv`, `_region_wb.csv`, `_country.csv`): Aggregated summary tables calculating the "Multiplier Effect" of overlapping hotspots (Compound Risk) grouped by specific geographic or administrative boundaries. 
  - *Key columns*: The spatial grouping column, `Local_Residents`, `Downstream_Beneficiaries`, `Access_Beneficiaries`, and the resulting ratio expansions.

## 2. Land Cover Drivers and Attribution (The "WHY")

- **`lcc_es_hotspot_overlap_*.csv`** (`_abs`, `_pct`, and base): Cross-tabulation matrices calculating the spatial overlap between extreme ecosystem service hotspots and localized land cover conversion drivers (e.g., forest loss, agricultural expansion). These tables quantify the "Attribution Gap" (76%), demonstrating where service decline occurs without macro-scale land conversion.
- **`lcc_reclassification_table.csv`**: The correspondence dictionary used to map raw ESA CCI 300m categorical land cover codes into broad driver typologies.

## 3. Geographic Distribution and Intensity (The "WHERE")

- **`hotspot_area_stats.csv`**: Quantifies the total land area of hotspots per region/biome and calculates "Relative Intensity" (enrichment scores indicating if a region carries a disproportionate hotspot burden relative to its global size).
- **`hotspot_multiservice_stats.csv`**: Grid-level counts of overlapping ecosystem service declines, categorizing the landscape by "Compound Risk" (or Hotness).
