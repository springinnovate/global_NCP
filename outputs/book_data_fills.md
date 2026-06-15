# Book Data Fills

Auto-generated from analysis outputs. Use these values to fill X/Y placeholders in chapters.

## GLOBAL SUMMARY
- **Total grid cells**: 1,302,099
- **Hotspot cells (≥1 service)**: 252,215
- **Hotspot coverage**: 19.37%

## CHAPTER-SPECIFIC FILLS

### 01-problem.qmd
*No numerical placeholders - narrative only*

### 03-global-patterns.qmd
- X = 252,215 (total hotspot cells)
- Y = 19.37% (global area)

### 04-hotspot-WHERE.qmd
*Regional intensity table:*
```
| Region | Relative Intensity |
|--------|-------------------|
| Latin America & Caribbean | 1.40x |
| East Asia & Pacific | 1.30x |
| South Asia | 0.80x |
| Sub-Saharan Africa | 0.78x |
| Europe & Central Asia | 0.65x |
```

### 05-hotspot-WHO.qmd
- Low-income populations: 12.4% of people affected
- **Total Baseline Population (GHSL 2020)**: 7855.5M people
- **Total Population in Hotspot Beneficiary Areas**: 7596.0M people (96.7% of baseline)
- Population in hotspots: 5286.2M people
- **Hydrological Beneficiaries**: 6420.1M people
- **Access-Based Beneficiaries**: 7401.2M people
- **The Multiplier Effect**: Populations with access to hotspots are **1.15 times larger** than populations living directly downstream.


### 06-drivers-WHY.qmd
*Top drivers of ecosystem service decline:*
```
| Driver | % Overlap |
|--------|----------|
| Forest_Loss | 3.7% |
| Grassland_Gain | 2.7% |
| Grassland_Loss | 2.5% |
```

### 07-regional-profiles.qmd
*Regional population exposure and vulnerability*
[To be filled with specific regional data from hotspot_pop_exposure.csv]

### 08-conclusions.qmd
- Hotspot coverage: 19.37%
- Low-income hotspot intensity: 0.84x (vs 0.74x for OECD)
- Significant socioeconomic differences: 39/40 tests (p<0.05)

---

## REFERENCE TABLES

### Hotspot Area Statistics (by service & income group)
See: `data/processed/tables/hotspot_area_stats.csv`
- n_hot = number of hotspot cells
- pct_area = percentage of region's area that is hotspot
- relative_intensity = hotspot concentration (>1 = enriched)

### Population Exposure
See: `data/processed/tables/hotspot_pop_exposure.csv`
- Total global population in hotspots: 5286.2M
- By income group: High income: 703.1M, Low income: 656.7M, Lower middle income: 1635.0M, Upper middle income: 2291.4M

### KS Test Results
See: `data/processed/tables/ks_results_hot_vs_non.csv`
- 39 out of 40 service×variable pairs show significant differences
- Hotspots have significantly different socioeconomic profiles from background

### Driver Attribution
See: `data/processed/tables/lcc_es_hotspot_overlap_abs.csv` and `_pct.csv`
- Absolute change: driver overlap patterns
- Percent change: driver overlap patterns
