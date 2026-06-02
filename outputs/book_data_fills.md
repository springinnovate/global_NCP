# Book Data Fills

Auto-generated from analysis outputs. Use these values to fill X/Y placeholders in chapters.

## GLOBAL SUMMARY
- **Total grid cells**: 1,689,609
- **Hotspot cells (≥1 service)**: 75,271
- **Hotspot coverage**: 4.45%

## CHAPTER-SPECIFIC FILLS

### 01-problem.qmd
*No numerical placeholders - narrative only*

### 03-global-patterns.qmd
- X = 75,271 (total hotspot cells)
- Y = 4.45% (global area)

### 04-hotspot-WHERE.qmd
*Regional intensity table:*
```
| Region | Relative Intensity |
|--------|-------------------|
| Sub-Saharan Africa | 2.34x |
| Europe & Central Asia | 1.24x |
| North America | 1.16x |
| Latin America & Caribbean | 1.13x |
| East Asia & Pacific | 0.94x |
```

### 05-hotspot-WHO.qmd
- Low-income populations: 4.2% of people affected
- Population in hotspots: 7936.4M people

### 06-drivers-WHY.qmd
*Top drivers of ecosystem service decline:*
```
| Driver | % Overlap |
|--------|----------|
| Forest_Loss | 19.8% |
| Crop_Exp | 19.0% |
| Grassland_Loss | 7.7% |
```

### 07-regional-profiles.qmd
*Regional population exposure and vulnerability*
[To be filled with specific regional data from hotspot_pop_exposure.csv]

### 08-conclusions.qmd
- Hotspot coverage: 4.45%
- Low-income hotspot intensity: 2.18x (vs 1.12x for OECD)
- Significant socioeconomic differences: 47/48 tests (p<0.05)

---

## REFERENCE TABLES

### Hotspot Area Statistics (by service & income group)
See: `data/processed/tables/hotspot_area_stats.csv`
- n_hot = number of hotspot cells
- pct_area = percentage of region's area that is hotspot
- relative_intensity = hotspot concentration (>1 = enriched)

### Population Exposure
See: `data/processed/tables/hotspot_pop_exposure.csv`
- Total global population in hotspots: 7936.4M
- By income group: High income: 1399.7M, Low income: 334.7M, Lower middle income: 1953.9M, Upper middle income: 4248.1M

### KS Test Results
See: `data/processed/tables/ks_results_hot_vs_non.csv`
- 47 out of 48 service×variable pairs show significant differences
- Hotspots have significantly different socioeconomic profiles from background

### Driver Attribution
See: `data/processed/tables/lcc_es_hotspot_overlap_abs.csv` and `_pct.csv`
- Absolute change: driver overlap patterns
- Percent change: driver overlap patterns
