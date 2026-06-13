#!/usr/bin/env python
"""
Extract key numbers from analysis outputs for book placeholders.
Generates a markdown file with all calculated values ready to fill into chapters.
"""

import pandas as pd
import os
from pathlib import Path

# TODO (v1.4.0 Technical Debt): Consolidate table directories!
# Historically, `data/processed/tables` lived on the remote server (lilling), while 
# `outputs/tables` was used for local scripts. We need to unify these into a single 
# canonical directory across all Python, R, and Quarto scripts to prevent confusion.
# -----------------------------------------------------------------------------------
data_root = Path("data/processed/tables")
outputs_root = Path("outputs/tables")

# Load all the data
hotspot_area = pd.read_csv(data_root / "hotspot_area_stats.csv")
hotspot_pop = pd.read_csv(data_root / "hotspot_pop_exposure.csv")
exposure_detail = pd.read_csv(outputs_root / "exposure_comparison_compiled.csv")
ks_results = pd.read_csv(data_root / "ks_results_hot_vs_non.csv")
lcc_abs = pd.read_csv(data_root / "lcc_es_hotspot_overlap_abs.csv")
lcc_pct = pd.read_csv(data_root / "lcc_es_hotspot_overlap_pct.csv")

# ============================================================================
# KEY CALCULATIONS
# ============================================================================

print("Extracting key numbers for book placeholders...\n")

import geopandas as gpd

# 1. GLOBAL HOTSPOT COUNT
# Get unique grid cells that are hotspots for at least one service
try:
    gpkg_path = "data/processed/hotspots/pct/global/hotspots_global_pct_epsg8857.gpkg"
    global_hotspot_count = len(gpd.read_file(gpkg_path, ignore_geometry=True))
except Exception as e:
    print(f"Warning: Could not load GPKG for hotspot count ({e}). Using fallback.")
    global_hotspot_count = 252215  # Fallback to known value if GPKG is inaccessible
print(f"Global hotspot cells: {global_hotspot_count:,}")

# 2. TOTAL GRID CELLS
# From the data, total cells globally across all income groups
total_cells_global = hotspot_area[hotspot_area['grouping_var'] == 'income_grp']['n_total'].sum() / 8  # 8 services
total_cells_global = int(total_cells_global)
print(f"Total grid cells: {total_cells_global:,}")

# 3. HOTSPOT PERCENTAGE
hotspot_pct_global = (global_hotspot_count / total_cells_global) * 100
print(f"Global hotspot coverage: {hotspot_pct_global:.2f}%")

# 4. POP EXPOSURE BY INCOME
pop_exp_income = hotspot_pop.groupby('income_grp')['exposed_population'].sum() / 1e6  # millions
pop_exp_total = pop_exp_income.sum()
pop_exp_low_income = pop_exp_income.get('Low income', 0) + pop_exp_income.get('Lower-Middle income', 0)
pop_pct_low_income = (pop_exp_low_income / pop_exp_total) * 100
print(f"\nPopulation in hotspots (millions):")
print(f"  Total: {pop_exp_total:.1f}M")
print(f"  Low + Lower-middle income: {pop_exp_low_income:.1f}M ({pop_pct_low_income:.1f}%)")

# 5. DRIVER ATTRIBUTION (absolute change)
if not lcc_abs.empty:
    driver_cols = [col for col in lcc_abs.columns if col not in ['service', 'driver']]
    # Get overall patterns
    top_driver_abs = lcc_abs.groupby('driver')['pct_overlap'].mean().sort_values(ascending=False)
    print(f"\nDriver attribution (absolute change):")
    for driver, pct in top_driver_abs.head(3).items():
        print(f"  {driver}: {pct:.1f}%")

# 6. REGIONAL HOTSPOT INTENSITY
regional_intensity = hotspot_area[hotspot_area['grouping_var'] == 'region_wb'].copy()
regional_intensity = regional_intensity.groupby('group')['relative_intensity'].mean().sort_values(ascending=False)
print(f"\nRegional hotspot intensity (relative):")
for region, intensity in regional_intensity.head(3).items():
    print(f"  {region}: {intensity:.2f}x")

# 7. LOW VS HIGH INCOME INTENSITY
income_intensity = hotspot_area[hotspot_area['grouping_var'] == 'income_grp'].groupby('group')['relative_intensity'].mean()
low_income_int = income_intensity.get('5. Low income', 0)
high_income_int = income_intensity.get('1. High income: OECD', 0)
print(f"\nHotspot intensity by income:")
print(f"  Low income: {low_income_int:.2f}x")
print(f"  High income OECD: {high_income_int:.2f}x")

# 8. KS TEST RESULTS (significant differences)
ks_sig = ks_results[ks_results['p_adj'] < 0.05]
ks_sig_count = len(ks_sig)
ks_total = len(ks_results)
print(f"\nKS test results:")
print(f"  Significant differences: {ks_sig_count}/{ks_total} ({(ks_sig_count/ks_total)*100:.1f}%)")

# 9. NEW: POPULATION EXPOSURE MULTIPLIER (All Hotspots)
exp_all = exposure_detail[exposure_detail['overlap_category'] == 'all hotspots'].copy()
global_exp = exp_all[exp_all['region_wb'] == 'Global']

pop_hydro = global_exp[global_exp['exposure_type'] == 'hydrological']['population'].sum() / 1e6
pop_travel = global_exp[global_exp['exposure_type'] == 'travel_footprint']['population'].sum() / 1e6
pop_combined = global_exp[global_exp['exposure_type'] == 'combined_total']['population'].sum() / 1e6
multiplier_effect = pop_travel / pop_hydro if pop_hydro > 0 else 0

print(f"\nPopulation Exposure (All Hotspots):")
print(f"  Hydrological (Downstream): {pop_hydro:.1f}M")
print(f"  Travel Footprint (Access): {pop_travel:.1f}M")
print(f"  Combined (Union): {pop_combined:.1f}M")
print(f"  Multiplier Effect (Travel vs. Hydro): {multiplier_effect:.2f}x")

# ============================================================================
# GENERATE MARKDOWN OUTPUT
# ============================================================================

markdown = f"""# Book Data Fills

Auto-generated from analysis outputs. Use these values to fill X/Y placeholders in chapters.

## GLOBAL SUMMARY
- **Total grid cells**: {total_cells_global:,}
- **Hotspot cells (≥1 service)**: {global_hotspot_count:,}
- **Hotspot coverage**: {hotspot_pct_global:.2f}%

## CHAPTER-SPECIFIC FILLS

### 01-problem.qmd
*No numerical placeholders - narrative only*

### 03-global-patterns.qmd
- X = {global_hotspot_count:,} (total hotspot cells)
- Y = {hotspot_pct_global:.2f}% (global area)

### 04-hotspot-WHERE.qmd
*Regional intensity table:*
```
| Region | Relative Intensity |
|--------|-------------------|
{chr(10).join([f"| {region} | {intensity:.2f}x |" for region, intensity in regional_intensity.head(5).items()])}
```

### 05-hotspot-WHO.qmd
- Low-income populations: {pop_pct_low_income:.1f}% of people affected
- **Total Population in Hotspot Beneficiary Areas**: {pop_combined:.1f}M people
- Population in hotspots: {pop_exp_total:.1f}M people
- **Hydrological Beneficiaries**: {pop_hydro:.1f}M people
- **Access-Based Beneficiaries**: {pop_travel:.1f}M people
- **The Multiplier Effect**: Populations with access to hotspots are **{multiplier_effect:.2f} times larger** than populations living directly downstream.


### 06-drivers-WHY.qmd
*Top drivers of ecosystem service decline:*
```
| Driver | % Overlap |
|--------|----------|
{chr(10).join([f"| {driver} | {pct:.1f}% |" for driver, pct in top_driver_abs.head(3).items()])}
```

### 07-regional-profiles.qmd
*Regional population exposure and vulnerability*
[To be filled with specific regional data from hotspot_pop_exposure.csv]

### 08-conclusions.qmd
- Hotspot coverage: {hotspot_pct_global:.2f}%
- Low-income hotspot intensity: {low_income_int:.2f}x (vs {high_income_int:.2f}x for OECD)
- Significant socioeconomic differences: {ks_sig_count}/{ks_total} tests (p<0.05)

---

## REFERENCE TABLES

### Hotspot Area Statistics (by service & income group)
See: `data/processed/tables/hotspot_area_stats.csv`
- n_hot = number of hotspot cells
- pct_area = percentage of region's area that is hotspot
- relative_intensity = hotspot concentration (>1 = enriched)

### Population Exposure
See: `data/processed/tables/hotspot_pop_exposure.csv`
- Total global population in hotspots: {pop_exp_total:.1f}M
- By income group: {', '.join([f'{income}: {pop:.1f}M' for income, pop in pop_exp_income.items()])}

### KS Test Results
See: `data/processed/tables/ks_results_hot_vs_non.csv`
- {ks_sig_count} out of {ks_total} service×variable pairs show significant differences
- Hotspots have significantly different socioeconomic profiles from background

### Driver Attribution
See: `data/processed/tables/lcc_es_hotspot_overlap_abs.csv` and `_pct.csv`
- Absolute change: driver overlap patterns
- Percent change: driver overlap patterns
"""

# Save to file
output_path = Path("outputs/book_data_fills.md")
output_path.parent.mkdir(parents=True, exist_ok=True)
with open(output_path, 'w', encoding='utf-8') as f:
    f.write(markdown)

print(f"\n[OK] Data fills saved to: {output_path}")
print(f"\nMarkdown preview:")
print("=" * 70)
print(markdown[:1000])
print("=" * 70)
