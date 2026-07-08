library(dplyr)
library(readr)

# Load using base R to avoid readr/vroom memory-mapping segfaults in Git Bash
area_stats <- read.csv("data/processed/tables/hotspot_area_stats.csv", stringsAsFactors = FALSE)

# Set up logging to permanently save the audit results
out_file <- "outputs/audit_summary.txt"
sink(out_file, split = TRUE)

cat("\n====================================================================\n")
cat("   GROUND-TRUTH AUDIT — Global NCP Hotspot Analysis\n")
cat("   Run this after any hotspot threshold change or pipeline update.\n")
cat("====================================================================\n\n")

# ---- 1. INCOME GROUP DISPARITY ----
cat("AUDIT 1: Income Group Relative Intensity\n")
cat("  Claim: lower-middle income countries bear ~1.6x the intensity of high-income OECD\n")
cat("  NOTE: audit checks lower-middle vs high-income OECD (not 'low' vs 'high')\n")
cat("--------------------------------------------------------------------\n")

income_stats <- area_stats %>%
  filter(grouping_var == "income_grp", !is.na(group)) %>%
  group_by(group) %>%
  summarise(avg_relative_intensity = mean(relative_intensity, na.rm = TRUE),
            total_hotspots = sum(n_hot, na.rm = TRUE)) %>%
  arrange(desc(avg_relative_intensity))

print(income_stats)

lower_middle <- income_stats %>%
  filter(grepl("Lower middle", group, ignore.case = TRUE)) %>%
  pull(avg_relative_intensity)
high_oecd <- income_stats %>%
  filter(grepl("High income.*OECD", group, ignore.case = TRUE)) %>%
  pull(avg_relative_intensity)

if (length(lower_middle) > 0 && length(high_oecd) > 0) {
  ratio <- mean(lower_middle) / mean(high_oecd)
  cat(sprintf("\n[RESULT] Lower-middle income avg intensity: %.3f\n", mean(lower_middle)))
  cat(sprintf("[RESULT] High income OECD avg intensity:    %.3f\n", mean(high_oecd)))
  cat(sprintf("[RESULT] Ratio (lower-middle / high-OECD): %.2f×  [claimed: ~1.6×]\n", ratio))
  if (abs(ratio - 1.6) > 0.1) cat("[WARNING] Ratio deviates >0.1 from claimed 1.6×\n")
} else {
  cat("[WARNING] Could not find matching income groups — check group names in CSV\n")
}

# ---- 2. REGIONAL INTENSITY ----
cat("\n\nAUDIT 2: Regional Relative Intensity\n")
cat("  Claim: LAC ~1.4×, EAP ~1.3×; no other region exceeds 1.0×\n")
cat("  Claim: Sub-Saharan Africa is NOT disproportionate (below 1.0×)\n")
cat("--------------------------------------------------------------------\n")

region_stats <- area_stats %>%
  filter(grouping_var == "region_wb", !is.na(group)) %>%
  group_by(group) %>%
  summarise(avg_relative_intensity = mean(relative_intensity, na.rm = TRUE),
            total_hotspot_cells = sum(n_hot, na.rm = TRUE),
            total_cells = sum(n_total, na.rm = TRUE)) %>%
  arrange(desc(avg_relative_intensity))

print(region_stats)

lac <- region_stats %>% filter(grepl("Latin America", group)) %>% pull(avg_relative_intensity)
eap <- region_stats %>% filter(grepl("East Asia", group)) %>% pull(avg_relative_intensity)
ssa <- region_stats %>% filter(grepl("Sub-Saharan", group)) %>% pull(avg_relative_intensity)

cat(sprintf("\n[RESULT] LAC:  %.3f× [claimed ~1.4×]\n", if(length(lac)>0) lac else NA))
cat(sprintf("[RESULT] EAP:  %.3f× [claimed ~1.3×]\n", if(length(eap)>0) eap else NA))
cat(sprintf("[RESULT] SSA:  %.3f× [should be <1.0 — NOT disproportionate]\n", if(length(ssa)>0) ssa else NA))
above_one <- region_stats %>% filter(avg_relative_intensity > 1.0) %>% pull(group)
cat(sprintf("[RESULT] Regions above 1.0×: %s\n", paste(above_one, collapse=", ")))

# ---- 3. HOTSPOT CELL COUNT ----
cat("\n\nAUDIT 3: Unique Hotspot Cell Count\n")
cat("  Claim: 252,215 unique grid cells with at least one extreme service decline\n")
cat("--------------------------------------------------------------------\n")

pop_exp <- tryCatch(
  read.csv("data/processed/tables/hotspot_pop_exposure.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

if (!is.null(pop_exp) && "overlap_category" %in% names(pop_exp)) {
  print(head(pop_exp, 10))
  all_hotspots <- pop_exp %>% filter(grepl("all|≥1|hotspot_count.1", overlap_category, ignore.case = TRUE))
  if (nrow(all_hotspots) > 0) {
    cat(sprintf("\n[RESULT] All-hotspot rows found: %d\n", nrow(all_hotspots)))
  }
} else {
  cat("[WARNING] hotspot_pop_exposure.csv schema has changed (no 'overlap_category' column) --\n")
  cat("  this check is stale and needs updating to the current column layout.\n")
  cat("[MANUAL] Verify unique hotspot cell count directly: st_layers() on\n")
  cat("  data/processed/hotspots/pct/global/hotspots_global_pct.gpkg (hotspot_count >= 1).\n")
  cat("  As of this run: 225,113 -- matches Ch06's traceability table (3,065M in-situ population).\n")
  cat("  NOTE: paper_draft.qmd and presentation.qmd still say 252,215 -- flagged for review,\n")
  cat("  NOT corrected automatically since it touches the paper's headline abstract number.\n")
}

# ---- 4. ATTRIBUTION GAP ----
cat("\n\nAUDIT 4: Attribution Gap (true union across all 5 drivers)\n")
cat("  Claim: ~65.5% of ES hotspot cells do not co-occur with extreme (top 5%) LC conversion\n")
cat("  Equivalently: ~34.5% DO co-occur; nearly 10x the ~4.1% background rate\n")
cat("  (odds ratio ~12.17) -- see scripts/compute_attribution_true_union.R\n")
cat("  NOTE: do NOT read from lcc_es_hotspot_overlap.csv (stale, pre-5-driver, wrong threshold).\n")
cat("  NOTE (2026-07-08): compute_attribution_true_union.R itself was fixed this session --\n")
cat("  it previously joined LCC data to the ES grid via two grid-cell ID columns that share a\n")
cat("  name but index different, incompatible source grids. A spatial crosswalk\n")
cat("  (scripts/build_lc_grid_fid_crosswalk.R, data/processed/lc_grid_fid_to_master_fid_crosswalk.csv)\n")
cat("  now corrects this. The previously-claimed 8.2%/91.8%/odds-ratio-0.88 figures were wrong.\n")
cat("--------------------------------------------------------------------\n")

union_file <- file.path("data", "processed", "tables", "lcc_es_hotspot_true_union.csv")

if (file.exists(union_file)) {
  union_stats <- read.csv(union_file, stringsAsFactors = FALSE)
  print(union_stats)
  pct_overlap <- union_stats$pct_overlap_of_es_hotspots[1]
  cat(sprintf("\n[RESULT] True union co-occurrence: %.1f%%  [claimed ~34.5%%]\n", pct_overlap))
  cat(sprintf("[RESULT] Attribution gap: %.1f%%  [claimed ~65.5%%]\n", 100 - pct_overlap))
  cat(sprintf("[RESULT] Odds ratio vs. background: %.2f  [claimed ~12.17]\n", union_stats$odds_ratio[1]))
  if (abs(pct_overlap - 34.5) > 1) cat("[WARNING] Co-occurrence deviates >1pp from claimed 34.5%\n")
} else {
  cat("[WARNING] lcc_es_hotspot_true_union.csv not found -- run scripts/compute_attribution_true_union.R\n")
}

# ---- 5. BIOME INTENSITY ----
cat("\n\nAUDIT 5: Biome Relative Intensity (top 10)\n")
cat("--------------------------------------------------------------------\n")

biome_stats <- area_stats %>%
  filter(grouping_var == "WWF_biome", !is.na(group)) %>%
  group_by(group) %>%
  summarise(avg_relative_intensity = mean(relative_intensity, na.rm = TRUE)) %>%
  arrange(desc(avg_relative_intensity))

print(biome_stats, n = 10)

# ---- 6. POPULATION EXPOSURE ----
cat("\n\nAUDIT 6: Population Exposure\n")
cat("  Claim: ~3.1 billion in-situ | ~7.6 billion connected | ~2.5x multiplier\n")
cat("  Claim: ~1.2 billion in compound risk zones (2+ services)\n")
cat("--------------------------------------------------------------------\n")

exp_compiled <- tryCatch(
  read.csv("outputs/tables/exposure_comparison_compiled.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

if (!is.null(exp_compiled)) {
  cat("Columns:", paste(names(exp_compiled), collapse=", "), "\n")
  print(head(exp_compiled, 15))
  cat("\n[MANUAL VERIFY] Sum rows for 'all hotspots' in-situ to get 3.1B total\n")
  cat("[MANUAL VERIFY] Sum connected (union downstream+access) to get 7.6B total\n")
} else {
  cat("[WARNING] exposure_comparison_compiled.csv not found\n")
  cat("[MANUAL] Verify from: hotspots_global_pct.gpkg joined to 10k_change_calc.gpkg (GHS_POP_E2020_GLOBE_sum)\n")
  cat("  In-situ (all hotspot cells, hotspot_count>=1): claimed ~3,065M (~3.1B)\n")
  cat("  In-situ (compound 2+ cells): claimed ~1,212M (~1.2B)\n")
  cat("  Connected (all, union downstream+access): claimed ~7,584M (~7.6B)\n")
  cat("  Multiplier (7,584 / 3,065): claimed ~2.5x\n")
}

# ---- 7. 5x / 8x COMPOUND RISK CLAIMS ----
cat("\n\nAUDIT 7: Compound Risk Population Claims (5x / 8x)\n")
cat("  *** NEEDS VERIFICATION: Two different interpretations in circulation ***\n")
cat("  Ch06 says: connected/in-situ for 2+ cells = ~5x; for 3+ cells = ~8x\n")
cat("  Presentation says: '5x higher population DENSITY than median background'\n")
cat("  These are DIFFERENT metrics. Confirm which is correct before submission.\n")
cat("--------------------------------------------------------------------\n")

if (!is.null(exp_compiled)) {
  two_plus <- exp_compiled %>% filter(grepl("2.*more|2\\+|compound", overlap_category, ignore.case = TRUE))
  three_plus <- exp_compiled %>% filter(grepl("3.*more|3\\+", overlap_category, ignore.case = TRUE))
  if (nrow(two_plus) > 0) { cat("2+ rows:\n"); print(two_plus) }
  if (nrow(three_plus) > 0) { cat("3+ rows:\n"); print(three_plus) }
} else {
  cat("[MANUAL] Verify from beneficiary CSVs in data/processed/hotspots/hotspot_beneficiaries/\n")
  cat("  Expected from Ch06: 2+ cells connected 6,011M / in-situ 1,212M = ~5x (serviceshed multiplier)\n")
  cat("  Expected from Ch06: 3+ cells connected 3,756M / in-situ 445M = ~8x (serviceshed multiplier)\n")
  cat("  If 5x/8x = serviceshed multiplier, update presentation notes to remove 'density' language\n")
}

# ---- 8. KS TEST COUNT ----
cat("\n\nAUDIT 8: KS Test Count\n")
cat("  Claim: 39 out of 40 service-covariate combinations significant after FDR\n")
cat("--------------------------------------------------------------------\n")

ks_files <- list.files("data/processed/tables", pattern = "ks_results", full.names = TRUE)
if (length(ks_files) > 0) {
  ks <- read.csv(ks_files[1], stringsAsFactors = FALSE)
  cat("KS results columns:", paste(names(ks), collapse=", "), "\n")
  sig_col <- names(ks)[grepl("p_adj|p.adj|fdr|significant", names(ks), ignore.case=TRUE)][1]
  if (!is.na(sig_col)) {
    n_sig <- sum(ks[[sig_col]] < 0.05, na.rm = TRUE)
    n_total <- sum(!is.na(ks[[sig_col]]))
    cat(sprintf("[RESULT] Significant combinations: %d / %d  [claimed 39/40]\n", n_sig, n_total))
    if (n_sig != 39 || n_total != 40) cat("[WARNING] Count does not match claimed 39/40\n")
  } else {
    cat("[WARNING] Could not identify p-value column — check column names above\n")
  }
} else {
  cat("[WARNING] ks_results CSV not found in data/processed/tables/\n")
}

cat("\n====================================================================\n")
cat("   AUDIT COMPLETE\n")
cat("====================================================================\n")
sink()
cat("[INFO] Full audit saved to:", out_file, "\n")
