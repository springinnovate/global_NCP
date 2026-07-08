# Computes the true cell-level union overlap between ES hotspots and LCC driver
# hotspots across all 5 current drivers (Forest_Loss, Crop_Exp, Urban_Exp,
# Grassland_Loss, Grassland_Gain), replacing the stale, 2-driver
# lcc_es_hotspot_overlap.csv (last generated 2026-05-21, superseded by the
# 5-driver pct/abs refactor of 2026-06-04, but never itself regenerated).
#
# Uses the exact same driver-hotspot definition as the live per-driver pipeline
# in analysis/hotspot_extraction.qmd (top-5% cutoff via quantile(.,0.95) over
# the full 10km grid) and the canonical ES-hotspot cell set already used
# throughout the book (hotspots_global_pct.gpkg, hotspot_count >= 1, the same
# 225,113 cells behind the Ch06 3.1B in-situ population figure).
#
# No script previously computed the union across drivers -- only per-driver
# marginals existed. This fills that gap and writes reproducible outputs.
#
# IMPORTANT: 10k_lcc_granular_metrics.gpkg's own `grid_fid` is a row-index into
# its OWN source grid (AOOGrid_10x10km_land_4326_clean.gpkg, 1,691,819 cells),
# which is a *different* file from landgrid_1_clean_enriched_4326.gpkg (the
# master grid behind hotspots_global_pct.gpkg's `grid_fid`, 1,522,073 cells).
# The two id schemes are unrelated despite the shared column name -- joining
# them directly (as an earlier version of this script did) pairs cells
# essentially at random. We remap lc's grid_fid to the master-grid fid via a
# nearest-centroid spatial crosswalk (see scripts/build_lc_grid_fid_crosswalk.R)
# before any id-based comparison against es_fids.

library(sf)
library(dplyr)

source("R/paths.R")

lc_gpkg  <- file.path(data_dir(), "processed", "10k_lcc_granular_metrics.gpkg")
es_gpkg  <- file.path(data_dir(), "processed", "hotspots", "pct", "global", "hotspots_global_pct.gpkg")
out_dir  <- file.path(data_dir(), "processed", "tables")
crosswalk_path <- file.path(data_dir(), "processed", "lc_grid_fid_to_master_fid_crosswalk.csv")

drv_query <- paste0(
  "SELECT grid_fid, ",
  "ForestLoss_Loss_Forest_2020_1992 AS Forest_Loss, ",
  "Expansion_Gain_Cropland_2020_1992 AS Crop_Exp, ",
  "Expansion_Gain_Urban_2020_1992 AS Urban_Exp, ",
  "GrasslandLoss_Loss_Grassland_2020_1992 AS Grassland_Loss, ",
  "GrasslandLoss_Gain_Grassland_2020_1992 AS Grassland_Gain ",
  "FROM \"10k_lcc_granular_metrics\""
)
lc_raw <- as.data.frame(st_read(lc_gpkg, query = drv_query, quiet = TRUE))

# Remap lc's grid_fid (own-grid row index) to the master-grid fid used by
# hotspots_global_pct.gpkg, dropping cells with no true spatial correspondence
# (nearest master cell centroid >6km away -- i.e. not the same cell).
crosswalk <- read.csv(crosswalk_path) %>% filter(valid_match)
lc <- lc_raw %>%
  inner_join(crosswalk %>% select(lc_grid_fid, master_fid), by = c("grid_fid" = "lc_grid_fid")) %>%
  select(-grid_fid) %>%
  rename(grid_fid = master_fid)
message(sprintf("LC rows: %d raw -> %d after crosswalk (dropped %d with no master-grid correspondence)",
                 nrow(lc_raw), nrow(lc), nrow(lc_raw) - nrow(lc)))
n_grid <- nrow(lc)

es <- as.data.frame(st_read(es_gpkg, query = "SELECT grid_fid FROM hotspots_global_pct", quiet = TRUE))
es_fids <- es$grid_fid
n_es <- length(es_fids)

drv_cols <- c("Forest_Loss", "Crop_Exp", "Urban_Exp", "Grassland_Loss", "Grassland_Gain")

# ---- Per-driver magnitude/extent + hotspot sets (top-5% cutoff, full grid) ----
magnitude_summary <- list()
hs_sets <- list()
for (col in drv_cols) {
  x <- lc[[col]]
  cutoff <- as.numeric(quantile(x, 0.95, na.rm = TRUE))
  keep <- !is.na(x) & x > cutoff
  hs_sets[[col]] <- lc$grid_fid[keep]
  magnitude_summary[[col]] <- tibble(
    driver          = col,
    n_nonNA         = sum(!is.na(x)),
    n_nonzero       = sum(x > 0, na.rm = TRUE),
    total_magnitude = sum(x, na.rm = TRUE),
    mean_magnitude  = mean(x, na.rm = TRUE),
    cutoff_p95      = cutoff,
    n_hotspot_cells = length(hs_sets[[col]])
  )
}
magnitude_summary <- bind_rows(magnitude_summary)
write.csv(magnitude_summary, file.path(out_dir, "lcc_driver_magnitude_summary.csv"), row.names = FALSE)

# ---- Per-driver risk ratio vs. ES hotspot status ----
per_driver_risk <- list()
for (col in drv_cols) {
  drv_fids <- hs_sets[[col]]
  n_drv <- length(drv_fids)
  n_both <- length(intersect(es_fids, drv_fids))
  n_es_only  <- n_es - n_both
  n_drv_only <- n_drv - n_both
  n_neither  <- n_grid - n_es - n_drv + n_both
  tab <- matrix(c(n_both, n_drv_only, n_es_only, n_neither), nrow = 2)
  ct <- suppressWarnings(chisq.test(tab, correct = FALSE))
  or_test <- fisher.test(tab)
  per_driver_risk[[col]] <- tibble(
    driver           = col,
    n_driver_hotspot = n_drv,
    pct_grid         = 100 * n_drv / n_grid,
    n_overlap        = n_both,
    pct_of_es_hotspots = 100 * n_both / n_es,
    risk_ratio       = (n_both / n_es) / (n_drv_only / (n_grid - n_es)),
    odds_ratio       = unname(or_test$estimate),
    or_ci_low        = or_test$conf.int[1],
    or_ci_high       = or_test$conf.int[2],
    p_value          = ct$p.value
  )
}
per_driver_risk <- bind_rows(per_driver_risk)
write.csv(per_driver_risk, file.path(out_dir, "lcc_es_hotspot_per_driver_risk.csv"), row.names = FALSE)

# ---- True union across all 5 drivers ----
union_fids <- Reduce(union, hs_sets)
n_union <- length(union_fids)
n_both_union <- length(intersect(es_fids, union_fids))

n_es_only_u  <- n_es - n_both_union
n_drv_only_u <- n_union - n_both_union
n_neither_u  <- n_grid - n_es - n_union + n_both_union
tab_u <- matrix(c(n_both_union, n_drv_only_u, n_es_only_u, n_neither_u), nrow = 2)
ct_u <- suppressWarnings(chisq.test(tab_u, correct = FALSE))
or_u <- fisher.test(tab_u)

union_summary <- tibble(
  n_grid_cells        = n_grid,
  n_es_hotspot_cells   = n_es,
  n_driver_union_cells = n_union,
  pct_driver_union_of_grid = 100 * n_union / n_grid,
  n_overlap            = n_both_union,
  pct_overlap_of_es_hotspots = 100 * n_both_union / n_es,
  pct_driver_union_among_non_es = 100 * n_drv_only_u / (n_grid - n_es),
  risk_ratio           = (n_both_union / n_es) / (n_drv_only_u / (n_grid - n_es)),
  odds_ratio           = unname(or_u$estimate),
  or_ci_low            = or_u$conf.int[1],
  or_ci_high           = or_u$conf.int[2],
  p_value              = ct_u$p.value
)
write.csv(union_summary, file.path(out_dir, "lcc_es_hotspot_true_union.csv"), row.names = FALSE)

cat("\n================ TRUE UNION ATTRIBUTION SUMMARY ================\n")
cat(sprintf("ES hotspot cells (any service, canonical): %d\n", n_es))
cat(sprintf("Driver-union hotspot cells (any of 5 drivers, top-5%% each): %d (%.2f%% of grid)\n",
            n_union, 100 * n_union / n_grid))
cat(sprintf("ES hotspots overlapping driver union: %d (%.2f%% of ES hotspots)\n",
            n_both_union, 100 * n_both_union / n_es))
cat(sprintf("Driver-union base rate among non-ES cells: %.2f%%\n", union_summary$pct_driver_union_among_non_es))
cat(sprintf("Risk ratio: %.3f | Odds ratio: %.3f [%.3f, %.3f] | p = %.3g\n",
            union_summary$risk_ratio, union_summary$odds_ratio,
            union_summary$or_ci_low, union_summary$or_ci_high, union_summary$p_value))
cat("==================================================================\n")
cat("\nOutputs written:\n")
cat(" -", file.path(out_dir, "lcc_driver_magnitude_summary.csv"), "\n")
cat(" -", file.path(out_dir, "lcc_es_hotspot_per_driver_risk.csv"), "\n")
cat(" -", file.path(out_dir, "lcc_es_hotspot_true_union.csv"), "\n")
