# Null model for the ES-hotspot / land-cover-conversion overlap (2026-10-01).
#
# The paper reports that 36.7% of ES hotspot cells overlap the union of the five LCC driver
# hotspots, against a 5.0% rate among non-hotspot cells (risk ratio 7.3). That comparison is
# already a test against global independence: if ES hotspots were placed at random over the grid,
# the expected overlap would be the union's share of the grid (about 9%).
#
# What it does not control for is shared broad geography: both layers concentrate in the same
# regions (tropical forest frontiers), so part of the overlap could come from "both are in the
# tropics" rather than "both are in the same cells". This script computes a stratified null:
# ES hotspots placed at random WITHIN geographic blocks (lat/lon squares of decreasing size),
# keeping each block's number of ES hotspots and of driver-union cells. Expected overlap:
#   E = sum over blocks b of  n_es_b * (n_union_b / n_cells_b)
# with a hypergeometric variance per block (an underestimate under spatial autocorrelation, so
# z-scores are indicative only). The observed / expected ratio says how much co-occurrence
# survives beyond co-location at that scale.
#
# Inputs and the driver-union definition are identical to scripts/compute_attribution_true_union.R
# (same crosswalk deduplication, same top-5% cutoff per driver, same canonical ES hotspot set).
# Cell centroids come from data/processed/10k_change_calc.gpkg (master-grid grid_fid).
#
# Output: outputs/tables/attribution_null_model.csv
# Run from the repo root: Rscript analysis/attribution_null_model.R

suppressPackageStartupMessages({ library(sf); library(dplyr) })
sf_use_s2(FALSE)
source("R/paths.R")

lc_gpkg   <- file.path(data_dir(), "processed", "10k_lcc_granular_metrics.gpkg")
es_gpkg   <- file.path(data_dir(), "processed", "hotspots", "pct", "global", "hotspots_global_pct.gpkg")
grid_gpkg <- file.path(data_dir(), "processed", "10k_change_calc.gpkg")
xw_path   <- file.path(data_dir(), "processed", "lc_grid_fid_to_master_fid_crosswalk.csv")
out_csv   <- file.path("outputs", "tables", "attribution_null_model.csv")

# --- driver union, exactly as compute_attribution_true_union.R ---
drv_query <- paste0(
  "SELECT grid_fid, ",
  "ForestLoss_Loss_Forest_2020_1992 AS Forest_Loss, ",
  "Expansion_Gain_Cropland_2020_1992 AS Crop_Exp, ",
  "Expansion_Gain_Urban_2020_1992 AS Urban_Exp, ",
  "GrasslandLoss_Loss_Grassland_2020_1992 AS Grassland_Loss, ",
  "GrasslandLoss_Gain_Grassland_2020_1992 AS Grassland_Gain ",
  "FROM \"10k_lcc_granular_metrics\""
)
lc <- as.data.frame(st_read(lc_gpkg, query = drv_query, quiet = TRUE)) %>%
  inner_join(read.csv(xw_path) %>% filter(valid_match) %>% select(lc_grid_fid, master_fid, match_dist_m),
             by = c("grid_fid" = "lc_grid_fid")) %>%
  select(-grid_fid) %>% rename(grid_fid = master_fid) %>%
  group_by(grid_fid) %>% slice_min(match_dist_m, n = 1, with_ties = FALSE) %>% ungroup()

drv_cols <- c("Forest_Loss", "Crop_Exp", "Urban_Exp", "Grassland_Loss", "Grassland_Gain")
in_union <- Reduce(`|`, lapply(drv_cols, function(col) {
  x <- lc[[col]]; !is.na(x) & x > quantile(x, 0.95, na.rm = TRUE)
}))
cells <- tibble(grid_fid = lc$grid_fid, union = in_union)

es_fids <- as.data.frame(st_read(es_gpkg, query = "SELECT grid_fid FROM hotspots_global_pct", quiet = TRUE))$grid_fid
cells$es <- cells$grid_fid %in% es_fids

# --- centroids ---
lyr <- st_layers(grid_gpkg)$name[1]
g <- st_read(grid_gpkg, query = sprintf('SELECT grid_fid, geom FROM "%s"', lyr), quiet = TRUE)
xy <- st_coordinates(st_centroid(st_geometry(g)))
cent <- tibble(grid_fid = g$grid_fid, lon = xy[, 1], lat = xy[, 2])
cells <- inner_join(cells, cent, by = "grid_fid")
stopifnot(!anyDuplicated(cells$grid_fid))

n <- nrow(cells); n_es <- sum(cells$es); n_u <- sum(cells$union); obs <- sum(cells$es & cells$union)
message(sprintf("cells %d (with centroid) | ES hotspots %d | union %d | observed overlap %d (%.1f%% of ES)",
                n, n_es, n_u, obs, 100 * obs / n_es))

null_at <- function(block_deg) {
  b <- cells %>%
    mutate(block = if (is.infinite(block_deg)) "global" else
             paste(floor(lon / block_deg), floor(lat / block_deg))) %>%
    group_by(block) %>%
    summarise(nb = n(), es_b = sum(es), u_b = sum(union), .groups = "drop") %>%
    mutate(p = u_b / nb,
           e = es_b * p,
           v = ifelse(nb > 1, es_b * p * (1 - p) * (nb - es_b) / (nb - 1), 0))
  E <- sum(b$e); V <- sum(b$v)
  tibble(block_deg = block_deg, n_blocks = nrow(b),
         observed_overlap = obs, expected_overlap = E,
         observed_pct_of_es = 100 * obs / n_es, expected_pct_of_es = 100 * E / n_es,
         ratio_obs_exp = obs / E, z_indicative = (obs - E) / sqrt(V))
}

res <- bind_rows(lapply(c(Inf, 10, 5, 2, 1, 0.5), null_at))
dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
write.csv(res, out_csv, row.names = FALSE)
print(as.data.frame(res), digits = 4, row.names = FALSE)
