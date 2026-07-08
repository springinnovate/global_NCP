# Builds a crosswalk from 10k_lcc_granular_metrics.gpkg's own `grid_fid` (a
# row-index into its source grid, AOOGrid_10x10km_land_4326_clean.gpkg,
# 1,691,819 cells -- that file no longer exists on disk) to the master-grid
# `fid` used everywhere else in the pipeline (row-index into
# landgrid_1_clean_enriched_4326.gpkg, 1,522,073 cells).
#
# These are two different grid exports of the same underlying 10km
# tessellation (identical bbox/CRS), so a nearest-centroid match recovers the
# true correspondence: ~99.4% of cells match at ~0m (same cell), the
# remainder have no counterpart in the master grid (excluded/clipped
# differently) and are marked invalid rather than force-matched.
#
# Run once; downstream scripts (compute_attribution_true_union.R,
# analysis/hotspot_extraction.qmd's LC driver export) join through this
# crosswalk instead of trusting `grid_fid` equality directly.

library(sf)
library(dplyr)

source("R/paths.R")

sf::sf_use_s2(FALSE)
t0 <- Sys.time()

message("Reading master grid...")
master_sf <- st_read(file.path(data_dir(), "vector_basedata", "landgrid_1_clean_enriched_4326.gpkg"), quiet = TRUE)
master_sf$master_fid <- seq_len(nrow(master_sf))
master_pts <- suppressWarnings(st_centroid(st_geometry(master_sf)))
master_coords <- st_coordinates(master_pts)
master_pts_sf <- st_sf(master_fid = master_sf$master_fid, geometry = master_pts)
rm(master_sf)
message("Master centroids ready: ", nrow(master_pts_sf), " (", round(as.numeric(Sys.time() - t0, units = "secs")), "s)")

message("Reading LCC granular grid...")
lc_sf <- st_read(file.path(data_dir(), "processed", "10k_lcc_granular_metrics.gpkg"), quiet = TRUE)
lc_pts <- suppressWarnings(st_centroid(st_geometry(lc_sf)))
lc_coords <- st_coordinates(lc_pts)
lc_pts_sf <- st_sf(grid_fid = lc_sf$grid_fid, geometry = lc_pts)
rm(lc_sf)
gc()
message("LC centroids ready: ", nrow(lc_pts_sf), " (", round(as.numeric(Sys.time() - t0, units = "secs")), "s)")

message("Running nearest-feature match...")
t1 <- Sys.time()
nn <- st_nearest_feature(lc_pts_sf, master_pts_sf)
message("Nearest-feature match done in ", round(as.numeric(Sys.time() - t1, units = "secs")), "s")

# Manual planar-approx distance (s2 is off; avoids a lwgeom dependency for a
# sanity-check distance only used to flag non-matches)
m_coords <- master_coords[nn, , drop = FALSE]
dlat <- (lc_coords[, 2] - m_coords[, 2]) * 111320
dlon <- (lc_coords[, 1] - m_coords[, 1]) * 111320 * cos(lc_coords[, 2] * pi / 180)
dist_m <- sqrt(dlat^2 + dlon^2)

crosswalk <- tibble::tibble(
  lc_grid_fid  = lc_pts_sf$grid_fid,
  master_fid   = master_pts_sf$master_fid[nn],
  match_dist_m = dist_m
) %>%
  mutate(valid_match = match_dist_m < 6000) # within ~one 10km cell width

message("Total rows: ", nrow(crosswalk))
message("Valid matches (<6km): ", sum(crosswalk$valid_match), " (", round(100 * mean(crosswalk$valid_match), 2), "%)")
message("No true correspondence: ", sum(!crosswalk$valid_match), " (", round(100 * mean(!crosswalk$valid_match), 2), "%)")

out_path <- file.path(data_dir(), "processed", "lc_grid_fid_to_master_fid_crosswalk.csv")
readr::write_csv(crosswalk, out_path)
message("Crosswalk written to: ", out_path)
message("Total time: ", round(as.numeric(Sys.time() - t0, units = "secs")), "s")
