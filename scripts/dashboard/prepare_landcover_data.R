# Land cover change on the explorer's 10 km grid (scripts/dashboard/app.R, "Land cover change" tab), 2026-11-03.
#
# The two land cover change metric sets (ESA CCI 1992 -> C3S 2020, 9 classes, diffeR metrics in % of each
# cell; analysis/LC_change.qmd and analysis/LC_change_granular.qmd, definitions in
# analysis/LCC_column_dictionary.md) are on the legacy 10 km grid. They reach the explorer's cells through
# the spatial crosswalk lc_grid_fid_to_master_fid_crosswalk.csv (valid matches only).
#
# Kept per cell, for natural land (binary natural vs transformed) and for forest, grassland/shrubland,
# cropland and urban (granular models): persistence, loss and gain, as % of the cell. A class absent from
# a cell in both years is stored as NA by diffeR and means 0 here. Quantity, exchange and shift are left out.
# Values are stored as integers in tenths of a percent (0-1000) to keep the app's memory small.
#
# Usage: Rscript scripts/dashboard/prepare_landcover_data.R   (after prepare_dashboard_data.R)
# Writes: data/processed/dashboard/landcover_grid.rds (one row per row of grid_dashboard.rds, same order)

suppressPackageStartupMessages({ library(sf); library(dplyr); library(here) })
setwd(here())

grid <- readRDS("data/processed/dashboard/grid_dashboard.rds")
xw <- read.csv("data/processed/lc_grid_fid_to_master_fid_crosswalk.csv") |> filter(valid_match)

read_cols <- function(file, layer, cols) {
  st_read(file, query = sprintf('SELECT grid_fid, %s FROM "%s"', paste(cols, collapse = ", "), layer),
          quiet = TRUE)
}
yr <- "_2020_1992"

# natural land: class 1 = transformed (cropland, urban), class 2 = natural (all other classes)
bin <- read_cols("data/processed/10k_lcc_metrics.gpkg", "10k_lcc_metrics",
                 paste0(c("Persistence_2", "Loss_2", "Gain_2", "Loss_1"), yr))
# a closed binary system: natural land gained = transformed land lost
stopifnot(isTRUE(all.equal(coalesce(bin[[paste0("Gain_2", yr)]], 0), coalesce(bin[[paste0("Loss_1", yr)]], 0),
                           tolerance = 1e-3)))

gran_spec <- c(forest = "ForestLoss_%s_Forest", grassland = "GrasslandLoss_%s_Grassland",
               cropland = "Expansion_%s_Cropland", urban = "Expansion_%s_Urban")
gran_cols <- unlist(lapply(gran_spec, function(p) paste0(sprintf(p, c("Persistence", "Loss", "Gain")), yr)))
gran <- read_cols("data/processed/10k_lcc_granular_metrics.gpkg", "10k_lcc_granular_metrics", gran_cols)

lc <- data.frame(lc_fid = bin$grid_fid,
                 natural_p = bin[[paste0("Persistence_2", yr)]], natural_l = bin[[paste0("Loss_2", yr)]],
                 natural_g = bin[[paste0("Gain_2", yr)]])
gi <- match(lc$lc_fid, gran$grid_fid)
for (k in names(gran_spec)) for (m in c("Persistence", "Loss", "Gain")) {
  lc[[paste0(k, "_", tolower(substr(m, 1, 1)))]] <- gran[[paste0(sprintf(gran_spec[[k]], m), yr)]][gi]
}
# cells with any land cover value: absent classes are 0
has_lc <- rowSums(!is.na(lc[-1])) > 0
val <- names(lc)[-1]
lc[has_lc, val] <- lapply(lc[has_lc, val], function(v) ifelse(is.na(v), 0, v))
# every cell's own composition adds up: 1992 share = persistence + loss, 2020 share = persistence + gain
tot92 <- with(lc[has_lc, ], forest_p + forest_l + grassland_p + grassland_l + cropland_p + cropland_l + urban_p + urban_l)
stopifnot(max(tot92) <= 100.5)

# to the explorer's cells
lc$master_fid <- xw$master_fid[match(lc$lc_fid, xw$lc_grid_fid)]
lc <- lc[has_lc & !is.na(lc$master_fid) & !duplicated(lc$master_fid), ]
out <- lc[match(grid$grid_fid, lc$master_fid), val]
out[] <- lapply(out, function(v) as.integer(round(v * 10)))
rownames(out) <- NULL   # the row selection above leaves character row names (doubles the size)
message("explorer cells with land cover values: ", sum(!is.na(out$natural_p)), " of ", nrow(grid),
        sprintf(" (%.1f%%)", 100 * mean(!is.na(out$natural_p))))
saveRDS(out, "data/processed/dashboard/landcover_grid.rds", compress = FALSE)
message("wrote data/processed/dashboard/landcover_grid.rds: ", format(object.size(out), units = "MB"))
