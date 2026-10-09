# Small, legend-free world maps of the input layers for the lab deck's "Data Inputs" slide
# (2026-10-08); scripts/mapping/make_deck_input_stacks.py then draws them as stacks of rasters.
#
# - Services, 2020: the native InVEST rasters (data/raw/base_years), read at ~0.2 degrees.
# - Land cover, 1992 and 2020: dominant class per 10 km cell, rebuilt from the land cover change
#   shares in 10k_lcc_granular_metrics.gpkg (class in year = persistence + loss for 1992,
#   persistence + gain for 2020), joined to the master grid through the LC crosswalk.
# - Socioeconomic: the 10 km grid columns (10k_change_calc.gpkg).
# Everything is drawn in Equal Earth with the explorer's pixel lookup (data/processed/dashboard).
#
# Usage: Rscript scripts/mapping/make_deck_input_layers.R
# Writes: docs/presentations/images/input_layers/*.png

suppressPackageStartupMessages({ library(terra); library(sf); library(here) })

out_dir <- here("docs", "presentations", "images", "input_layers")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
W <- 1400; H <- 640
ylim <- c(-6.3e6, 8.4e6); xlim <- c(-1.32e7, 1.46e7)
plate <- "#F2F2F2"

layers <- readRDS(here("data", "processed", "dashboard", "change_map_layers.rds"))
d <- readRDS(here("data", "processed", "dashboard", "grid_dashboard.rds"))
px <- layers$pix$world
base <- layers$base

# rank stretch: colour by percentile so skewed layers still show their pattern
stretch <- function(v) { r <- rank(v, na.last = "keep", ties.method = "average"); r / max(r, na.rm = TRUE) }

draw_png <- function(file, x, y, col) {
  png(file.path(out_dir, file), width = W, height = H, bg = plate)
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  plot(NA, xlim = xlim, ylim = ylim, asp = 1, axes = FALSE, xlab = "", ylab = "")
  plot(st_geometry(base), col = "#E2E2E2", border = NA, add = TRUE)
  points(x, y, pch = 15, cex = 0.32, col = col)
  plot(st_geometry(base), col = NA, border = "#BDBDBD", lwd = 0.3, add = TRUE)
  dev.off()
  message("wrote ", file)
}

# grid column -> colours on the Equal Earth pixels
draw_grid <- function(file, v, pal, log = FALSE) {
  vv <- if (log) log1p(v) else v
  s <- stretch(vv)
  cols <- pal(100)[pmax(1, ceiling(s * 100))]
  k <- !is.na(s[px$row]) & (if (log) v[px$row] > 0 else TRUE)
  k[is.na(k)] <- FALSE
  draw_png(file, px$x[k], px$y[k], cols[px$row[k]])
}

# ---- services (native rasters, 2020) -------------------------------------------------------------
svc <- list(
  n_export = c("global_n_export_tnc_esa2020_compressed_md5_1d3c17-007.tif", "YlOrBr"),
  sed_export = c("global_sed_export_marine_mod_ESA_2020_compressed_md5_a988c0-002.tif", "Oranges"),
  pollination = c("realized_polllination_on_ag_ESA2020mar_md5_da610a.tif", "YlGn"),
  nature_access = c("nature_access_lspop2019_ESA2020_compressed_md5_6727ac.tif", "Blues")
)
for (s in names(svc)) {
  r <- rast(here("data", "raw", "base_years", svc[[s]][1]))
  smp <- spatSample(r, 1800 * 700, method = "regular", as.raster = TRUE)
  ee <- project(smp, "EPSG:8857", method = "near")
  xy <- as.data.frame(ee, xy = TRUE, na.rm = TRUE)
  names(xy)[3] <- "v"
  xy <- xy[xy$v > 0, ]
  st <- stretch(log1p(xy$v))
  pal <- hcl.colors(100, svc[[s]][2], rev = TRUE)
  draw_png(paste0("svc_", s, ".png"), xy$x, xy$y, pal[pmax(1, ceiling(st * 100))])
}

# ---- socioeconomic (10 km grid) -------------------------------------------------------------------
soc_cols <- c("grid_fid", "GHS_POP_E2020_GLOBE_sum", "rast_gdpTot_1990_2020_30arcsec_2020_sum",
              "hdi_raster_predictions_2020_mean", "rast_adm1_gini_disp_2020_mean")
soc <- st_read(here("data", "processed", "10k_change_calc.gpkg"),
               query = sprintf('SELECT %s FROM "10k_change_calc"', paste(soc_cols, collapse = ", ")), quiet = TRUE)
soc <- soc[match(d$grid_fid, soc$grid_fid), ]
draw_grid("soc_population.png", soc$GHS_POP_E2020_GLOBE_sum, function(n) hcl.colors(n, "Reds 3", rev = TRUE), log = TRUE)
draw_grid("soc_gdp.png", soc$rast_gdpTot_1990_2020_30arcsec_2020_sum, function(n) hcl.colors(n, "Purples 3", rev = TRUE), log = TRUE)
draw_grid("soc_hdi.png", soc$hdi_raster_predictions_2020_mean, function(n) hcl.colors(n, "Teal", rev = TRUE))
draw_grid("soc_gini.png", soc$rast_adm1_gini_disp_2020_mean, function(n) hcl.colors(n, "Burg", rev = TRUE))

# ---- land cover, dominant class 1992 and 2020 -----------------------------------------------------
lcc_cols <- c("grid_fid",
              paste0("ForestLoss_", c("Persistence", "Loss", "Gain"), "_Forest_2020_1992"),
              paste0("Expansion_", c("Persistence", "Loss", "Gain"), "_Cropland_2020_1992"),
              paste0("Expansion_", c("Persistence", "Loss", "Gain"), "_Urban_2020_1992"),
              paste0("GrasslandLoss_", c("Persistence", "Loss", "Gain"), "_Grassland_2020_1992"))
lcc <- st_read(here("data", "processed", "10k_lcc_granular_metrics.gpkg"),
               query = sprintf('SELECT %s FROM "10k_lcc_granular_metrics"', paste(lcc_cols, collapse = ", ")),
               quiet = TRUE)
cw <- read.csv(here("data", "processed", "lc_grid_fid_to_master_fid_crosswalk.csv"))
cw <- cw[cw$valid_match, ]
lcc$master_fid <- cw$master_fid[match(lcc$grid_fid, cw$lc_grid_fid)]
lcc <- lcc[match(d$grid_fid, lcc$master_fid), ]
z <- function(v) ifelse(is.na(v), 0, v)
cls <- c("Forest", "Cropland", "Urban", "Grassland")
lc_cols <- c(Forest = "#2E7D32", Cropland = "#E9C46A", Urban = "#C62828", Grassland = "#A7C957", Other = "#CDBFA6")
for (yr in c("1992", "2020")) {
  add <- if (yr == "1992") "Loss" else "Gain"
  share <- sapply(c(Forest = "ForestLoss_%s_Forest", Cropland = "Expansion_%s_Cropland",
                    Urban = "Expansion_%s_Urban", Grassland = "GrasslandLoss_%s_Grassland"), function(p)
    z(lcc[[paste0(sprintf(p, "Persistence"), "_2020_1992")]]) + z(lcc[[paste0(sprintf(p, add), "_2020_1992")]]))
  other <- pmax(0, 100 - rowSums(share))
  dom <- c(cls, "Other")[max.col(cbind(share, other), ties.method = "first")]
  dom[is.na(lcc$master_fid)] <- NA
  k <- !is.na(dom[px$row])
  draw_png(paste0("lc_", yr, ".png"), px$x[k], px$y[k], lc_cols[dom[px$row[k]]])
}
