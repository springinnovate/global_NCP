# ==============================================================================
# Colombia Report Maps — CLEC / Sandra Valenzuela materials (2026-08-11/12)
# ==============================================================================
#
# Builds the map set for the Colombia-scoped hotspot/beneficiary-exposure
# report: per-service raw pixel change (5 headline services), compound
# hotspot map (5-service definition), and a beneficiary-reach map (combined
# cross-category union coverage + population). Colombia has only 11,378
# cells, so this plots directly off the vector grid (geom_sf) rather than
# the raster-rasterize optimization make_native_change_figure.R needs at
# global scale (1.37M cells) -- unnecessary complexity at this extent.
#
# Uses the corrected hotspot_area_stats_Colombia.csv (post Coastal Risk
# denominator fix, WORKLOG.md 2026-08-11) for anything numeric; this script
# only produces maps.
# ==============================================================================

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)
library(here)

source(here("R", "paths.R"))

out_dir <- here("outputs", "plots", "colombia_report")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# WWF brand palette (docs/presentations/wwf_theme.css)
wwf_green      <- "#007930"
wwf_dark_green <- "#004D1E"
wwf_orange     <- "#F07D00"
wwf_teal       <- "#009191"
wwf_yellow     <- "#F5D200"

# ------------------------------------------------------------------------------
# 1. Load Colombia-filtered change grid
# ------------------------------------------------------------------------------

message("Loading 10k_change_calc.gpkg, filtering to Colombia...")
grid_full <- st_read(here("data", "processed", "10k_change_calc.gpkg"), quiet = TRUE)
grid_col  <- grid_full %>% filter(nev_name == "Colombia")
message("  -> ", nrow(grid_col), " cells")

colombia_boundary <- st_union(grid_col)
col_bbox <- st_bbox(grid_col)

# ------------------------------------------------------------------------------
# 2. Per-service raw pixel change panels (5 headline services)
# ------------------------------------------------------------------------------

svc_defs <- list(
  list(service = "Pollination",   col = "pollination_pct_chg",   direction = "good"),
  list(service = "Sed_export",    col = "sed_export_pct_chg",    direction = "damage"),
  list(service = "N_export",      col = "n_export_pct_chg",      direction = "damage"),
  list(service = "Nature_Access", col = "nature_access_pct_chg", direction = "good"),
  list(service = "C_Risk",        col = "c_risk_pct_chg",        direction = "damage")
)

get_color_scale <- function(direction, limits) {
  if (direction == "good") {
    scale_fill_gradient2(low = wwf_orange, mid = "white", high = wwf_teal,
                          midpoint = 0, limits = limits, name = "% change", na.value = "gray95")
  } else {
    scale_fill_gradient2(low = wwf_teal, mid = "white", high = wwf_orange,
                          midpoint = 0, limits = limits, name = "% change", na.value = "gray95")
  }
}

make_change_panel <- function(d) {
  vals <- grid_col[[d$col]]
  q <- quantile(vals, c(0.01, 0.99), na.rm = TRUE)
  lim <- max(abs(q), na.rm = TRUE)
  limits <- c(-lim, lim)

  gdf <- grid_col %>%
    mutate(val = pmax(pmin(.data[[d$col]], limits[2]), limits[1]))

  ggplot(gdf) +
    geom_sf(data = colombia_boundary, fill = "gray97", color = "gray80", linewidth = 0.2) +
    geom_sf(aes(fill = val), color = NA) +
    get_color_scale(d$direction, limits) +
    labs(title = gsub("_", " ", d$service)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, color = wwf_dark_green),
      legend.position = "bottom",
      legend.key.width = unit(0.9, "cm"),
      legend.key.height = unit(0.25, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7)
    )
}

message("Building per-service change panels...")
panels <- lapply(svc_defs, make_change_panel)
names(panels) <- sapply(svc_defs, function(d) d$service)

for (nm in names(panels)) {
  fp <- file.path(out_dir, paste0("colombia_change_", tolower(nm), ".png"))
  ggsave(fp, panels[[nm]], width = 5.5, height = 6.5, dpi = 200, bg = "white")
  message("  Saved: ", fp)
}

combined_change <- wrap_plots(panels, ncol = 3) +
  plot_annotation(
    title = "Colombia — Ecosystem Service Change, 1992–2020",
    subtitle = "Percentage change per 10km grid cell (5 tracked services)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = wwf_dark_green),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30")
    )
  )
ggsave(file.path(out_dir, "colombia_change_5panel.png"), combined_change,
       width = 16, height = 11, dpi = 200, bg = "white")
message("Saved combined change panel.")

# ------------------------------------------------------------------------------
# 3. Compound hotspot map (5-service definition, recomputed from binary flags)
# ------------------------------------------------------------------------------

message("Building hotspot map...")
hs <- st_read(here("data", "processed", "hotspots", "pct", "nev_name",
                    "hotspots_nev_name_Colombia_pct.gpkg"), quiet = TRUE)

headline_svcs <- c("Pollination", "Sed_export", "N_export", "Nature_Access", "C_Risk")

hs5 <- hs %>%
  mutate(hotspot_count_5svc = rowSums(across(all_of(headline_svcs)), na.rm = TRUE)) %>%
  filter(hotspot_count_5svc > 0) %>%
  mutate(
    hotspot_capped = pmin(hotspot_count_5svc, 4),
    hotspot_label = factor(hotspot_capped, levels = 1:4, labels = c("1", "2", "3", "4–5"))
  )
message("  -> ", nrow(hs5), " hotspot cells (>=1 of 5 headline services)")

ramp <- colorRampPalette(c("#FFE0B2", wwf_orange, "#B85C00", wwf_dark_green))(4)
names(ramp) <- c("1", "2", "3", "4–5")

p_hs <- ggplot() +
  geom_sf(data = colombia_boundary, fill = "gray97", color = "gray70", linewidth = 0.3) +
  geom_sf(data = hs5, aes(fill = hotspot_label), color = NA) +
  scale_fill_manual(name = "Overlapping\nES hotspots", values = ramp, na.value = "gray90", drop = FALSE) +
  labs(
    title = "Colombia — Compound Ecosystem Service Hotspots",
    subtitle = "Cells with extreme decline in ≥1 of 5 tracked services, 1992–2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = wwf_dark_green),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )
ggsave(file.path(out_dir, "colombia_hotspot_map.png"), p_hs, width = 9, height = 10, dpi = 200, bg = "white")
message("Saved hotspot map.")

# ------------------------------------------------------------------------------
# 4. Beneficiary-reach map (combined cross-category union coverage + population)
# ------------------------------------------------------------------------------

message("Building beneficiary-reach map...")
ben_dir <- file.path(data_dir(), "processed", "hotspots_5service", "rasters_5_var",
                      "output_jeronimo_2026_07_29_18_49_00_combined_cross_category_beneficiaries")
coverage_tif <- file.path(ben_dir, "full_raster_extent_union_coverage.tif")
population_tif <- file.path(ben_dir, "full_raster_extent_union_population.tif")

if (file.exists(coverage_tif)) {
  col_bbox_buffered <- col_bbox
  # small buffer so edge-of-country buffer reach isn't clipped
  col_bbox_buffered[c("xmin","ymin")] <- col_bbox_buffered[c("xmin","ymin")] - 1
  col_bbox_buffered[c("xmax","ymax")] <- col_bbox_buffered[c("xmax","ymax")] + 1
  ext_col <- ext(col_bbox_buffered["xmin"], col_bbox_buffered["xmax"],
                  col_bbox_buffered["ymin"], col_bbox_buffered["ymax"])

  r_cov <- rast(coverage_tif)
  r_cov_col <- crop(r_cov, ext_col)

  df_cov <- as.data.frame(r_cov_col, xy = TRUE)
  names(df_cov)[3] <- "coverage"
  df_cov <- df_cov %>% filter(coverage > 0)

  p_ben <- ggplot() +
    geom_sf(data = colombia_boundary, fill = "gray97", color = "gray70", linewidth = 0.3) +
    geom_raster(data = df_cov, aes(x = x, y = y), fill = wwf_teal, alpha = 0.55) +
    geom_sf(data = hs5, aes(color = hotspot_label), fill = NA, size = 0.05, show.legend = FALSE) +
    scale_color_manual(values = ramp) +
    coord_sf(xlim = c(col_bbox["xmin"], col_bbox["xmax"]),
             ylim = c(col_bbox["ymin"], col_bbox["ymax"]), expand = FALSE) +
    labs(
      title = "Colombia — Beneficiary Reach",
      subtitle = "Teal: within 50km downstream or 1hr travel-time of a combined-cross-category hotspot\n(reaches 75.8% of Colombia's population, 38.1M people)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = wwf_dark_green),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30")
    )
  ggsave(file.path(out_dir, "colombia_beneficiary_map.png"), p_ben, width = 9, height = 10.5, dpi = 200, bg = "white")
  message("Saved beneficiary-reach map.")
} else {
  message("  SKIP beneficiary map: ", coverage_tif, " not found")
}

message("\nDone. All Colombia report maps saved to: ", out_dir)
