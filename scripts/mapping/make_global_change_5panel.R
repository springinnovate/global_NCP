# ==============================================================================
# Global — 5-Panel Change Figure (headline services, deck-matching style)
# ==============================================================================
#
# Companion to make_colombia_report_maps.R's colombia_change_5panel.png --
# same 5 headline services, same layout/palette/title style, but at global
# scale, for a "same pipeline, global then Colombia" slide pair in the
# Sandra Valenzuela deck. Reuses the rasterize+geom_tile performance
# approach from make_native_change_figure.R (~15s/panel vs. ~145s for a
# full-grid geom_sf render) since this runs at 1.37M cells, not Colombia's
# ~11K.
#
# Builds panels once, then composes TWO title variants (ES/EN) from the
# same cached panels -- the expensive step (rasterize) only runs once.
#
# Output moved 2026-09-02 from outputs/plots/colombia_report/ to outputs/plots/maps/ -- that
# directory is genuinely Colombia-scoped (7+ make_colombia_*.R scripts write there), and this
# figure now does double duty as the paper's main "Global Pattern of Change" figure, not just a
# Sandra-deck companion panel. outputs/plots/maps/ already holds the other global-scale map
# outputs (global_hotspot_count_heatmap_*, global_attribution_gap_map_*) -- same naming
# convention, better fit. Still referenced by the Sandra deck alongside colombia_report's own
# colombia_change_5panel.png, just from the new path.
# ==============================================================================

library(sf)
library(terra)
library(ggplot2)
library(dplyr)
library(patchwork)
library(here)

source(here("R", "paths.R"))
source(here("R", "service_config.R"))

out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

wwf_green      <- "#007930"
wwf_dark_green <- "#004D1E"
wwf_orange     <- "#F07D00"
wwf_teal       <- "#009191"

# 5-service redesign (2026-08-28): swapped from export/risk residuals to retention/protection
# amounts, per Steve's clarification (settled 2026-08-31). All remaining services are uniformly
# "good" direction -- an increase is always the favorable direction, matching the paper's Methods
# text ("All five are framed consistently as an amount of benefit provided").
# Fixed 2026-08-31 after finding this script (unlike make_native_change_figure.R, which is a
# deliberately separate all-8-service reference figure) was the actual generator behind the paper's
# "Global Pattern of Change" figure and had drifted to the old export/risk names.
#
# Coastal protection deliberately dropped from this panel, 2026-09-01 (user decision): the data is
# correct (49,291 valid cells, real signal) but coastal protection only exists on a 1-cell-wide
# coastline fringe, invisible at full-globe map scale next to the other services' continental
# footprints -- confirmed by diagnostic, not a rendering bug. Noted in the panel count/caption
# rather than shown blank. Regional zoom insets for high-hotspot-concentration coastlines are a
# possible Annex addition, not yet built.
# Built from R/service_config.R's SERVICE_AMOUNTS (single source of truth for name/column/
# direction) rather than hardcoded -- only the display label and the coastal exclusion (see
# note above) are specific to this figure.
panel_labels <- c(
  Pollination    = "Pollination",
  Sed_retention  = "Sediment retention",
  N_retention    = "Nitrogen retention",
  Nature_Access  = "Nature Access"
)

# 2026-09-02: added an absolute-change variant alongside the existing percentage-change one, to
# compare which reads better for this specific figure -- this map is per-cell, not aggregated to
# region/biome/income, so the Simpson's-paradox/MAUP sign-flip risk that justifies preferring SPC
# for REGIONAL summaries elsewhere in the paper doesn't apply here (see docs/methodology.md).
# Native units per service for the absolute legend (not tracked in R/service_config.R since no
# other consumer needs them):
abs_units <- c(
  N_retention   = "kg N/ha/yr",
  Sed_retention = "t/ha/yr",
  Pollination   = "people-fed equiv./ha",
  Nature_Access = "access index"
)

build_svc_defs <- function(metric) {
  col_suffix <- if (metric == "pct") "_pct_chg" else "_abs_chg"
  lapply(Filter(function(s) s$name != "C_Prot_service", SERVICE_AMOUNTS), function(s) {
    list(
      service     = s$name,
      col         = paste0(s$col_prefix, col_suffix),
      direction   = if (s$good_direction == "high") "good" else "damage",
      label       = panel_labels[[s$name]],
      legend_name = if (metric == "pct") "% change" else abs_units[[s$name]]
    )
  })
}

get_color_scale <- function(direction, limits, legend_name) {
  if (direction == "good") {
    scale_fill_gradient2(low = wwf_orange, mid = "white", high = wwf_teal,
                          midpoint = 0, limits = limits, name = legend_name, na.value = NA)
  } else {
    scale_fill_gradient2(low = wwf_teal, mid = "white", high = wwf_orange,
                          midpoint = 0, limits = limits, name = legend_name, na.value = NA)
  }
}

# ------------------------------------------------------------------------------
# 1. Load, filter, and reproject the global grid ONCE
# ------------------------------------------------------------------------------

svc_defs_pct <- build_svc_defs("pct")
svc_defs_abs <- build_svc_defs("abs")

message("Loading 10k_change_calc.gpkg (global)...")
needed_cols <- c("grid_fid", "continent", "WWF_biome",
                  sapply(svc_defs_pct, function(d) d$col),
                  sapply(svc_defs_abs, function(d) d$col))
grid <- st_read(here("data", "processed", "10k_change_calc.gpkg"), quiet = TRUE)
grid <- grid[, intersect(names(grid), c(needed_cols, "geom", "geometry"))]

grid <- grid %>%
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)"),
         !WWF_biome %in% c("Lakes", "Rock & Ice"))
message("  -> ", nrow(grid), " cells after exclusion")

grid <- st_transform(grid, "EPSG:8857")
vect_grid <- vect(grid)
r_template <- rast(vect_grid, resolution = 10000)

base_map_path <- file.path(data_dir(), "vector_basedata", "cartographic_ee_r264_correspondence.gpkg")
base_sf <- st_read(base_map_path, quiet = TRUE) %>% st_transform("EPSG:8857")

compute_limits <- function(val_vec) {
  q <- quantile(val_vec, c(0.01, 0.99), na.rm = TRUE)
  lim <- max(abs(q))
  c(-lim, lim)
}

# ------------------------------------------------------------------------------
# 2. Rasterize + plot each panel (built once, reused for both title variants)
# ------------------------------------------------------------------------------

make_panel <- function(d) {
  message("  -> ", d$service)
  limits <- compute_limits(grid[[d$col]])

  r <- terra::rasterize(vect_grid, r_template, field = d$col)
  df_r <- as.data.frame(r, xy = TRUE)
  names(df_r)[3] <- "value"
  df_r$value <- pmax(pmin(df_r$value, limits[2]), limits[1])

  fade_threshold <- max(0.08 * limits[2], 1e-9)
  df_r$alpha <- pmin(abs(df_r$value) / fade_threshold, 1)

  ggplot() +
    geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
    geom_tile(data = df_r, aes(x = x, y = y, fill = value, alpha = alpha)) +
    scale_alpha_identity(guide = "none") +
    get_color_scale(d$direction, limits, d$legend_name) +
    coord_sf(crs = "EPSG:8857") +
    labs(title = d$label) +
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

message("Building global panels (pct change)...")
panels_pct <- lapply(svc_defs_pct, make_panel)
names(panels_pct) <- sapply(svc_defs_pct, function(d) d$service)

message("Building global panels (abs change)...")
panels_abs <- lapply(svc_defs_abs, make_panel)
names(panels_abs) <- sapply(svc_defs_abs, function(d) d$service)

# ------------------------------------------------------------------------------
# 3. Compose ES + EN title variants from the same cached panels
# ------------------------------------------------------------------------------

# Title/subtitle removed 2026-09-02: these figures are always embedded with an external Quarto
# figure caption in the paper (and this pair is now two standalone figures there, not one
# 2-panel block), so an in-image title just duplicated it and ate vertical space -- same fix
# already applied to the hotspot count map, intensity charts, and boxplots.
compose_and_save <- function(panels, out_path) {
  combined <- wrap_plots(panels, ncol = 2)
  ggsave(out_path, combined, width = 16, height = 11, dpi = 200, bg = "white", limitsize = FALSE)
  message("Saved: ", out_path)
}

compose_and_save(panels_pct, file.path(out_dir, "global_change_5panel_es.png"))
compose_and_save(panels_pct, file.path(out_dir, "global_change_5panel_en.png"))
compose_and_save(panels_abs, file.path(out_dir, "global_change_5panel_abs_es.png"))

compose_and_save(panels_abs, file.path(out_dir, "global_change_5panel_abs_en.png"))

message("\nDone.")
