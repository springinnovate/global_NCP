# ==============================================================================
# Colombia — Per-Service Critical Natural Asset Maps
# ==============================================================================
#
# make_colombia_report_maps.R already builds a 5-panel "raw change per
# service" figure (colombia_change_5panel.png) as a visual lead-in to the
# hotspot-extraction step. This script builds the equivalent for the
# "value" side of the deck: 5 individual critical-natural-asset maps (one
# per headline service, from the paper's own single-NCP prioritizr
# solutions — see data/external/critical_natural_assets/
# per_service_solutions/README.md for the letter-code mapping) plus a
# combined 5-panel figure, as a lead-in to the aggregate "Oportunidad"
# slide the same way the change panel leads into "Dónde".
#
# Solutions are binary (critical/not, 90% target only) in Eckert IV —
# trimmed, reprojected (nearest-neighbor, to preserve the binary
# classification), then zonal-extracted onto this project's 10km grid
# using the same >=50% majority rule as make_colombia_per_service_overlap.R,
# for visual/resolution consistency with the rest of the deck's maps.
#
# A flat binary fill flattens real variation within the "critical" cells —
# same lesson already learned and fixed for the aggregate CNA map in
# make_colombia_priority_overlap.R, which modulates fill opacity by
# continuous priority rank 1-20. The per-service prioritizr solutions have no
# equivalent continuous rank (Extended Data Table 5: single-NCP scenarios
# were only solved at the 90% target, not the full 5-100% budget range that
# produces the aggregate raster's rank). So instead opacity within the
# critical (green) cells is modulated using the continuous "realized"
# per-service magnitude layers (individual_layers/*.tif) — a disclosable
# difference from the aggregate map's technique (magnitude of provision, not
# optimization priority rank), not a substitute pretending to be the same
# thing. Nature access has no 1:1 realized-layer match (the solution is
# "within 1hr, no rural/urban split" but realized layers split 4 ways
# rural/urban x 60/360 min) — approximated as mean(rural_60, urban_60), the
# closest match to the solution's 1hr framing; disclosed in the panel caption.
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

wwf_green      <- "#007930"
wwf_dark_green <- "#004D1E"

solutions_dir <- file.path(data_dir(), "external", "critical_natural_assets", "per_service_solutions")
layers_dir    <- file.path(data_dir(), "external", "critical_natural_assets", "individual_layers")

svc_defs <- list(
  list(label = "Polinización",              file = "solution_B1_pollination_col_target-90.tif",             slug = "pollination",
       mag_files = "realized_pollination_norm_nathab_clamped_WARPED_average_MASKED_md5_0b1c860775d3e917459b53aeda637a62.tif"),
  list(label = "Retención de sedimentos",    file = "solution_D1_sediment_retention_500km_col_target-90.tif", slug = "sediment",
       mag_files = "realized_sedimentdeposition_attn_500km_WARPED_average_MASKED_md5_4976bc9d25d324871f47c2f53e578af7.tif"),
  list(label = "Retención de nitrógeno",     file = "solution_A1_nitrogen_retention_500km_col_target-90.tif", slug = "nitrogen",
       mag_files = "realized_nitrogenretention_attn_500km_WARPED_average_MASKED_md5_ca56502377df465a80295c60b819693c.tif"),
  list(label = "Acceso a la naturaleza",     file = "solution_Z_nature_access_1hr_col_target-90.tif",         slug = "nature_access",
       mag_files = c("realized_norm_nature_access_lspop_2017_URCA_rural_60_WARPED_average_MASKED_md5_0883b8119cfa48637a320a7bdfc4ef9d.tif",
                      "realized_norm_nature_access_lspop_2017_URCA_urban_60_WARPED_average_MASKED_md5_df1be9127612ff3c19817ef0c235c03e.tif")),
  list(label = "Protección costera",         file = "solution_S_coastal_risk_reduction_col_target-90.tif",    slug = "coastal",
       mag_files = "realized_coastalprotection_norm_WARPED_average_md5_ca4d5414b3153d38bba26ad4d72f566b_ovr.tif")
)

# Nature access magnitude = mean(rural_60, urban_60), the closest available
# match to the solution's "within 1hr, no rural/urban split" definition
# (decided 2026-08-20; the 360-min variants don't match the 1hr threshold).

# ------------------------------------------------------------------------------
# 1. Colombia grid
# ------------------------------------------------------------------------------

message("Loading Colombia grid...")
grid_col <- st_read(here("data", "processed", "10k_change_calc.gpkg"), quiet = TRUE) %>%
  filter(nev_name == "Colombia") %>%
  select(grid_fid, geom)
colombia_boundary <- st_union(grid_col)
grid_col_vect <- vect(grid_col)

# ------------------------------------------------------------------------------
# 2. Per-service zonal extraction + individual panels
# ------------------------------------------------------------------------------

make_cna_panel <- function(g, label) {
  ggplot(g) +
    geom_sf(data = colombia_boundary, fill = "gray97", color = "gray80", linewidth = 0.2) +
    geom_sf(aes(fill = is_critical, alpha = magnitude), color = NA) +
    scale_fill_manual(values = c("TRUE" = wwf_green, "FALSE" = "gray95"),
                       labels = c("TRUE" = "Crítico", "FALSE" = "No crítico"),
                       name = NULL) +
    scale_alpha(range = c(0.3, 1), guide = "none") +
    labs(title = label) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, color = wwf_dark_green),
      legend.position = "bottom",
      legend.text = element_text(size = 8)
    )
}

# Flat-fill comparison version (no opacity gradient) -- generated alongside
# the variable-intensity version so the two can be compared before deciding
# which to keep in the deck (2026-08-20: user asked to see both).
make_cna_panel_flat <- function(g, label) {
  ggplot(g) +
    geom_sf(data = colombia_boundary, fill = "gray97", color = "gray80", linewidth = 0.2) +
    geom_sf(aes(fill = is_critical), color = NA) +
    scale_fill_manual(values = c("TRUE" = wwf_green, "FALSE" = "gray95"),
                       labels = c("TRUE" = "Crítico", "FALSE" = "No crítico"),
                       name = NULL) +
    labs(title = label) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, color = wwf_dark_green),
      legend.position = "bottom",
      legend.text = element_text(size = 8)
    )
}

panels <- list()
panels_flat <- list()

for (svc in svc_defs) {
  message("Processing ", svc$label, " (", svc$file, ")...")

  r <- rast(file.path(solutions_dir, svc$file))
  r <- trim(r)
  r_wgs84 <- project(r, "EPSG:4326", method = "near")

  frac_crit <- terra::extract(r_wgs84, grid_col_vect, fun = mean, na.rm = TRUE, ID = FALSE)
  g <- grid_col
  g$frac_critical <- frac_crit[[1]]
  g$frac_critical[is.na(g$frac_critical)] <- 0
  g$is_critical <- g$frac_critical >= 0.5

  # Magnitude layer(s): continuous "realized" provision, reprojected with
  # bilinear (not "near" -- this is continuous data, not a classification).
  # Multiple files (nature access) are averaged before zonal extraction.
  mag_rasters <- lapply(svc$mag_files, function(f) {
    m <- rast(file.path(layers_dir, f))
    m <- trim(m)
    project(m, "EPSG:4326", method = "bilinear")
  })
  m_combined <- if (length(mag_rasters) > 1) {
    mag_rasters <- lapply(mag_rasters, function(m) resample(m, mag_rasters[[1]], method = "bilinear"))
    mean(rast(mag_rasters))
  } else {
    mag_rasters[[1]]
  }

  mag_vals <- terra::extract(m_combined, grid_col_vect, fun = mean, na.rm = TRUE, ID = FALSE)[[1]]
  # Values are not 0-1 despite "norm" in the filenames -- clip to 1st/99th
  # percentile (same pattern as make_colombia_report_maps.R) before rescaling.
  q <- quantile(mag_vals, c(0.01, 0.99), na.rm = TRUE)
  mag_clipped <- pmax(pmin(mag_vals, q[2]), q[1])
  g$magnitude <- (mag_clipped - q[1]) / (q[2] - q[1])
  g$magnitude[is.na(g$magnitude)] <- 0
  # Outside the critical fill, magnitude opacity is irrelevant to the map's
  # message -- clamp non-critical cells to 0 (the low end of scale_alpha's
  # range) so alpha only communicates gradient within "critical" (green) cells.
  g$magnitude[!g$is_critical] <- 0

  p <- make_cna_panel(g, svc$label)
  panels[[svc$slug]] <- p

  fp <- file.path(out_dir, paste0("colombia_cna_", svc$slug, ".png"))
  ggsave(fp, p, width = 5.5, height = 6.5, dpi = 200, bg = "white")
  message("  Saved: ", fp)

  p_flat <- make_cna_panel_flat(g, svc$label)
  panels_flat[[svc$slug]] <- p_flat

  fp_flat <- file.path(out_dir, paste0("colombia_cna_", svc$slug, "_flat.png"))
  ggsave(fp_flat, p_flat, width = 5.5, height = 6.5, dpi = 200, bg = "white")
  message("  Saved: ", fp_flat)
}

# ------------------------------------------------------------------------------
# 3. Combined 5-panel figure
# ------------------------------------------------------------------------------

combined_cna <- wrap_plots(panels, ncol = 3) +
  plot_annotation(
    title = "Colombia — Activos naturales críticos, servicio por servicio",
    subtitle = "Solución individual por servicio (umbral 90%, optimización prioritizr) — celda de 10km\nEl tono dentro de cada celda crítica refleja la magnitud de provisión del servicio, no un rango de prioridad",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = wwf_dark_green),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30")
    )
  )
ggsave(file.path(out_dir, "colombia_cna_5panel.png"), combined_cna,
       width = 16, height = 11, dpi = 200, bg = "white")
message("Saved combined CNA panel.")

combined_cna_flat <- wrap_plots(panels_flat, ncol = 3) +
  plot_annotation(
    title = "Colombia — Activos naturales críticos, servicio por servicio",
    subtitle = "Solución individual por servicio (umbral 90%, optimización prioritizr) — celda de 10km\nRelleno binario: crítico / no crítico (sin gradiente de intensidad)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = wwf_dark_green),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30")
    )
  )
ggsave(file.path(out_dir, "colombia_cna_5panel_flat.png"), combined_cna_flat,
       width = 16, height = 11, dpi = 200, bg = "white")
message("Saved flat-fill combined CNA panel (comparison).")

message("\nDone.")
