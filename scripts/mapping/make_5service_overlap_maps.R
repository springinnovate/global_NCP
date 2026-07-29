# ==============================================================================
# 5-Service Hotspot Redesign: Water / Access / Combined Overlap Maps
# Per Becky's 2026-07-21 meeting + Slack instructions -- the 3 map outputs
# requested: water overlap, access/coastal/pollination overlap, combined
# cross-category overlap. Mirrors scripts/mapping/make_hotspot_count_map.R's
# conventions exactly (same base map, projection, theme, output location) so
# these sit visually consistent with the existing hotspot map suite.
# ==============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(here)

source(here("R", "paths.R"))

out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Base map: same custom cartographic basemap as make_hotspot_count_map.R
base_map_path <- file.path(data_dir(), "vector_basedata", "cartographic_ee_r264_correspondence.gpkg")
if (file.exists(base_map_path)) {
  message("Reading custom base map...")
  base_sf <- st_read(base_map_path, quiet = TRUE)
  base_sf <- st_transform(base_sf, crs = "EPSG:8857")
} else {
  message("Custom base map not found; falling back to rnaturalearth.")
  base_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  base_sf <- st_transform(base_sf, crs = "EPSG:8857")
}

theme_hotspot_map <- theme_void() + theme(
  plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
  legend.position = "bottom",
  legend.key.width = unit(2.5, "cm")
)

generate_water_overlap_map <- function(gpkg_path, metric, base_sf) {
  message("\nWater overlap map, metric: ", metric)
  if (!file.exists(gpkg_path)) { message("  -> WARNING: not found: ", gpkg_path); return(invisible(NULL)) }

  sf_data <- st_read(gpkg_path, quiet = TRUE) |> st_transform(crs = "EPSG:8857")
  metric_label <- ifelse(metric == "pct", "Percentage Change", "Absolute Change")

  # Only cells that ARE water-overlap hotspots (count_water >= 1) -- matches
  # Becky's spec: "include areas that are hotspots for either one or both"
  sf_water <- sf_data |>
    filter(as.numeric(count_water) >= 1) |>
    mutate(water_label = factor(as.numeric(count_water), levels = 1:2, labels = c("1 (N or Sed export)", "2 (both)")))

  p <- ggplot() +
    geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
    geom_sf(data = sf_water, aes(fill = water_label), color = NA) +
    scale_fill_manual(name = "Water services\nin top-5% decline", values = c("1 (N or Sed export)" = "#4FC3F7", "2 (both)" = "#01579B"), na.value = "gray90", drop = FALSE) +
    labs(
      title = paste("Water Overlap Hotspots", paste0("(", metric_label, ")")),
      subtitle = "Cells in the top-5% decline tail for Nitrogen Export and/or Sediment Export"
    ) +
    theme_hotspot_map

  out_path <- file.path(out_dir, paste0("global_water_overlap_heatmap_", metric, ".png"))
  ggsave(out_path, p, width = 16, height = 9, bg = "white", dpi = 300)
  message("Saved: ", out_path)
}

generate_access_overlap_map <- function(gpkg_path, metric, base_sf) {
  message("\nAccess overlap map, metric: ", metric)
  if (!file.exists(gpkg_path)) { message("  -> WARNING: not found: ", gpkg_path); return(invisible(NULL)) }

  sf_data <- st_read(gpkg_path, quiet = TRUE) |> st_transform(crs = "EPSG:8857")
  metric_label <- ifelse(metric == "pct", "Percentage Change", "Absolute Change")

  sf_access <- sf_data |>
    filter(as.numeric(count_access) >= 1) |>
    mutate(access_label = factor(as.numeric(count_access), levels = 1:3, labels = c("1", "2", "3 (all three)")))

  p <- ggplot() +
    geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
    geom_sf(data = sf_access, aes(fill = access_label), color = NA) +
    scale_fill_manual(name = "Access services\nin top-5% decline", values = c("1" = "#FFD54F", "2" = "#FB8C00", "3 (all three)" = "#E53935"), na.value = "gray90", drop = FALSE) +
    labs(
      title = paste("Access / Coastal / Pollination Overlap Hotspots", paste0("(", metric_label, ")")),
      subtitle = "Cells in the top-5% decline tail for Nature Access, Pollination, and/or Coastal Risk"
    ) +
    theme_hotspot_map

  out_path <- file.path(out_dir, paste0("global_access_overlap_heatmap_", metric, ".png"))
  ggsave(out_path, p, width = 16, height = 9, bg = "white", dpi = 300)
  message("Saved: ", out_path)
}

generate_combined_cross_map <- function(gpkg_path, metric, base_sf) {
  message("\nCombined cross-category map, metric: ", metric)
  if (!file.exists(gpkg_path)) { message("  -> WARNING: not found: ", gpkg_path); return(invisible(NULL)) }

  sf_data <- st_read(gpkg_path, quiet = TRUE) |> st_transform(crs = "EPSG:8857")
  metric_label <- ifelse(metric == "pct", "Percentage Change", "Absolute Change")

  # Per Becky's spec: only cells with >=1 water hotspot AND >=1 access hotspot --
  # explicitly excludes water-only and access-only cells. Two-tier shading (same
  # grammar as the water/access-pollination maps): light = the minimum possible
  # cross-over (exactly 1 water + 1 access = hotspot_count 2), dark = deeper
  # compounding (hotspot_count 3+, up to all 5 services). Distribution checked
  # 2026-07-29: 24,227 cells at the minimum, 23,774 at 3+ (16,877 / 6,883 / 14
  # for 3, 4, 5 respectively) -- a near-even split, not a long tail dominated by
  # one bucket.
  sf_cross <- sf_data |>
    filter(as.numeric(combined_cross) == 1) |>
    mutate(cross_depth = ifelse(as.numeric(hotspot_count) <= 2, "2 (minimum: 1 water + 1 access)", "3+ (deeper overlap)"))

  p <- ggplot() +
    geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
    geom_sf(data = sf_cross, aes(fill = cross_depth), color = NA) +
    scale_fill_manual(name = "Total services\noverlapping", values = c("2 (minimum: 1 water + 1 access)" = "#CE93D8", "3+ (deeper overlap)" = "#4A148C"), na.value = "gray90", drop = FALSE) +
    labs(
      title = paste("Combined Cross-Category Overlap Hotspots", paste0("(", metric_label, ")")),
      subtitle = "Cells with >=1 water-service hotspot AND >=1 access-service hotspot (water-only / access-only cells excluded)"
    ) +
    theme_hotspot_map

  out_path <- file.path(out_dir, paste0("global_combined_cross_overlap_heatmap_", metric, ".png"))
  ggsave(out_path, p, width = 16, height = 9, bg = "white", dpi = 300)
  message("Saved: ", out_path)
}

# ------------------------------------------------------------------------------
# Follow-up (2026-07-29): the 3-way "access" map (Nature Access + Pollination +
# Coastal Risk) barely shows anything meaningful at global scale -- Coastal Risk
# is inherently a narrow shoreline phenomenon, so the "all three" tier is a strip
# too thin to see, and lumping it in with the other two dilutes the map's real
# signal. Splitting into two targeted pairwise maps instead, using the
# individual per-service binary columns already in the gpkg (no re-extraction
# needed): Access+Coastal Risk (expected to be a real, if narrow, coastal
# signal) and Access+Pollination (expected to be the much broader, more
# interesting terrestrial story), the latter with a bolder, higher-contrast
# single color per the ask. Original 3-way access map is kept as-is.
# ------------------------------------------------------------------------------

generate_access_coastal_pair_map <- function(gpkg_path, metric, base_sf) {
  message("\nAccess + Coastal Risk pairwise map, metric: ", metric)
  if (!file.exists(gpkg_path)) { message("  -> WARNING: not found: ", gpkg_path); return(invisible(NULL)) }

  sf_data <- st_read(gpkg_path, quiet = TRUE) |> st_transform(crs = "EPSG:8857")
  metric_label <- ifelse(metric == "pct", "Percentage Change", "Absolute Change")

  sf_pair <- sf_data |> filter(as.numeric(Nature_Access) == 1 & as.numeric(C_Risk) == 1)

  p <- ggplot() +
    geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
    geom_sf(data = sf_pair, fill = "#00838F", color = NA) +
    labs(
      title = paste("Access + Coastal Risk Overlap Hotspots", paste0("(", metric_label, ")")),
      subtitle = "Cells that are hotspots for BOTH Nature Access AND Coastal Risk (regardless of Pollination) -- expected to be a narrow, shoreline-concentrated signal"
    ) +
    theme_hotspot_map

  out_path <- file.path(out_dir, paste0("global_access_coastal_pair_heatmap_", metric, ".png"))
  ggsave(out_path, p, width = 16, height = 9, bg = "white", dpi = 300)
  message("Saved: ", out_path)
}

generate_access_pollination_pair_map <- function(gpkg_path, metric, base_sf) {
  message("\nAccess + Pollination pairwise map, metric: ", metric)
  if (!file.exists(gpkg_path)) { message("  -> WARNING: not found: ", gpkg_path); return(invisible(NULL)) }

  sf_data <- st_read(gpkg_path, quiet = TRUE) |> st_transform(crs = "EPSG:8857")
  metric_label <- ifelse(metric == "pct", "Percentage Change", "Absolute Change")

  # Two-tier count within just this pair (Nature_Access + Pollination, 0/1/2),
  # same structure as count_water: light = hotspot for exactly one of the two,
  # dark = hotspot for both simultaneously. Previously this map only showed the
  # strict AND (the "2" tier) and silently dropped single-service cells --
  # this version shows both tiers, like the water map does.
  sf_pair <- sf_data |>
    mutate(access_poll_count = as.numeric(Nature_Access) + as.numeric(Pollination)) |>
    filter(access_poll_count >= 1) |>
    mutate(access_poll_label = factor(access_poll_count, levels = 1:2, labels = c("1 (Access or Pollination)", "2 (both)")))

  p <- ggplot() +
    geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
    geom_sf(data = sf_pair, aes(fill = access_poll_label), color = NA) +
    scale_fill_manual(name = "Access + Pollination\nin top-5% decline", values = c("1 (Access or Pollination)" = "#FFAB91", "2 (both)" = "#BF360C"), na.value = "gray90", drop = FALSE) +
    labs(
      title = paste("Access + Pollination Overlap Hotspots", paste0("(", metric_label, ")")),
      subtitle = "Cells in the top-5% decline tail for Nature Access and/or Pollination (regardless of Coastal Risk)"
    ) +
    theme_hotspot_map

  out_path <- file.path(out_dir, paste0("global_access_pollination_pair_heatmap_", metric, ".png"))
  ggsave(out_path, p, width = 16, height = 9, bg = "white", dpi = 300)
  message("Saved: ", out_path)
}

# ==============================================================================
# Execution -- pct metric only for now (the primary, canonical metric currently
# being shared; abs can be added the same way later if needed)
# ==============================================================================

gpkg_5service_pct <- file.path(data_dir(), "processed", "hotspots_5service", "pct", "global", "hotspots_global_5service_pct.gpkg")

generate_water_overlap_map(gpkg_5service_pct, metric = "pct", base_sf = base_sf)
generate_access_overlap_map(gpkg_5service_pct, metric = "pct", base_sf = base_sf)
generate_combined_cross_map(gpkg_5service_pct, metric = "pct", base_sf = base_sf)
generate_access_coastal_pair_map(gpkg_5service_pct, metric = "pct", base_sf = base_sf)
generate_access_pollination_pair_map(gpkg_5service_pct, metric = "pct", base_sf = base_sf)
