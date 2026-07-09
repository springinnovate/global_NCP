# ==============================================================================
# Automate Faceted Global Maps for Ecosystem Services Change
# ==============================================================================
#
# Author: Jerónimo Rodríguez Escobar
#
# Description:
# This script generates a series of faceted global maps visualizing the change
# in multiple ecosystem services across different geographic groupings (e.g.,
# World Bank Regions, Income Groups). It enforces a strict set of cartographic
# rules to ensure consistency and clear communication for presentations.
#
# Key Methodological Rules:
# 1.  **Semantic Color Scale**: A universal Red/Green diverging scale is used,
#     anchored at zero. Red always signifies a negative outcome (loss of a
#     good, increase of a damage), and Green a positive one. This is enforced
#     even for datasets that do not cross zero to maintain visual consistency.
# 2.  **Antarctica Masking**: The script includes a spatial filtering step to
#     identify and neutralize the continent of Antarctica, which often causes
#     visual artifacts by being mis-classified into unrelated economic groups
#     (e.g., "High income: nonOECD"). It is rendered in a neutral gray.

library(sf)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(here)
library(patchwork) # Essential for different color scales in a "faceted" layout
library(rnaturalearth)

# ------------------------------------------------------------------------------
# 1. Cartography Rule Engine & Lookup Tables
# ------------------------------------------------------------------------------

# RULE 1: Directionality
# Goods: Increase is Good, Decrease is Bad
goods <- c("Pollination", "Nature_Access", "N_Ret_Ratio", "Sed_Ret_Ratio", "C_Risk_Red_Ratio")

# Damages: Increase is Bad, Decrease is Good
damages <- c("N_export", "Sed_export", "C_Risk")

# Helper to determine directionality
get_direction <- function(service) {
  if (service %in% goods) return("good")
  if (service %in% damages) return("damage")
  return("unknown")
}

# RULE 2: Color Ramp Selection
# Colorblind-safe diverging pair (Okabe-Ito-derived, WWF-brand-consistent):
# orange = decline in the service being mapped, teal = improvement. Avoids
# red/green, which is indistinguishable to the most common form of color
# vision deficiency.
get_color_scale <- function(data_vec, service, limits = NULL) {
  if (all(is.na(data_vec))) {
    return(scale_fill_viridis_c(na.value = "gray90", name = "Change", limits = limits))
  }

  dir <- get_direction(service)

  if (dir == "good") {
    # Good: high (more of a good thing) is teal, low (less) is orange
    scale_fill_gradient2(low = "#F07D00", mid = "white", high = "#009191",
                         midpoint = 0, name = "Change", na.value = "gray90", limits = limits)
  } else {
    # Damage: high (more damage) is orange, low (less damage) is teal
    scale_fill_gradient2(low = "#009191", mid = "white", high = "#F07D00",
                         midpoint = 0, name = "Change", na.value = "gray90", limits = limits)
  }
}

# RULE 3: Universal Palettes for Context Maps
group_palettes <- list(
  biome = c(
    'Tropical & Subtropical Moist Broadleaf Forests' = '#319D00',
    'Tropical & Subtropical Dry Broadleaf Forests' = '#7ABD1B',
    'Tropical & Subtropical Coniferous Forests' = '#556E19',
    'Temperate Broadleaf & Mixed Forests' = '#207433',
    'Temperate Coniferous Forests' = '#3E8D62',
    'Boreal Forests/Taiga' = '#496FF3',
    'Tropical & Subtropical Grasslands, Savannas & Shrublands' = '#D6F392',
    'Temperate Grasslands, Savannas & Shrublands' = '#D1E614',
    'Flooded Grasslands & Savannas' = '#75D0D5',
    'Montane Grasslands & Shrublands' = '#98E600',
    'Tundra' = '#C7DEFF',
    'Mediterranean Forests, Woodlands & Scrub' = '#AF963C',
    'Deserts & Xeric Shrublands' = '#C55C5C',
    'Mangroves' = '#FE04BC'
  ),
  income_grp = c('1. High income: OECD' = '#004D33', '2. High income: nonOECD' = '#1d7355', '3. Upper middle income' = '#4b9e80', '4. Lower middle income' = '#8bc5af', '5. Low income' = '#cde9df'),
  region_wb = c('East Asia & Pacific' = '#2E5A88', 'Europe & Central Asia' = '#D86018', 'Latin America & Caribbean' = '#7A3F91', 'Middle East & North Africa' = '#B38F00', 'North America' = '#1D8A99', 'South Asia' = '#6B8E23', 'Sub-Saharan Africa' = '#8B0000')
)

# ------------------------------------------------------------------------------
# 2. Map Generation Function
# ------------------------------------------------------------------------------

# Function to generate a plot for a single service
# `shared_limits`, when supplied, is a symmetric c(-lim, lim) range computed
# ACROSS ALL SERVICES in the parent map (see generate_faceted_map()) so that
# the same color represents the same magnitude of change in every panel.
# Without this, each service was independently scaled to its own 1st/99th
# percentile range, making the same red/green shade mean very different
# things from one panel to the next.
plot_single_service <- function(sf_data, service_name, value_col = "pct_chg", shared_limits = NULL) {

  # Filter data for the specific service
  plot_data <- sf_data %>% filter(service == service_name)

  if (nrow(plot_data) == 0) return(NULL)

  val_vec <- plot_data[[value_col]]

  if (!is.null(shared_limits)) {
    # Trim to the shared, cross-service range so panels are comparable
    plot_data[[value_col]] <- pmax(pmin(val_vec, shared_limits[2]), shared_limits[1])
  } else {
    # Fallback: trim extreme outliers (1st and 99th percentiles) to this
    # service's own range only -- used only if no shared range was computed
    q_low <- quantile(val_vec, 0.01, na.rm = TRUE)
    q_high <- quantile(val_vec, 0.99, na.rm = TRUE)
    plot_data[[value_col]] <- pmax(pmin(val_vec, q_high), q_low)
  }

  # Apply Rule 2 to get the correct color scale, shared across all panels
  color_scale <- get_color_scale(plot_data[[value_col]], service_name, limits = shared_limits)

  # Generate the map
  p <- ggplot(plot_data) +
    geom_sf(aes(fill = .data[[value_col]]), color = "gray50", linewidth = 0.1) +
    color_scale +
    labs(title = service_name) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      legend.position = "bottom",
      legend.key.width = unit(1, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7)
    )

  return(p)
}

# Function to generate a clean "First Look" global transition map
generate_first_look_map <- function(gpkg_path, out_file) {
  message("Generating First Look map from: ", gpkg_path)
  if (!file.exists(gpkg_path)) {
    message("  -> WARNING: File not found: ", gpkg_path)
    return(NULL)
  }
  
  hotspots <- st_read(gpkg_path, quiet = TRUE)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  p <- ggplot() +
    geom_sf(data = world, fill = "gray90", color = "white", linewidth = 0.2) +
    geom_sf(data = hotspots, fill = "#E83737", color = NA) + 
    coord_sf(crs = "+proj=eqearth") + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(0, 0, 0, 0))
  
  ggsave(out_file, p, width = 16, height = 9, dpi = 300, bg = "white")
  message("Saved First Look map to: ", out_file)
}

# Computes a per-service symmetric color limit (1st/99th percentile, pooled
# across every grouping file) so the SAME service uses the SAME color range
# in every map it appears in (region/income/biome), for a given metric
# (pct or abs). Deliberately does NOT share one limit ACROSS different
# services within a map: abs-change units differ by service (per-hectare
# mass for Sed/N export, dimensionless index for Pollination/Nature Access,
# per-linear-metre for Coastal Risk), so forcing them onto one shared
# absolute-value scale would misrepresent magnitude rather than clarify it.
# Percentage change is comparable across services by construction (SPC),
# so this still gives every service its fair, comparable range, just not a
# single one shared by all 8 at once.
compute_service_limits <- function(gpkg_paths, value_col) {
  all_vals <- map_dfr(gpkg_paths, function(p) {
    if (!file.exists(p)) return(tibble())
    d <- st_read(p, quiet = TRUE) %>% st_drop_geometry()
    chg_cols <- grep(paste0("_", value_col, "$"), names(d), value = TRUE)
    if (length(chg_cols) == 0) return(tibble())
    d %>%
      select(all_of(chg_cols)) %>%
      pivot_longer(everything(), names_to = "service", values_to = value_col) %>%
      mutate(service = sub(paste0("_", value_col, "$"), "", service))
  })

  limits_df <- all_vals %>%
    filter(!is.na(.data[[value_col]])) %>%
    group_by(service) %>%
    summarise(
      q_low = quantile(.data[[value_col]], 0.01, na.rm = TRUE),
      q_high = quantile(.data[[value_col]], 0.99, na.rm = TRUE),
      lim = max(abs(q_low), abs(q_high)),
      .groups = "drop"
    )

  limits_list <- setNames(
    lapply(limits_df$lim, function(l) c(-l, l)),
    limits_df$service
  )
  limits_list
}

# Function to create the faceted layout for a specific grouping
generate_faceted_map <- function(gpkg_path, grouping_name, value_col = "pct_chg", out_dir = "outputs/plots/maps", service_limits = NULL) {

  message("Processing grouping: ", grouping_name)

  if (!file.exists(gpkg_path)) {
    message("  -> WARNING: File not found: ", gpkg_path)
    message("  -> Skipping ", grouping_name, "...")
    return(NULL)
  }

  sf_data <- st_read(gpkg_path, quiet = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # ---------------------------------------------------------------------------
  # Identify Antarctica to prevent attribute grouping artifacts
  # (e.g., Antarctica being mistakenly classified as "High income: nonOECD").
  # We break multi-part geometries into single polygons and flag any landmass
  # whose centroid is below -60 latitude so it can be colored gray (NA).
  # ---------------------------------------------------------------------------
  sf_data <- suppressWarnings(st_cast(sf_data, "POLYGON"))
  cents <- suppressWarnings(st_centroid(st_geometry(sf_data)))
  cents_4326 <- st_transform(cents, 4326)
  sf_data$is_antarctica <- st_coordinates(cents_4326)[, "Y"] <= -60

  # Transform to Equal Earth projection (EPSG:8857) to correct polar distortion
  sf_data <- st_transform(sf_data, crs = "EPSG:8857")

  # Safety check: Pivot to long format if the data is wide
  if (!"service" %in% names(sf_data)) {
    message("  -> Data appears to be in 'wide' format. Pivoting to long format...")

    # Strictly look for the exact metric we are mapping (e.g., _pct_chg)
    chg_cols <- grep(paste0("_", value_col, "$"), names(sf_data), value = TRUE)

    if (length(chg_cols) > 0) {
      sf_data <- sf_data %>%
        pivot_longer(cols = all_of(chg_cols), names_to = "service", values_to = value_col) %>%
        mutate(service = sub(paste0("_", value_col, "$"), "", service))
    } else {
      message("  -> WARNING: Could not find any columns ending in '_", value_col, "'.")
    }
  }

  # Mask out Antarctica and unclassified regions so they render as neutral gray (NA)
  if ("income_grp" %in% names(sf_data)) {
    sf_data[[value_col]][is.na(sf_data$income_grp) | sf_data$income_grp %in% c("NA", "Not classified", "Unclassified")] <- NA
  }
  if ("region_wb" %in% names(sf_data)) {
    sf_data[[value_col]][is.na(sf_data$region_wb) | sf_data$region_wb %in% c("NA", "Antarctica")] <- NA
  }
  if ("WWF_biome" %in% names(sf_data)) {
    sf_data[[value_col]][is.na(sf_data$WWF_biome) | sf_data$WWF_biome %in% c("NA", "Lakes", "Rock & Ice")] <- NA
  }
  if ("iso3" %in% names(sf_data)) {
    sf_data[[value_col]][is.na(sf_data$iso3) | sf_data$iso3 == "ATA"] <- NA
  }
  if ("nev_name" %in% names(sf_data)) {
    sf_data[[value_col]][is.na(sf_data$nev_name) | sf_data$nev_name == "Antarctica"] <- NA
  }
  if ("is_antarctica" %in% names(sf_data)) {
    sf_data[[value_col]][sf_data$is_antarctica] <- NA
  }

  services <- unique(sf_data$service)
  services <- services[!is.na(services)]

  # Enforce Canonical Ordering
  canon_order <- c("C_Risk", "N_export", "Sed_export",
                   "C_Risk_Red_Ratio", "N_Ret_Ratio", "Sed_Ret_Ratio",
                   "Pollination", "Nature_Access")
  services <- intersect(canon_order, services)

  plots <- map(services, ~plot_single_service(sf_data, .x, value_col, shared_limits = service_limits[[.x]]))
  plots <- compact(plots) # Remove NULLs

  if (length(plots) == 0) {
    message("  -> ERROR: No valid maps generated. Check if the '", value_col, "' columns exist and contain data. Skipping...")
    return(NULL)
  }

  metric_label <- if (value_col == "pct") "Percentage Change (%)" else "Absolute Change"

  # Stitch them together into a layout (simulating facet_wrap)
  combined_plot <- wrap_plots(plots, ncol = 3) +
    plot_annotation(title = paste("Ecosystem Services Change:", gsub("_", " ", grouping_name)),
                    subtitle = metric_label,
                    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                                  plot.subtitle = element_text(size = 14, hjust = 0.5)))

  out_path <- file.path(out_dir, paste0("map_", grouping_name, "_", value_col, ".png"))
  ggsave(out_path, combined_plot, width = 16, height = 12, bg = "white", dpi = 300)
  message("Saved map to: ", out_path)
}

# Function to generate a 4-facet context map showing the geographic groupings
generate_context_groupings_map <- function(groupings_files, out_dir = "outputs/plots/maps") {
  message("Generating Context Groupings map...")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  biome_labels <- c(
    'Tropical & Subtropical Moist Broadleaf Forests' = 'Trop/Subtrop Moist Broadleaf',
    'Tropical & Subtropical Dry Broadleaf Forests' = 'Trop/Subtrop Dry Broadleaf',
    'Tropical & Subtropical Coniferous Forests' = 'Trop/Subtrop Coniferous',
    'Temperate Broadleaf & Mixed Forests' = 'Temp Broadleaf/Mixed',
    'Temperate Coniferous Forests' = 'Temp Coniferous',
    'Boreal Forests/Taiga' = 'Boreal/Taiga',
    'Tropical & Subtropical Grasslands, Savannas & Shrublands' = 'Trop/Subtrop Grass/Sav/Shrub',
    'Temperate Grasslands, Savannas & Shrublands' = 'Temp Grass/Sav/Shrub',
    'Flooded Grasslands & Savannas' = 'Flooded Grass/Savannas',
    'Montane Grasslands & Shrublands' = 'Montane Grass/Shrub',
    'Tundra' = 'Tundra',
    'Mediterranean Forests, Woodlands & Scrub' = 'Mediterranean',
    'Deserts & Xeric Shrublands' = 'Deserts & Xeric Shrub',
    'Mangroves' = 'Mangroves'
  )

  # Helper to load and clean geometries (masking Antarctica)
  load_and_clean <- function(gpkg_path) {
    d <- st_read(gpkg_path, quiet = TRUE)
    d <- suppressWarnings(st_cast(d, "POLYGON"))
    cents <- suppressWarnings(st_centroid(st_geometry(d)))
    cents_4326 <- st_transform(cents, 4326)
    d <- d[st_coordinates(cents_4326)[, "Y"] > -60, ]
    d <- st_transform(d, crs = "EPSG:8857")
    return(d)
  }

  # 1. World Bank Region
  sf_wb <- load_and_clean(groupings_files$region_wb)
  sf_wb$region_wb[is.na(sf_wb$region_wb) | sf_wb$region_wb == "Antarctica"] <- NA
  p_wb <- ggplot(sf_wb) +
    geom_sf(aes(fill = region_wb), color = "gray50", linewidth = 0.1) +
    scale_fill_manual(values = group_palettes$region_wb, na.value = "gray90", name = NULL) +
    labs(title = "World Bank Regions") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "bottom", legend.text = element_text(size = 8),
          legend.key.size = unit(0.3, "cm")) +
    guides(fill = guide_legend(ncol = 3))

  # 2. Income Group
  sf_inc <- load_and_clean(groupings_files$income_grp)
  sf_inc$income_grp[is.na(sf_inc$income_grp) | sf_inc$income_grp %in% c("Not classified", "Unclassified")] <- NA
  p_inc <- ggplot(sf_inc) +
    geom_sf(aes(fill = income_grp), color = "gray50", linewidth = 0.1) +
    scale_fill_manual(values = group_palettes$income_grp, na.value = "gray90", name = NULL) +
    labs(title = "Income Groups") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "bottom", legend.text = element_text(size = 8),
          legend.key.size = unit(0.3, "cm")) +
    guides(fill = guide_legend(ncol = 2))

  # 3. Biome
  sf_bio <- load_and_clean(groupings_files$biome)
  sf_bio$WWF_biome[is.na(sf_bio$WWF_biome) | sf_bio$WWF_biome %in% c("Lakes", "Rock & Ice")] <- NA
  p_bio <- ggplot(sf_bio) +
    geom_sf(aes(fill = WWF_biome), color = "gray50", linewidth = 0.1) +
    scale_fill_manual(values = group_palettes$biome, labels = biome_labels, na.value = "gray90", name = NULL) +
    labs(title = "WWF Biomes") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "bottom", legend.text = element_text(size = 7),
          legend.key.size = unit(0.3, "cm")) +
    guides(fill = guide_legend(ncol = 3))

  # 4. Country
  sf_ctry <- load_and_clean(groupings_files$country)
  p_ctry <- ggplot(sf_ctry) +
    geom_sf(fill = "#E0E0E0", color = "gray40", linewidth = 0.15) +
    labs(title = "National Boundaries") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

  combined_plot <- (p_wb + p_inc) / (p_bio + p_ctry) +
    plot_annotation(title = "Global NCP Analytical Context: Geographic Groupings",
                    theme = theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)))

  out_path <- file.path(out_dir, "context_groupings_map.png")
  ggsave(out_path, combined_plot, width = 16, height = 9, bg = "white", dpi = 300)
  message("Saved context map to: ", out_path)
}

# ------------------------------------------------------------------------------
# 3. Execution
# ------------------------------------------------------------------------------

source(here::here("R", "paths.R"))

out_maps_dir <- here::here("outputs", "maps")
dir.create(out_maps_dir, recursive = TRUE, showWarnings = FALSE)

# Wipe out old maps before generating new ones to ensure clean state
old_maps <- list.files(out_maps_dir, pattern = "\\.png$", full.names = TRUE)
if (length(old_maps) > 0) {
  unlink(old_maps)
  message("Cleared ", length(old_maps), " old maps from: ", out_maps_dir)
}

# Define your groupings and their corresponding GPKG file paths
groupings_files <- list(
  "region_wb"  = here::here("outputs", "maps", "region_wb_change_map.gpkg"),
  "income_grp" = here::here("outputs", "maps", "income_grp_change_map.gpkg"),
  "biome"      = here::here("outputs", "maps", "biome_change_map.gpkg"),
  "country"    = here::here("outputs", "maps", "country_change_map.gpkg")
)

# 1. Generate First Look Overview Maps
gpkg_global_pct <- here::here("data", "processed", "hotspots", "pct", "global", "hotspots_global_pct.gpkg")
gpkg_global_abs <- here::here("data", "processed", "hotspots", "abs", "global", "hotspots_global_abs.gpkg")

generate_first_look_map(gpkg_global_pct, file.path(out_maps_dir, "first_look_map_pct.png"))
generate_first_look_map(gpkg_global_abs, file.path(out_maps_dir, "first_look_map_abs.png"))

# Compute per-service color limits ONCE, pooled across the three grouping
# maps (region/income/biome; "country" excluded -- much larger file, and not
# part of these particular faceted maps), so the same service gets the same
# color range in every map it appears in. Different services keep their own
# ranges rather than being forced onto one shared absolute-value scale.
mapped_groupings <- groupings_files[c("region_wb", "income_grp", "biome")]
message("Computing shared per-service color limits (pct)...")
service_limits_pct <- compute_service_limits(mapped_groupings, value_col = "pct")
message("Computing shared per-service color limits (abs)...")
service_limits_abs <- compute_service_limits(mapped_groupings, value_col = "abs")

# Run the loop for percentage change maps
iwalk(groupings_files, ~generate_faceted_map(gpkg_path = .x, grouping_name = .y, value_col = "pct", out_dir = out_maps_dir, service_limits = service_limits_pct))

# Run the loop for absolute change maps
iwalk(groupings_files, ~generate_faceted_map(gpkg_path = .x, grouping_name = .y, value_col = "abs", out_dir = out_maps_dir, service_limits = service_limits_abs))

# Generate the contextual groupings map
generate_context_groupings_map(groupings_files, out_dir = out_maps_dir)