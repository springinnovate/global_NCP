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
get_color_scale <- function(data_vec, service) {
  if (all(is.na(data_vec))) {
    return(scale_fill_viridis_c(na.value = "gray90", name = "Change"))
  }

  dir <- get_direction(service)

  if (dir == "good") {
    # Good: high is Green, low is Red
    scale_fill_gradient2(low = "red", mid = "white", high = "forestgreen",
                         midpoint = 0, name = "Change", na.value = "gray90")
  } else {
    # Damage: high is Red, low is Green
    scale_fill_gradient2(low = "forestgreen", mid = "white", high = "red",
                         midpoint = 0, name = "Change", na.value = "gray90")
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
plot_single_service <- function(sf_data, service_name, value_col = "pct_chg") {

  # Filter data for the specific service
  plot_data <- sf_data %>% filter(service == service_name)

  if (nrow(plot_data) == 0) return(NULL)

  # Trim extreme outliers (1st and 99th percentiles) to prevent squashed color ramps
  val_vec <- plot_data[[value_col]]
  q_low <- quantile(val_vec, 0.01, na.rm = TRUE)
  q_high <- quantile(val_vec, 0.99, na.rm = TRUE)
  plot_data[[value_col]] <- pmax(pmin(val_vec, q_high), q_low)

  # Apply Rule 2 to get the correct color scale
  color_scale <- get_color_scale(plot_data[[value_col]], service_name)

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

# Function to create the faceted layout for a specific grouping
generate_faceted_map <- function(gpkg_path, grouping_name, value_col = "pct_chg", out_dir = "outputs/plots/maps") {

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

  plots <- map(services, ~plot_single_service(sf_data, .x, value_col))
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
  sf_wb <- load_and_clean(groupings_files$World_Bank_Region)
  sf_wb$region_wb[is.na(sf_wb$region_wb) | sf_wb$region_wb == "Antarctica"] <- NA
  p_wb <- ggplot(sf_wb) +
    geom_sf(aes(fill = region_wb), color = "gray50", linewidth = 0.1) +
    scale_fill_manual(values = group_palettes$region_wb, na.value = "gray90", name = NULL) +
    labs(title = "World Bank Regions") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "bottom", legend.text = element_text(size = 9)) +
    guides(fill = guide_legend(ncol = 3))

  # 2. Income Group
  sf_inc <- load_and_clean(groupings_files$Income_Group)
  sf_inc$income_grp[is.na(sf_inc$income_grp) | sf_inc$income_grp %in% c("Not classified", "Unclassified")] <- NA
  p_inc <- ggplot(sf_inc) +
    geom_sf(aes(fill = income_grp), color = "gray50", linewidth = 0.1) +
    scale_fill_manual(values = group_palettes$income_grp, na.value = "gray90", name = NULL) +
    labs(title = "Income Groups") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "bottom", legend.text = element_text(size = 9)) +
    guides(fill = guide_legend(ncol = 2))

  # 3. Biome
  sf_bio <- load_and_clean(groupings_files$Biome)
  sf_bio$WWF_biome[is.na(sf_bio$WWF_biome) | sf_bio$WWF_biome %in% c("Lakes", "Rock & Ice")] <- NA
  p_bio <- ggplot(sf_bio) +
    geom_sf(aes(fill = WWF_biome), color = "gray50", linewidth = 0.1) +
    scale_fill_manual(values = group_palettes$biome, na.value = "gray90", name = NULL) +
    labs(title = "WWF Biomes") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "bottom", legend.text = element_text(size = 9)) +
    guides(fill = guide_legend(ncol = 2))

  # 4. Country
  sf_ctry <- load_and_clean(groupings_files$Country)
  p_ctry <- ggplot(sf_ctry) +
    geom_sf(fill = "#E0E0E0", color = "gray40", linewidth = 0.15) +
    labs(title = "National Boundaries") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

  combined_plot <- (p_wb + p_inc) / (p_bio + p_ctry) +
    plot_annotation(title = "Global NCP Analytical Context: Geographic Groupings",
                    theme = theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)))

  out_path <- file.path(out_dir, "context_groupings_map.png")
  ggsave(out_path, combined_plot, width = 18, height = 14, bg = "white", dpi = 300)
  message("Saved context map to: ", out_path)
}

# ------------------------------------------------------------------------------
# 3. Execution
# ------------------------------------------------------------------------------

source(here::here("R", "paths.R"))

# Define your groupings and their corresponding GPKG file paths
groupings_files <- list(
  "World_Bank_Region" = file.path(data_dir(), "processed", "output_maps", "region_wb_change_map.gpkg"),
  "Income_Group"      = file.path(data_dir(), "processed", "output_maps", "income_grp_change_map.gpkg"),
  "Biome"             = file.path(data_dir(), "processed", "output_maps", "biome_change_map.gpkg"),
  "Country"           = file.path(data_dir(), "processed", "output_maps", "country_change_map.gpkg")
)

# Run the loop for percentage change maps
iwalk(groupings_files, ~generate_faceted_map(gpkg_path = .x, grouping_name = .y, value_col = "pct"))

# Run the loop for absolute change maps
iwalk(groupings_files, ~generate_faceted_map(gpkg_path = .x, grouping_name = .y, value_col = "abs"))

# Generate the contextual groupings map
generate_context_groupings_map(groupings_files)