# ==============================================================================
# Automate Faceted Global Maps for Ecosystem Services Change
# ==============================================================================
# This script reads spatial groupings, applies a dynamic Cartography Rule Engine,
# and generates faceted global maps with appropriate color ramps per service.

library(sf)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
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

  min_val <- min(data_vec, na.rm = TRUE)
  max_val <- max(data_vec, na.rm = TRUE)
  dir <- get_direction(service)

  crosses_zero <- (min_val < 0) && (max_val > 0)

  if (crosses_zero) {
    # Diverging scale
    if (dir == "good") {
      # Good: high is Blue/Green, low is Red
      scale_fill_gradient2(low = "red", mid = "white", high = "forestgreen",
                           midpoint = 0, name = "Change", na.value = "gray90")
    } else {
      # Damage: high is Red, low is Blue/Green
      scale_fill_gradient2(low = "forestgreen", mid = "white", high = "red",
                           midpoint = 0, name = "Change", na.value = "gray90")
    }
  } else {
    # Sequential scale (viridis)
    if (dir == "good") {
      # Good: Bad is low values. So we want low values to be dark.
      # direction = 1 makes low values dark.
      scale_fill_viridis_c(direction = 1, option = "viridis", name = "Change", na.value = "gray90")
    } else {
      # Damage: Bad is high values. So we want high values to be dark.
      # direction = -1 makes high values dark.
      scale_fill_viridis_c(direction = -1, option = "viridis", name = "Change", na.value = "gray90")
    }
  }
}

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

# ------------------------------------------------------------------------------
# 3. Execution
# ------------------------------------------------------------------------------

library(here)
source(here("R", "paths.R"))

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