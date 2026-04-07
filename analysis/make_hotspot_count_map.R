# ==============================================================================
# Global Hotspot Frequency Heatmap
# ==============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(here)

source(here("R", "paths.R"))

# Path to the global percent change hotspots layer
gpkg_path <- file.path(data_dir(), "processed", "hotspots", "pct", "global", "hotspots_global_pct.gpkg")

message("Reading global hotspots file...")
sf_data <- st_read(gpkg_path, quiet = TRUE)

message("Transforming to Equal Earth projection (EPSG:8857)...")
sf_data <- st_transform(sf_data, crs = "EPSG:8857")

message("Reading and transforming base map...")
base_map_path <- "/home/jeronimo/data/global_ncp/vector_basedata/cartographic_ee_r264_correspondence.gpkg"
base_sf <- st_read(base_map_path, quiet = TRUE)
base_sf <- st_transform(base_sf, crs = "EPSG:8857")

# Ensure hotspot_count is numeric so the color ramp behaves as a continuous gradient
sf_data$hotspot_count <- as.numeric(sf_data$hotspot_count)
max_count <- max(sf_data$hotspot_count, na.rm = TRUE)

message("Generating heatmap...")

p <- ggplot() +
  # Base map underneath
  geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
  # color = NA ensures the 10km grid cell borders do not render, preventing visual clutter
  geom_sf(data = sf_data, aes(fill = hotspot_count), color = NA) +
  scale_fill_viridis_c(
    option = "inferno",
    direction = 1,
    limits = c(1, max_count),   # Lock the scale so colors mean the same thing across maps
    breaks = seq(1, max_count), # Clean integer breaks in the legend
    name = "Overlapping\nHotspots"
  ) +
  labs(
    title = "Global Ecosystem Service Hotspot Frequency",
    subtitle = "Number of overlapping hotspots of decline/damage per 10km grid cell"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm")
  )

out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, "global_hotspot_count_heatmap.png")

message("Saving map to: ", out_path)
ggsave(out_path, p, width = 16, height = 9, bg = "white", dpi = 300)

# ==============================================================================
# Map 2: High-Frequency Hotspots Only (3 or more)
# ==============================================================================

message("Generating high-frequency heatmap (3+ hotspots)...")

sf_data_3plus <- sf_data %>% filter(hotspot_count >= 3)

p_3plus <- ggplot() +
  # Base map underneath
  geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
  # color = NA ensures the 10km grid cell borders do not render, preventing visual clutter
  geom_sf(data = sf_data_3plus, aes(fill = hotspot_count), color = NA) +
  scale_fill_viridis_c(
    option = "inferno",
    direction = 1,
    limits = c(1, max_count),    # Lock the scale so colors match Map 1 exactly
    breaks = seq(3, max_count), # Clean integer breaks in the legend, starting from 3
    name = "Overlapping\nHotspots"
  ) +
  labs(
    title = "High-Frequency Hotspot Concentrations",
    subtitle = "Locations with at least 3 overlapping hotspots of decline/damage"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm")
  )

out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path_3plus <- file.path(out_dir, "global_hotspot_count_heatmap_3plus.png")

message("Saving map to: ", out_path_3plus)
ggsave(out_path_3plus, p_3plus, width = 16, height = 9, bg = "white", dpi = 300)