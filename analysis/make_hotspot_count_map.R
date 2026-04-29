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

# Cap hotspot_count at 4 and convert to a discrete factor
sf_data <- sf_data %>%
  mutate(
    hotspots_capped = pmin(as.numeric(hotspot_count), 4),
    hotspot_label = factor(hotspots_capped, levels = 1:4, labels = c("1", "2", "3", "4+"))
  )

message("Generating heatmap...")

p <- ggplot() +
  # Base map underneath
  geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
  # color = NA ensures the 10km grid cell borders do not render, preventing visual clutter
  geom_sf(data = sf_data, aes(fill = hotspot_label), color = NA) +
  scale_fill_manual(
    name = "Overlapping\nHotspots",
    values = c(
      "1" = "#FFD54F",   # Yellow (Warning)
      "2" = "#FB8C00",   # Orange (Elevated)
      "3" = "#E53935",   # Red (High)
      "4+" = "#800026"   # Dark Red (Extreme / Compound Risk)
    ),
    na.value = "gray90",
    drop = FALSE
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

sf_data_3plus <- sf_data %>% filter(as.numeric(hotspot_count) >= 3)

p_3plus <- ggplot() +
  # Base map underneath
  geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
  # color = NA ensures the 10km grid cell borders do not render, preventing visual clutter
  geom_sf(data = sf_data_3plus, aes(fill = hotspot_label), color = NA) +
  scale_fill_manual(
    name = "Overlapping\nHotspots",
    values = c(
      "1" = "#FFD54F",
      "2" = "#FB8C00",
      "3" = "#E53935",
      "4+" = "#800026"
    ),
    na.value = "gray90",
    drop = FALSE
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

# ==============================================================================
# Map 3: Hotspot Frequency (Capped at 3+)
# ==============================================================================

message("Generating heatmap capped at 3+ hotspots...")

sf_data_cap3 <- sf_data %>%
  mutate(
    hotspots_capped_3 = pmin(as.numeric(hotspot_count), 3),
    hotspot_label_3 = factor(hotspots_capped_3, levels = 1:3, labels = c("1", "2", "3+"))
  )

p_cap3 <- ggplot() +
  # Base map underneath
  geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
  geom_sf(data = sf_data_cap3, aes(fill = hotspot_label_3), color = NA) +
  scale_fill_manual(
    name = "Overlapping\nHotspots",
    values = c(
      "1" = "#FFD54F",   # Yellow
      "2" = "#FB8C00",   # Orange
      "3+" = "#E53935"   # Red
    ),
    na.value = "gray90",
    drop = FALSE
  ) +
  labs(
    title = "Global Ecosystem Service Hotspot Frequency",
    subtitle = "Number of overlapping hotspots of decline/damage per 10km grid cell (Capped at 3+)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm")
  )

out_path_cap3 <- file.path(out_dir, "global_hotspot_count_heatmap_cap3.png")

message("Saving map to: ", out_path_cap3)
ggsave(out_path_cap3, p_cap3, width = 16, height = 9, bg = "white", dpi = 300)