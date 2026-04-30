# ==============================================================================
# Global Hotspot Frequency Heatmap & First Look Maps
# ==============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(here)
library(rnaturalearth)

source(here("R", "paths.R"))

out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Wipe out old maps before generating new ones to ensure clean state
old_maps <- list.files(out_dir, pattern = "\\.png$", full.names = TRUE)
if (length(old_maps) > 0) {
  unlink(old_maps)
  message("Cleared ", length(old_maps), " old maps from: ", out_dir)
}

# 1. First Look Map Function
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

# 2. Count Heatmaps Function
generate_count_maps <- function(gpkg_path, metric, base_sf) {
  message("\nProcessing heatmaps for metric: ", metric)
  if (!file.exists(gpkg_path)) {
    message("  -> WARNING: File not found: ", gpkg_path)
    return(NULL)
  }

  sf_data <- st_read(gpkg_path, quiet = TRUE)
  sf_data <- st_transform(sf_data, crs = "EPSG:8857")

  # Map 1: Capped at 4+
  sf_data_cap4 <- sf_data %>%
    mutate(hotspots_capped = pmin(as.numeric(hotspot_count), 4),
           hotspot_label = factor(hotspots_capped, levels = 1:4, labels = c("1", "2", "3", "4+")))
  
  p1 <- ggplot() + geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) + geom_sf(data = sf_data_cap4, aes(fill = hotspot_label), color = NA) + scale_fill_manual(name = "Overlapping\nHotspots", values = c("1" = "#FFD54F", "2" = "#FB8C00", "3" = "#E53935", "4+" = "#800026"), na.value = "gray90", drop = FALSE) + labs(title = "Global Ecosystem Service Hotspot Frequency", subtitle = "Number of overlapping hotspots of decline/damage per 10km grid cell") + theme_void() + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)), legend.position = "bottom", legend.key.width = unit(2.5, "cm"))
  
  out_p1 <- file.path(out_dir, paste0("global_hotspot_count_heatmap_cap4_", metric, ".png"))
  ggsave(out_p1, p1, width = 16, height = 9, bg = "white", dpi = 300)
  message("Saved: ", out_p1)

  # Map 2: 3+ Only
  sf_data_3plus <- sf_data %>% filter(as.numeric(hotspot_count) >= 3) %>% mutate(hotspot_label = factor(pmin(as.numeric(hotspot_count), 4), levels = 1:4, labels = c("1", "2", "3", "4+")))
  p2 <- ggplot() + geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) + geom_sf(data = sf_data_3plus, aes(fill = hotspot_label), color = NA) + scale_fill_manual(name = "Overlapping\nHotspots", values = c("1" = "#FFD54F", "2" = "#FB8C00", "3" = "#E53935", "4+" = "#800026"), na.value = "gray90", drop = FALSE) + labs(title = "High-Frequency Hotspot Concentrations", subtitle = "Locations with at least 3 overlapping hotspots of decline/damage") + theme_void() + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)), legend.position = "bottom", legend.key.width = unit(2.5, "cm"))
  
  out_p2 <- file.path(out_dir, paste0("global_hotspot_count_heatmap_3plus_", metric, ".png"))
  ggsave(out_p2, p2, width = 16, height = 9, bg = "white", dpi = 300)
  message("Saved: ", out_p2)

  # Map 3: Capped at 3+
  sf_data_cap3 <- sf_data %>% mutate(hotspots_capped_3 = pmin(as.numeric(hotspot_count), 3), hotspot_label_3 = factor(hotspots_capped_3, levels = 1:3, labels = c("1", "2", "3+")))
  p3 <- ggplot() + geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) + geom_sf(data = sf_data_cap3, aes(fill = hotspot_label_3), color = NA) + scale_fill_manual(name = "Overlapping\nHotspots", values = c("1" = "#FFD54F", "2" = "#FB8C00", "3+" = "#E53935"), na.value = "gray90", drop = FALSE) + labs(title = "Global Ecosystem Service Hotspot Frequency", subtitle = "Number of overlapping hotspots of decline/damage per 10km grid cell (Capped at 3+)") + theme_void() + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)), legend.position = "bottom", legend.key.width = unit(2.5, "cm"))
  
  out_p3 <- file.path(out_dir, paste0("global_hotspot_count_heatmap_cap3_", metric, ".png"))
  ggsave(out_p3, p3, width = 16, height = 9, bg = "white", dpi = 300)
  message("Saved: ", out_p3)
}

# ==============================================================================
# Execution
# ==============================================================================

# 1. First Look Overview Maps
gpkg_global_pct <- file.path("home", "jeronimo", "data", "global_ncp", "processed", "hotspots", "pct", "global", "hotspots_global_pct.gpkg")
gpkg_global_abs <- file.path("home", "jeronimo", "data", "global_ncp", "processed", "hotspots", "abs", "global", "hotspots_global_abs.gpkg")

generate_first_look_map(gpkg_global_pct, file.path(out_dir, "first_look_map_pct.png"))
generate_first_look_map(gpkg_global_abs, file.path(out_dir, "first_look_map_abs.png"))

# 2. Generate Count Heatmaps
base_map_path <- tryCatch(
  file.path(data_dir(), "vector_basedata", "cartographic_ee_r264_correspondence.gpkg"),
  error = function(e) ""
)

if (base_map_path != "" && file.exists(base_map_path)) {
  message("Reading and transforming custom base map from data directory...")
  base_sf <- st_read(base_map_path, quiet = TRUE)
  base_sf <- st_transform(base_sf, crs = "EPSG:8857")
} else {
  message("Custom base map not found locally. Falling back to rnaturalearth background...")
  base_sf <- ne_countries(scale = "medium", returnclass = "sf")
  base_sf <- st_transform(base_sf, crs = "EPSG:8857")
}

generate_count_maps(gpkg_global_pct, metric = "pct", base_sf = base_sf)
generate_count_maps(gpkg_global_abs, metric = "abs", base_sf = base_sf)