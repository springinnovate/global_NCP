# ==============================================================================
# Global Land Cover Change Overview Map (Net Change)
# ==============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(here)
library(rnaturalearth)
library(stringr)

source(here("R", "paths.R"))

# 1. Define Paths
gpkg_path <- file.path(data_dir(), "processed", "10k_lcc_metrics.gpkg")
out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(out_dir, "global_lcc_net_change_map.png")

message("Reading LCC metrics from: ", gpkg_path)
lcc_sf <- st_read(gpkg_path, quiet = TRUE)

# 2. Identify/Calculate Net Change
target_col <- "dir_ch_1_2020_1992"

if (!target_col %in% names(lcc_sf)) {
  stop("Could not find the net change column '", target_col, "'. Available columns: ", paste(names(lcc_sf), collapse = ", "))
}

# 3. Transform and Prepare for Plotting
message("Fixing dateline wraparound artifacts...")
lcc_sf <- st_wrap_dateline(lcc_sf, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

message("Transforming CRS to Equal Earth (EPSG:8857)...")
lcc_sf <- st_transform(lcc_sf, crs = "EPSG:8857")
world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_transform(crs = "EPSG:8857")

# Filter out NAs and calculate robust limits (trimming extreme 1% outliers to prevent washed-out colors)
plot_data <- lcc_sf %>% filter(!is.na(.data[[target_col]]))
limits <- quantile(plot_data[[target_col]], probs = c(0.01, 0.99), na.rm = TRUE)

plot_data <- plot_data %>%
  mutate(plot_val = pmin(pmax(.data[[target_col]], limits[1]), limits[2]))

# 4. Generate Map
message("Generating map...")
p <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white", linewidth = 0.2) +
  geom_sf(data = plot_data, aes(fill = plot_val), color = NA) +
  # Diverging scale anchored at 0: Red for expansion of Transformed land, Green for recovery
  scale_fill_gradient2(name = "Net Conversion Intensity\n(Natural to Transformed)", low = "#007930", mid = "gray95", high = "#E53935", midpoint = 0, na.value = "transparent") +
  coord_sf(crs = "+proj=eqearth") +
  labs(title = "Global Land Cover Conversion (1992 - 2020)", subtitle = "Net change intensity toward human-transformed landscapes") +
  theme_void() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)), legend.position = "bottom", legend.key.width = unit(3, "cm"), panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(t = 10, r = 0, b = 10, l = 0))

ggsave(out_file, p, width = 16, height = 9, dpi = 300, bg = "white")
message("Saved LCC Overview map to: ", out_file)

# ==============================================================================
# 5. Global Land Cover Conversion Hotspots (Drivers) Map
# ==============================================================================
message("\n--- Generating LCC Driver Hotspots Map ---")
gpkg_drivers <- file.path(data_dir(), "processed", "hotspots", "drivers_by_group", "pct", "global", "hotspots_global_pct.gpkg")

if (file.exists(gpkg_drivers)) {
  message("Reading LCC Driver Hotspots from: ", gpkg_drivers)
  drivers_sf <- st_read(gpkg_drivers, quiet = TRUE)
  
  message("Fixing dateline wraparound artifacts for drivers...")
  drivers_sf <- st_wrap_dateline(drivers_sf, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
  drivers_sf <- st_transform(drivers_sf, crs = "EPSG:8857")
  
  message("Classifying LCC Drivers...")
  drivers_sf <- drivers_sf %>%
    mutate(
      hotspot_count = coalesce(as.numeric(hotspot_count), 0),
      hotspot_services = coalesce(hotspot_services, ""),
      Driver = case_when(
        hotspot_count > 1 ~ "Multiple Conversion Drivers",
        str_detect(hotspot_services, "Forest_Loss") ~ "Forest Loss",
        str_detect(hotspot_services, "Crop_Exp") ~ "Agricultural Expansion",
        str_detect(hotspot_services, "Urban_Exp") ~ "Urban Expansion",
        TRUE ~ "Other"
      ),
      Driver = factor(Driver, levels = c("Forest Loss", "Agricultural Expansion", "Urban Expansion", "Multiple Conversion Drivers"))
    ) %>% filter(Driver != "Other")

  driver_colors <- c("Forest Loss" = "#2ca25f", "Agricultural Expansion" = "#e69138", "Urban Expansion" = "#e41a1c", "Multiple Conversion Drivers" = "#984ea3")

  message("Generating map...")
  p_drivers <- ggplot() +
    geom_sf(data = world, fill = "gray95", color = "gray80", linewidth = 0.1) +
    geom_sf(data = drivers_sf, aes(fill = Driver), color = NA) +
    scale_fill_manual(values = driver_colors, name = "Primary Conversion Driver") +
    coord_sf(crs = "+proj=eqearth") + labs(title = "Global Hotspots of Direct Land Cover Conversion", subtitle = "Top 5% of intensive natural land loss driven by specific human activities") + theme_void() + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)), legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 12), legend.key.size = unit(0.75, "cm"), panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(t = 10, r = 0, b = 10, l = 0))

  out_file_drivers <- file.path(out_dir, "global_lcc_driver_hotspots_map.png")
  ggsave(out_file_drivers, p_drivers, width = 16, height = 9, dpi = 300, bg = "white")
  message("Saved LCC Driver Hotspots map to: ", out_file_drivers)
} else { message("WARNING: LCC Driver Hotspots GPKG not found at: ", gpkg_drivers) }