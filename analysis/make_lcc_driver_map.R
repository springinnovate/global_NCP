# ==============================================================================
# Global Land Cover Change Driver Hotspots Map
# ==============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(here)

source(here("R", "paths.R"))

# Path to the global LCC driver hotspots layer (consolidated)
gpkg_path <- file.path(data_dir(), "processed", "hotspots", "drivers_by_group", "pct", "global", "hotspots_global_pct.gpkg")

message("Reading global LCC driver hotspots file...")
if (!file.exists(gpkg_path)) {
  stop("Driver GPKG not found: ", gpkg_path)
}
sf_data <- st_read(gpkg_path, quiet = TRUE)

message("Fixing dateline wraparound artifacts and overwriting input file...")
sf_data <- st_wrap_dateline(sf_data, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
st_write(sf_data, gpkg_path, append = FALSE, delete_dsn = TRUE, quiet = TRUE)

message("Transforming to Equal Earth projection (EPSG:8857)...")
sf_data <- st_transform(sf_data, crs = "EPSG:8857")

message("Reading and transforming base map...")
base_map_path <- "/home/jeronimo/data/global_ncp/vector_basedata/cartographic_ee_r264_correspondence.gpkg"
base_sf <- st_read(base_map_path, quiet = TRUE)
base_sf <- st_transform(base_sf, crs = "EPSG:8857")

# Create a clean categorical variable for the map based on the hotspot_services string
sf_data <- sf_data %>%
  mutate(
    hotspot_count = as.numeric(hotspot_count),
    Driver = case_when(
      hotspot_count > 1 ~ "Multiple Overlapping Drivers",
      str_detect(hotspot_services, "Forest_Loss") ~ "Forest Loss",
      str_detect(hotspot_services, "Crop_Exp") ~ "Cropland Expansion",
      str_detect(hotspot_services, "Urban_Exp") ~ "Urban Expansion",
      TRUE ~ "Other"
    ),
    Driver = factor(Driver, levels = c("Forest Loss", "Cropland Expansion", "Urban Expansion", "Multiple Overlapping Drivers", "Other"))
  )

message("Generating categorical map...")

# Define custom semantic colors
driver_colors <- c(
  "Forest Loss" = "#2ca25f",                  # Forest Green
  "Cropland Expansion" = "#e69138",           # Agricultural Orange
  "Urban Expansion" = "#e41a1c",              # Built-up Red
  "Multiple Overlapping Drivers" = "#984ea3", # Purple
  "Other" = "gray50"
)

p <- ggplot() +
  # Base map underneath
  geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
  # color = NA ensures the grid cell borders do not render, preventing visual clutter
  geom_sf(data = sf_data, aes(fill = Driver), color = NA) +
  scale_fill_manual(values = driver_colors, name = "Primary Land Conversion Driver\n(Top 5% Most Intense)") +
  labs(
    title = "Hotspots of Direct Land Cover Conversion (1992 - 2020)",
    subtitle = "Locations experiencing the most extreme physical landscape transformation"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, "global_lcc_driver_map.png")

message("Saving map to: ", out_path)
ggsave(out_path, p, width = 16, height = 9, bg = "white", dpi = 300)