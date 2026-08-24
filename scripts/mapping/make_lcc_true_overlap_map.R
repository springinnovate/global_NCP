# ==============================================================================
# TRUE Overlap Map: ES Hotspots ∩ LCC Driver Hotspots
# ==============================================================================
# Companion to global_lcc_driver_hotspots_map.png, which plots ALL LCC driver
# hotspot cells (~9% of the grid) regardless of ES-hotspot status. That map's
# caption claims it shows "only the locations where service declines spatially
# intersect with severe land cover conversion" -- it does not; it is the raw
# driver-hotspot union, unconditioned on ES status.
#
# This script builds the actual intersection: cells that are BOTH in the
# canonical ES-hotspot set (225,113 cells, hotspots_global_pct.gpkg) AND in the
# 5-driver LCC hotspot union (drivers_by_group/pct/global/hotspots_global_pct.gpkg).
# Written to a NEW file -- does not overwrite the existing map -- for side-by-side
# comparison.

library(sf)
library(ggplot2)
library(dplyr)
library(here)
library(rnaturalearth)
library(stringr)

source(here("R", "paths.R"))

out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

es_gpkg  <- file.path(data_dir(), "processed", "hotspots", "pct", "global", "hotspots_global_pct.gpkg")
drv_gpkg <- file.path(data_dir(), "processed", "hotspots", "drivers_by_group", "pct", "global", "hotspots_global_pct.gpkg")

# 1. ES hotspot cell IDs (canonical set, no geometry needed)
message("Reading ES hotspot cell IDs from: ", es_gpkg)
es_fids <- st_read(es_gpkg, query = "SELECT grid_fid FROM hotspots_global_pct", quiet = TRUE)$grid_fid
message("ES hotspot cells: ", length(es_fids))

# 2. LCC driver hotspot cells, WITH geometry and driver classification
message("Reading LCC driver hotspots from: ", drv_gpkg)
drivers_sf <- st_read(drv_gpkg, quiet = TRUE)
message("LCC driver hotspot cells (union of 5 drivers): ", nrow(drivers_sf))

# 3. TRUE intersection: driver-hotspot cells that are ALSO ES-hotspot cells
overlap_sf <- drivers_sf %>% filter(grid_fid %in% es_fids)
message("TRUE overlap cells (ES hotspot AND LCC driver hotspot): ", nrow(overlap_sf))
message("As % of ES hotspots: ", round(100 * nrow(overlap_sf) / length(es_fids), 2), "%")

# 4. Classify by driver type (same scheme as the existing driver map, for visual comparability)
overlap_sf <- overlap_sf %>%
  mutate(
    hotspot_count = coalesce(as.numeric(hotspot_count), 0),
    hotspot_services = coalesce(hotspot_services, ""),
    Driver = case_when(
      hotspot_count > 1 ~ "Multiple Conversion Drivers",
      str_detect(hotspot_services, "Forest_Loss") ~ "Forest Loss",
      str_detect(hotspot_services, "Crop_Exp") ~ "Agricultural Expansion",
      str_detect(hotspot_services, "Urban_Exp") ~ "Urban Expansion",
      TRUE ~ "Other (Grassland only)"
    ),
    Driver = factor(Driver, levels = c("Forest Loss", "Agricultural Expansion", "Urban Expansion",
                                        "Multiple Conversion Drivers", "Other (Grassland only)"))
  )

message("\nDriver breakdown of TRUE overlap cells:")
print(table(overlap_sf$Driver))

# 5. Map
message("\nFixing dateline wraparound artifacts...")
overlap_sf <- st_wrap_dateline(overlap_sf, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
overlap_sf <- st_transform(overlap_sf, crs = "EPSG:8857")
world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_transform(crs = "EPSG:8857")

driver_colors <- c(
  "Forest Loss" = "#2ca25f",
  "Agricultural Expansion" = "#e69138",
  "Urban Expansion" = "#e41a1c",
  "Multiple Conversion Drivers" = "#984ea3",
  "Other (Grassland only)" = "#7f7f7f"
)

n_overlap <- nrow(overlap_sf)
pct_overlap <- round(100 * n_overlap / length(es_fids), 1)

p <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80", linewidth = 0.1) +
  geom_sf(data = overlap_sf, aes(fill = Driver), color = NA, size = 0.6) +
  scale_fill_manual(values = driver_colors, name = "Conversion Driver (at overlap cells)") +
  coord_sf(crs = "+proj=eqearth") +
  labs(
    title = "TRUE Overlap: ES Hotspots ∩ LCC Driver Hotspots",
    subtitle = paste0(n_overlap, " cells (", pct_overlap, "% of the 225,113 ES hotspot cells) — for comparison, ",
                       "the existing driver-hotspot map shows all ", nrow(drivers_sf),
                       " LCC driver hotspot cells regardless of ES status")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.7, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 10, r = 0, b = 10, l = 0)
  )

out_file <- file.path(out_dir, "global_lcc_true_overlap_map.png")
ggsave(out_file, p, width = 16, height = 9, dpi = 300, bg = "white")
message("\nSaved TRUE overlap map to: ", out_file)
