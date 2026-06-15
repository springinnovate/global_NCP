# ==============================================================================
# The Attribution Gap: ES Hotspots vs LCC Drivers Map
#
# OVERVIEW:
# This script generates the primary "Attribution Gap" maps for the Global NCP
# time-series analysis. It visualizes the spatial relationship between Ecosystem
# Service (ES) hotspots and hotspots of physical Land Cover Change (LCC).
#
# WORKFLOW:
# 1.  Loads pre-calculated ES hotspot data (from hotspot_extraction.qmd).
# 2.  Loads pre-calculated LCC driver hotspot data (also from hotspot_extraction.qmd).
# 3.  Joins the two datasets based on the 10km grid cell ID (`grid_fid`).
# 4.  Applies a `case_when` classification logic to attribute each ES hotspot cell
#     to a primary driver based on which LCC hotspots it overlaps with. This
#     logic prioritizes specific, policy-relevant transitions (e.g.,
#     "Deforestation for Cropland") before falling back to general categories.
# 5.  Generates and saves two types of maps to `outputs/plots/maps/`:
#     a. A global map showing attribution for cells with 1, 2, or 3+ ES hotspots.
#     b. A series of individual maps, one for each of the 8 canonical services.
#
# ==============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(here)

source(here("R", "paths.R"))

# --- Configuration ---
# This percentage should match the cutoff used to define LCC hotspots.
hotspot_percent_threshold <- 5

# Define the output directory
out_dir <- here("outputs", "plots", "maps", "attribution_by_service")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load Driver Hotspots - we only need the attributes from the LCC data to do a join
lcc_path <- file.path(data_dir(), "processed", "hotspots", "drivers_by_group", "pct", "global", "hotspots_global_pct.gpkg")
message("Reading LCC drivers...")
lcc_df <- st_read(lcc_path, quiet = TRUE) %>% st_drop_geometry()

# 2. Load base map
message("Reading and transforming base map...")
base_map_path <- file.path(data_dir(), "vector_basedata", "cartographic_ee_r264_correspondence.gpkg")
base_sf <- st_read(base_map_path, quiet = TRUE) %>% st_transform(crs = "EPSG:8857")

# 3. Define attribution colors
attr_colors <- c(
  "Forest Loss" = "#2ca25f",
  "Cropland Expansion" = "#e69138",
  "Grassland Loss" = "#fdbf6f",
  "Urban Expansion" = "#e41a1c",
  "Multiple Conversion Drivers" = "#984ea3",
  "Change Occuring Off-Pixel" = "#3182bd",
  "Deforestation for Cropland" = "#b15928",
  "Savannization / Pasture" = "#ffff99",
  "Grassland to Cropland" = "#a65628"
)

canon_order <- c("C_Risk", "N_export", "Sed_export",
                 "C_Risk_Red_Ratio", "N_Ret_Ratio", "Sed_Ret_Ratio",
                 "Pollination", "Nature_Access")

# Helper function to classify attribution based on LCC services
classify_attribution <- function(lcc_services, lcc_count) {
  case_when(
    # Specific combinations first
    str_detect(lcc_services, "Forest_Loss") & str_detect(lcc_services, "Crop_Exp") ~ "Deforestation for Cropland",
    str_detect(lcc_services, "Forest_Loss") & str_detect(lcc_services, "Grassland_Gain") ~ "Savannization / Pasture",
    str_detect(lcc_services, "Grassland_Loss") & str_detect(lcc_services, "Crop_Exp") ~ "Grassland to Cropland",
    # If multiple drivers but not a special case, use the generic multiple
    lcc_count > 1 ~ "Multiple Conversion Drivers",
    # General, single drivers next
    str_detect(lcc_services, "Forest_Loss") ~ "Forest Loss",
    str_detect(lcc_services, "Grassland_Loss") ~ "Grassland Loss",
    str_detect(lcc_services, "Crop_Exp") ~ "Cropland Expansion",
    str_detect(lcc_services, "Urban_Exp") ~ "Urban Expansion",
    TRUE ~ "Change Occuring Off-Pixel"
  )
}

# 4. Load ES Hotspots, loop through metrics and services, and map
for (metric in c("pct", "abs")) {
  es_path <- file.path(data_dir(), "processed", "hotspots", metric, "global", paste0("hotspots_global_", metric, ".gpkg"))

  if (!file.exists(es_path)) {
    message("\nFile not found, skipping metric ", metric, ": ", es_path)
    next
  }

  message("\nReading ES hotspots for metric: ", toupper(metric), " ...")
  es_sf <- st_read(es_path, quiet = TRUE)

  message("Fixing dateline wraparound artifacts for ES hotspots...")
  es_sf <- st_wrap_dateline(es_sf, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

  # ----------------------------------------------------------------------------
  # 4a. Generate the GLOBAL COMBINED Attribution Map
  # ----------------------------------------------------------------------------
  for (min_es in c(1, 2, 3)) {
    message(paste("Generating GLOBAL attribution map for", toupper(metric), "- Min ES Hotspots:", min_es))

    es_filtered <- es_sf %>% filter(as.numeric(hotspot_count) >= min_es)
    
    if (nrow(es_filtered) == 0) {
      message("  -> No ES hotspots found with count >= ", min_es, ". Skipping.")
      next
    }

    es_global_joined <- es_filtered %>%
      left_join(lcc_df %>% select(grid_fid, lcc_services = hotspot_services, lcc_count = hotspot_count), by = "grid_fid") %>%
      mutate(
        lcc_count = coalesce(as.numeric(lcc_count), 0),
        lcc_services = coalesce(lcc_services, ""),
        Attribution = classify_attribution(lcc_services, lcc_count),
        Attribution = factor(Attribution, levels = names(attr_colors))
      ) %>% st_transform(crs = "EPSG:8857")

    subtitle_text <- paste0("Comparing areas with ", ifelse(min_es == 1, "at least 1 ES Hotspot", paste("at least", min_es, "overlapping ES Hotspots")),
                           " against the Top ", hotspot_percent_threshold, "% of physical Land Conversion")

    p_global <- ggplot() +
      geom_sf(data = base_sf, fill = "gray95", color = NA) +
      geom_sf(data = es_global_joined, aes(fill = Attribution), color = NA) +
      scale_fill_manual(values = attr_colors, name = "Primary Driver of ES Hotspot") +
      labs(title = "The Attribution Gap: Drivers of Ecosystem Service Decline",
           subtitle = subtitle_text) +
      theme_void() +
      theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(b = 20)),
            legend.position = "bottom", legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10),
            legend.key.size = unit(0.75, "cm")) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    out_path_global <- here("outputs", "plots", "maps", paste0("global_attribution_gap_map_min", min_es, "_", metric, ".png"))
    ggsave(out_path_global, p_global, width = 16, height = 9, bg = "white", dpi = 600)
    message("Saving global map to: ", out_path_global)
  }
  # ----------------------------------------------------------------------------
  # 4b. Generate PER-SERVICE Attribution Maps
  # ----------------------------------------------------------------------------
  for (service_name in canon_order) {
    message(paste("Generating attribution map for", service_name, "(", toupper(metric), ")"))

    es_service <- es_sf %>% filter(str_detect(hotspot_services, service_name))

    if (nrow(es_service) == 0) {
      message(paste("  -> No hotspots found for", service_name, "- skipping."))
      next
    }

    es_joined <- es_service %>%
      left_join(lcc_df %>% select(grid_fid, lcc_services = hotspot_services, lcc_count = hotspot_count), by = "grid_fid") %>%
      mutate(
        lcc_count = coalesce(as.numeric(lcc_count), 0),
        lcc_services = coalesce(lcc_services, ""),
        Attribution = classify_attribution(lcc_services, lcc_count),
        Attribution = factor(Attribution, levels = names(attr_colors))
      ) %>% st_transform(crs = "EPSG:8857")

    p <- ggplot() +
      geom_sf(data = base_sf, fill = "gray95", color = NA) +
      geom_sf(data = es_joined, aes(fill = Attribution), color = NA) +
      scale_fill_manual(values = attr_colors, name = "Primary Driver of ES Hotspot") +
      labs(title = paste("Attribution of", service_name, "Hotspots to LCC Drivers"),
           subtitle = paste("Comparing ES Hotspots (", toupper(metric), " Change) against the Top ", hotspot_percent_threshold, "% of physical Land Conversion")) +
      theme_void() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 10)),
            legend.position = "bottom", legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.75, "cm")) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    out_path <- file.path(out_dir, paste0("map_attr_", service_name, "_", metric, ".png"))
    ggsave(out_path, p, width = 10, height = 6, bg = "white", dpi = 600)
    message("Saving map to: ", out_path)
  }
}