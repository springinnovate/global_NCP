# ==============================================================================
# The Attribution Gap: ES Hotspots vs LCC Drivers Map
# ==============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(here)

source(here("R", "paths.R"))

# Define the output directory
out_dir <- here("outputs", "plots", "maps", "attribution_by_service")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load Driver Hotspots - we only need the attributes from the LCC data to do a join
lcc_path <- here("home", "jeronimo", "data", "global_ncp", "processed", "hotspots", "drivers_by_group", "pct", "global", "hotspots_global_pct.gpkg")
message("Reading LCC drivers...")
lcc_df <- st_read(lcc_path, quiet = TRUE) %>% st_drop_geometry()

# 2. Load base map
message("Reading and transforming base map...")
base_map_path <- "/home/jeronimo/data/global_ncp/vector_basedata/cartographic_ee_r264_correspondence.gpkg"
base_sf <- st_read(base_map_path, quiet = TRUE) %>% st_transform(crs = "EPSG:8857")

# 3. Define attribution colors
attr_colors <- c(
  "Forest Loss" = "#2ca25f", # Forest Green
  "Cropland Expansion" = "#e69138", # Agricultural Orange
  "Urban Expansion" = "#e41a1c", # Built-up Red
  "Multiple Conversion Drivers" = "#984ea3", # Purple
  "Unexplained by Top Conversion (Degradation)" = "#3182bd" # Striking Blue to highlight the gap
)

canon_order <- c("C_Risk", "N_export", "Sed_export",
                 "C_Risk_Red_Ratio", "N_Ret_Ratio", "Sed_Ret_Ratio",
                 "Pollination", "Nature_Access")

# 4. Load ES Hotspots, loop through metrics and services, and map
for (metric in c("pct", "abs")) {
  es_path <- here("home", "jeronimo", "data", "global_ncp", "processed", "hotspots", metric, "global", paste0("hotspots_global_", metric, ".gpkg"))

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
  message(paste("Generating GLOBAL attribution map for", toupper(metric)))

  es_global_joined <- es_sf %>%
    left_join(lcc_df %>% select(grid_fid, lcc_services = hotspot_services, lcc_count = hotspot_count), by = "grid_fid") %>%
    mutate(
      lcc_count = coalesce(as.numeric(lcc_count), 0),
      lcc_services = coalesce(lcc_services, ""),
      # Classify the pixel based on which driver is present
      Attribution = case_when(
        lcc_count > 1 ~ "Multiple Conversion Drivers",
        str_detect(lcc_services, "Forest_Loss") ~ "Forest Loss",
        str_detect(lcc_services, "Crop_Exp") ~ "Cropland Expansion",
        str_detect(lcc_services, "Urban_Exp") ~ "Urban Expansion",
        TRUE ~ "Unexplained by Top Conversion (Degradation)"
      ),
      Attribution = factor(Attribution, levels = c("Forest Loss", "Cropland Expansion", "Urban Expansion", "Multiple Conversion Drivers", "Unexplained by Top Conversion (Degradation)"))
    ) %>% st_transform(crs = "EPSG:8857")

  p_global <- ggplot() +
    geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
    geom_sf(data = es_global_joined, aes(fill = Attribution), color = NA) +
    scale_fill_manual(values = attr_colors, name = "Primary Driver of ES Hotspot") +
    labs(title = "The Attribution Gap: Drivers of Ecosystem Service Decline",
         subtitle = paste("Comparing global ES Hotspots (", toupper(metric), " Change) against the Top 5% of physical Land Conversion")) +
    theme_void() +
    theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(b = 20)),
          legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.75, "cm")) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))

  out_path_global <- here("outputs", "plots", "maps", paste0("global_attribution_gap_map_", metric, ".png"))
  ggsave(out_path_global, p_global, width = 16, height = 9, bg = "white", dpi = 300)
  message("Saving global map to: ", out_path_global)

  # ----------------------------------------------------------------------------
  # 4b. Generate PER-SERVICE Attribution Maps
  # ----------------------------------------------------------------------------
  for (service_name in canon_order) {
    message(paste("Generating attribution map for", service_name, "(", toupper(metric), ")"))

    # Filter ES hotspots for the current service
    es_service <- es_sf %>% filter(str_detect(hotspot_services, service_name))

    if (nrow(es_service) == 0) {
      message(paste("  -> No hotspots found for", service_name, "- skipping."))
      next
    }

    # Join by grid_fid and classify attribution
    es_joined <- es_service %>%
      left_join(lcc_df %>% select(grid_fid, lcc_services = hotspot_services, lcc_count = hotspot_count), by = "grid_fid") %>%
      mutate(
        lcc_count = coalesce(as.numeric(lcc_count), 0),
        lcc_services = coalesce(lcc_services, ""),
        # Classify the pixel based on which driver is present
        Attribution = case_when(
          lcc_count > 1 ~ "Multiple Conversion Drivers",
          str_detect(lcc_services, "Forest_Loss") ~ "Forest Loss",
          str_detect(lcc_services, "Crop_Exp") ~ "Cropland Expansion",
          str_detect(lcc_services, "Urban_Exp") ~ "Urban Expansion",
          TRUE ~ "Unexplained by Top Conversion (Degradation)"
        ),
        Attribution = factor(Attribution, levels = c("Forest Loss", "Cropland Expansion", "Urban Expansion", "Multiple Conversion Drivers", "Unexplained by Top Conversion (Degradation)"))
      ) %>% st_transform(crs = "EPSG:8857")

    p <- ggplot() +
      geom_sf(data = base_sf, fill = "gray95", color = "gray80", linewidth = 0.1) +
      geom_sf(data = es_joined, aes(fill = Attribution), color = NA) +
      scale_fill_manual(values = attr_colors, name = "Primary Driver of ES Hotspot") +
      labs(title = paste("Attribution of", service_name, "Hotspots to LCC Drivers"),
           subtitle = paste("Comparing ES Hotspots (", toupper(metric), " Change) against the Top 5% of physical Land Conversion")) +
      theme_void() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 10)),
            legend.position = "bottom", legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.75, "cm")) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    out_path <- file.path(out_dir, paste0("attribution_map_", gsub("[^[:alnum:]]", "_", service_name), "_", metric, ".png"))
    ggsave(out_path, p, width = 10, height = 6, bg = "white", dpi = 300)
    message("Saving map to: ", out_path)
  }
}