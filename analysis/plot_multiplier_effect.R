library(tidyverse)
library(here)
library(scales)
library(sf)

# 1. Load Connected Beneficiaries Data
data_path <- here("outputs", "tables", "exposure_comparison_compiled.csv")
df <- read_csv(data_path, show_col_types = FALSE)

# 2. Dynamically Calculate Local (In-Situ) Population
message("Loading master grid and hotspot flags to calculate Local Residents...")
grid_sf <- st_read(here("data", "processed", "10k_change_calc.gpkg"), quiet = TRUE)

# Dynamically locate the hotspot file (GPKG or TIF)
gpkg_path <- here("data", "processed", "hotspots", "pct", "global", "hotspots_global_pct.gpkg")
tif_path <- here("data", "processed", "hotspots", "pct", "global", "hotspot_count_pct.tif")

gpkg_valid <- FALSE
if (file.exists(gpkg_path)) {
  message("Found hotspot vector GPKG. Reading fids...")
  hotspots_df <- st_read(gpkg_path, quiet = TRUE) %>% st_drop_geometry()
  
  if (!"fid" %in% names(hotspots_df)) {
    if ("orig_fid" %in% names(hotspots_df)) hotspots_df <- dplyr::rename(hotspots_df, fid = orig_fid)
    else if ("grid_fid" %in% names(hotspots_df)) hotspots_df <- dplyr::rename(hotspots_df, fid = grid_fid)
    else if ("id" %in% names(hotspots_df)) hotspots_df <- dplyr::rename(hotspots_df, fid = id)
  }
  if ("fid" %in% names(hotspots_df)) {
    hotspot_fids <- as.character(unique(hotspots_df$fid))
    if (length(hotspot_fids) > 0) gpkg_valid <- TRUE
  }
}
if (!gpkg_valid) {
  if (file.exists(tif_path)) {
    message("GPKG invalid or missing 'fid'. Extracting fids from TIF raster (this may take a minute)...")
    library(terra)
    hotspot_rast <- rast(tif_path)
    if (requireNamespace("exactextractr", quietly = TRUE)) {
      counts <- exactextractr::exact_extract(hotspot_rast, grid_sf, 'max', progress = FALSE)
    } else {
      message("exactextractr not installed. Using terra::extract (slower)...")
      counts <- terra::extract(hotspot_rast, vect(grid_sf), fun = max, na.rm = TRUE, touches = TRUE)[, 2]
    }
    hotspot_fids <- grid_sf$fid[counts > 0 & !is.na(counts)]
  } else {
    stop("Could not find hotspot GPKG or TIF in data/processed/hotspots/pct/global/")
  }
}

grid_df <- st_drop_geometry(grid_sf)

# Standardize 'fid' column naming since older files might use orig_fid or grid_fid
if (!"fid" %in% names(grid_df)) {
  if ("orig_fid" %in% names(grid_df)) grid_df <- dplyr::rename(grid_df, fid = orig_fid)
  else if ("grid_fid" %in% names(grid_df)) grid_df <- dplyr::rename(grid_df, fid = grid_fid)
  else if ("id" %in% names(grid_df)) grid_df <- dplyr::rename(grid_df, fid = id)
}
grid_df$fid <- as.character(grid_df$fid)

# Filter grid to only hotspot cells and extract their 2020 population
pop_var <- "GHS_POP_E2020_GLOBE_sum"
local_pop_df <- grid_df %>%
  filter(fid %in% hotspot_fids) %>%
  # Handle country naming consistency
  mutate(country = if("nev_name" %in% names(.)) nev_name else if("country" %in% names(.)) country else NA_character_) %>%
  select(fid, all_of(pop_var), region_wb, income_grp, WWF_biome, country)

# Define the groupings we want to plot and export
groupings <- c("region_wb", "income_grp", "WWF_biome", "country")

for (grp_col in groupings) {
  
  # 3. Aggregate local population for this specific grouping
  local_agg <- local_pop_df %>%
    filter(!is.na(!!sym(grp_col)), !!sym(grp_col) != "None", !!sym(grp_col) != "nan") %>%
    group_by(!!sym(grp_col)) %>%
    summarise(`Local Residents` = sum(.data[[pop_var]], na.rm = TRUE), .groups = "drop")

  # 4. Prepare Connected Data for Dumbbell Plot
  plot_data <- df %>%
    filter(
      overlap_category == "all hotspots",
      !!sym(grp_col) != "Global",
      !!sym(grp_col) != "nan",
      !!sym(grp_col) != "None",
      !is.na(!!sym(grp_col))
    ) %>%
    # Aggregate across all other dimensions FIRST
    group_by(!!sym(grp_col), exposure_type) %>%
    summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    # Pivot wider to get exposure types in separate columns
    pivot_wider(
      names_from = exposure_type,
      values_from = population,
      values_fill = 0
    )

  if (!"travel_footprint" %in% names(plot_data)) plot_data$travel_footprint <- 0
  if (!"hydrological" %in% names(plot_data)) plot_data$hydrological <- 0

  plot_data <- plot_data %>%
    # Join the dynamically calculated Local Residents!
    left_join(local_agg, by = grp_col) %>%
    mutate(
      `Local Residents` = replace_na(`Local Residents`, 0),
      `Connected Beneficiaries` = travel_footprint,
      multiplier = `Connected Beneficiaries` / ifelse(`Local Residents` == 0, 1, `Local Residents`)
    ) %>%
    # Order regions by their total connected exposure footprint
    arrange(`Connected Beneficiaries`) %>%
    mutate(!!sym(grp_col) := factor(!!sym(grp_col), levels = unique(!!sym(grp_col))))
    
  # EXPORT THE TABLE FOR THE BOOK/PAPER!
  write_csv(plot_data, here("outputs", "tables", paste0("multiplier_summary_", grp_col, ".csv")))
  
  # Skip generating giant messy plots for country-level (just keep the CSV table)
  if (grp_col == "country") next
  
  # 5. Plotting
  p <- ggplot(plot_data) +
    geom_segment(
      aes(x = `Local Residents`, xend = `Connected Beneficiaries`, y = !!sym(grp_col), yend = !!sym(grp_col)),
      color = "gray70", linewidth = 1.5
    ) +
    geom_point(aes(x = `Local Residents`, y = !!sym(grp_col), color = "Local Residents"), size = 4) +
    geom_point(aes(x = `Connected Beneficiaries`, y = !!sym(grp_col), color = "Connected Beneficiaries"), size = 4) +
    
    # Pseudo-log scale allows 0 values without evaluating to -Inf and dropping points
    scale_x_continuous(trans = "pseudo_log", breaks = c(0, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9), labels = label_number(scale_cut = cut_short_scale())) +
    scale_color_manual(values = c("Local Residents" = "#E83737", "Connected Beneficiaries" = "#1F77B4")) +
    
    labs(
      title = "The Serviceshed Multiplier Effect",
      subtitle = paste("Expansion from Local Residents to Total Connected Beneficiaries by", grp_col),
      x = "Exposed Population (Log Scale)",
      y = NULL,
      color = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  ggsave(here("outputs", "plots", paste0("exposure_multiplier_dumbbell_", grp_col, ".png")), p, width = 10, height = 6, bg = "white")
}

# 6. Compound Risk Dumbbell Plot (Global Aggregated)
message("Generating Compound Risk (Aggregated) Dumbbell Plot...")

overlap_mapping <- list(
  "all hotspots" = 1,
  "2 or more overlapping" = 2,
  "3 or more overlapping" = 3,
  "4 or more overlapping" = 4
)

compound_local <- lapply(names(overlap_mapping), function(cat_name) {
  threshold <- overlap_mapping[[cat_name]]
  
  if (gpkg_valid && "hotspot_count" %in% names(hotspots_df)) {
    valid_fids <- hotspots_df %>% 
      filter(hotspot_count >= threshold) %>% 
      pull(fid) %>% unique() %>% as.character()
  } else {
    if (exists("counts")) {
      valid_fids <- grid_sf$fid[counts >= threshold & !is.na(counts)] %>% as.character()
    } else {
      valid_fids <- character(0)
    }
  }
  
  local_pop <- grid_df %>%
    filter(fid %in% valid_fids) %>%
    pull(.data[[pop_var]]) %>%
    sum(na.rm = TRUE)
    
  tibble(overlap_category = cat_name, `Local Residents` = local_pop)
}) %>% bind_rows()

compound_connected <- df %>%
  filter(exposure_type == "travel_footprint") %>%
  group_by(overlap_category) %>%
  summarise(`Connected Beneficiaries` = sum(population, na.rm = TRUE), .groups = "drop")
  
compound_plot_data <- compound_local %>%
  left_join(compound_connected, by = "overlap_category") %>%
  mutate(
    Label = case_when(
      overlap_category == "all hotspots" ~ ">= 1 Service",
      overlap_category == "2 or more overlapping" ~ ">= 2 Services",
      overlap_category == "3 or more overlapping" ~ ">= 3 Services",
      overlap_category == "4 or more overlapping" ~ ">= 4 Services"
    ),
    Label = factor(Label, levels = c(">= 1 Service", ">= 2 Services", ">= 3 Services", ">= 4 Services"))
  )
  
p_compound <- ggplot(compound_plot_data) +
  geom_segment(
    aes(x = `Local Residents`, xend = `Connected Beneficiaries`, y = Label, yend = Label),
    color = "gray70", linewidth = 1.5
  ) +
  geom_point(aes(x = `Local Residents`, y = Label, color = "Local Residents"), size = 5) +
  geom_point(aes(x = `Connected Beneficiaries`, y = Label, color = "Connected Beneficiaries"), size = 5) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(values = c("Local Residents" = "#E83737", "Connected Beneficiaries" = "#1F77B4")) +
  labs(
    title = "Multiplier Effect: Local vs. Connected Population Exposure",
    subtitle = "Log10 scale of exposed population to ecosystem service declines",
    x = "Exposed Population (Log10 Scale)",
    y = "Compound Hotspot Level",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
  
ggsave(here("outputs", "plots", "downstream_exposure_dumbbell_compound.png"), p_compound, width = 10, height = 6, bg = "white")