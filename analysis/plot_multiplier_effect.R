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

  if (!"combined_total" %in% names(plot_data)) plot_data$combined_total <- 0

  plot_data <- plot_data %>%
    # Join the dynamically calculated Local Residents!
    left_join(local_agg, by = grp_col) %>%
    mutate(
      `Local Residents` = replace_na(`Local Residents`, 0),
      # Connected Beneficiaries = union of downstream + travel-access reach,
      # matching the paper's documented definition (02-methods.qmd) -- NOT
      # travel_footprint alone, which understates true connected reach.
      `Connected Beneficiaries` = combined_total,
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

# Load the 5-service hotspot gpkg alongside the 8-service one already loaded
# above, so water/access/combined-cross categories have a local-population
# source to draw from once Rich delivers those beneficiary folders.
gpkg_5service_path <- here("data", "processed", "hotspots_5service", "pct", "global", "hotspots_global_5service_pct.gpkg")
hotspots_5service_df <- if (file.exists(gpkg_5service_path)) {
  st_read(gpkg_5service_path, quiet = TRUE) %>%
    st_drop_geometry() %>%
    mutate(fid = as.character(grid_fid))
} else {
  message("  -> 5-service hotspot gpkg not found (", gpkg_5service_path, "); water/access/combined categories will be skipped if present.")
  NULL
}

# Local (in-situ) population definition for each overlap_category this
# script knows how to handle -- which source gpkg's identity space to use,
# which column qualifies a cell, and the display label. `overlap_category`
# values come from whatever folders extraction_script.py discovered under
# hotspot_beneficiaries/ (see exposure_comparison_compiled.csv); a category
# present in that data without a matching entry here is skipped with a
# message rather than guessed at, so adding a new beneficiary category (e.g.
# Rich's forthcoming water-hotspot/access-hotspot folders) is a one-line
# addition to this list, not a rewrite of the logic below.
category_defs <- list(
  "all hotspots"          = list(label = ">= 1 Service",           source = "8service", col = "hotspot_count",  min_val = 1),
  "2 or more overlapping" = list(label = ">= 2 Services",          source = "8service", col = "hotspot_count",  min_val = 2),
  "3 or more overlapping" = list(label = ">= 3 Services",          source = "8service", col = "hotspot_count",  min_val = 3),
  "4 or more overlapping" = list(label = ">= 4 Services",          source = "8service", col = "hotspot_count",  min_val = 4),
  "water hotspot"         = list(label = "Water Hotspot",          source = "5service", col = "count_water",    min_val = 1),
  "access hotspot"        = list(label = "Access Hotspot",         source = "5service", col = "count_access",   min_val = 1),
  "combined hotspot"      = list(label = "Combined Cross-Category", source = "5service", col = "combined_cross", min_val = 1)
)

overlap_categories_present <- unique(df$overlap_category)
overlap_categories_present <- overlap_categories_present[overlap_categories_present != "Global"]

compound_local <- lapply(overlap_categories_present, function(cat_name) {
  defn <- category_defs[[cat_name]]
  if (is.null(defn)) {
    message("  -> No local-population definition for overlap_category '", cat_name, "'; skipping (add an entry to category_defs to include it).")
    return(NULL)
  }

  if (defn$source == "8service") {
    if (gpkg_valid && defn$col %in% names(hotspots_df)) {
      valid_fids <- hotspots_df %>%
        filter(.data[[defn$col]] >= defn$min_val) %>%
        pull(fid) %>% unique() %>% as.character()
    } else if (exists("counts")) {
      valid_fids <- grid_sf$fid[counts >= defn$min_val & !is.na(counts)] %>% as.character()
    } else {
      valid_fids <- character(0)
    }
  } else if (defn$source == "5service") {
    if (is.null(hotspots_5service_df) || !defn$col %in% names(hotspots_5service_df)) {
      message("  -> 5-service column '", defn$col, "' unavailable; skipping '", cat_name, "'.")
      return(NULL)
    }
    valid_fids <- hotspots_5service_df %>%
      filter(as.numeric(.data[[defn$col]]) >= defn$min_val) %>%
      pull(fid) %>% unique()
  } else {
    return(NULL)
  }

  local_pop <- grid_df %>%
    filter(fid %in% valid_fids) %>%
    pull(.data[[pop_var]]) %>%
    sum(na.rm = TRUE)

  tibble(overlap_category = cat_name, Label = defn$label, `Local Residents` = local_pop)
}) %>% compact() %>% bind_rows()

## BUG FIX (2026-07-28): exposure_comparison_compiled.csv carries a
## synthesized `country == "Global"` row per overlap_category/exposure_type
## (already equal to the sum of all 224 real countries), alongside the
## per-country rows. The filter below previously summed by overlap_category/
## exposure_type with no country filter at all, so it silently added the
## Global row on top of the correctly-summed per-country total -- exactly
## doubling every value (confirmed: "all hotspots" came out 14.80B instead
## of the true 7.40B). Also switched from `travel_footprint` alone to
## `combined_total` (union of downstream + travel-access), matching the
## paper's documented definition and the verified 7.6B headline figure --
## travel_footprint alone was an under-representation of true connected
## reach. The per-region/income/biome/country loop above was NOT affected by
## the doubling (it already filters out its own grouping column's "Global"
## value) but used the same wrong exposure_type; fixed there too for
## consistency between Figure 9 and the Annex breakdowns it references.
compound_connected <- df %>%
  filter(exposure_type == "combined_total", country != "Global") %>%
  group_by(overlap_category) %>%
  summarise(`Connected Beneficiaries` = sum(population, na.rm = TRUE), .groups = "drop")
  
# Label already assigned per-category above (defn$label); order the factor
# levels by category_defs' own insertion order (nested tiers first, then
# water/access/combined) rather than a separate hardcoded level vector, so a
# newly-added category_defs entry is ordered sensibly without a second edit.
label_order <- vapply(category_defs, function(d) d$label, character(1))
compound_plot_data <- compound_local %>%
  left_join(compound_connected, by = "overlap_category") %>%
  mutate(Label = factor(Label, levels = intersect(label_order, Label)))
  
p_compound <- ggplot(compound_plot_data) +
  geom_segment(
    aes(x = `Local Residents`, xend = `Connected Beneficiaries`, y = Label, yend = Label),
    color = "gray70", linewidth = 1.5
  ) +
  geom_point(aes(x = `Local Residents`, y = Label, color = "Local Residents"), size = 5) +
  geom_point(aes(x = `Connected Beneficiaries`, y = Label, color = "Connected Beneficiaries"), size = 5) +
  scale_x_log10(
    breaks = c(1e8, 3e8, 1e9, 3e9, 1e10),
    labels = label_number(scale_cut = cut_short_scale()),
    limits = c(1e8, 1.2e10)
  ) +
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