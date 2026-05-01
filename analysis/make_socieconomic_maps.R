# ==============================================================================
# Global Socioeconomic Context Maps
# ==============================================================================
#
# Description:
# This script generates a 2x2 faceted-style map showing the global distribution
# of key socioeconomic variables (Population Density, HDI, GDP, GINI) to
# provide context for the equity and attribution analyses.
#
# Data Source: 10k_grid_services_base.gpkg
#
# ==============================================================================

library(sf)
library(ggplot2)
library(scales)
library(dplyr)
library(here)
library(patchwork)
library(rnaturalearth)

source(here("R", "paths.R"))

# 1. Define Paths & Parameters
gpkg_path <- file.path(data_dir(), "interim", "10k_grid_benef.gpkg")
out_dir <- here("outputs", "plots", "maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(out_dir, "global_socioeconomic_context_map.png")

# IMPORTANT: Verify these column names match your GPKG file
vars_to_map <- list(
  Pop = "GHS_POP_E2020_GLOBE_mean",
  HDI = "hdi_raster_predictions_2020_mean",
  GDP = "rast_gdpTot_1990_2020_30arcsec_2020_mean",
  GINI = "rast_adm1_gini_disp_2020_mean"
)

# 2. Load and Prepare Data
message("Reading socioeconomic data from: ", gpkg_path)
if (!file.exists(gpkg_path)) {
  stop("Data file not found. Please check the path: ", gpkg_path)
}
sf_data <- st_read(gpkg_path, quiet = TRUE)

# Check if columns exist (robustly)
expected_vars <- unlist(vars_to_map, use.names = FALSE)
missing_vars <- expected_vars[!expected_vars %in% names(sf_data)]
if (length(missing_vars) > 0) {
  stop("Error: The following required columns were not found in the data file:\n- ", paste(missing_vars, collapse = "\n- "),
       "\n\nAvailable columns include: ", paste(head(names(sf_data), 20), collapse = ", "))
}

message("Fixing dateline wraparound artifacts...")
sf_data <- st_wrap_dateline(sf_data, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

message("Transforming CRS to Equal Earth (EPSG:8857)...")
sf_data <- st_transform(sf_data, crs = "EPSG:8857")
world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_transform(crs = "EPSG:8857")

# 3. Map Generation Function
create_map <- function(data, col, title, palette, trans = "identity", direction = 1, limits = NULL) {
  
  # Filter out NA/zero for log scales
  if (trans == "log10") {
    plot_data <- data %>% filter(!is.na(.data[[col]]) & .data[[col]] > 0)
  } else {
    plot_data <- data %>% filter(!is.na(.data[[col]]))
  }

  ggplot() +
    geom_sf(data = world, fill = "gray90", color = "white", linewidth = 0.2) +
    geom_sf(data = plot_data, aes(fill = .data[[col]]), color = NA) +
    scale_fill_viridis_c(option = palette, name = NULL, trans = trans, direction = direction, limits = limits, oob = scales::squish) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.4, "cm")
    )
}

# 4. Create Individual Maps
p_pop <- create_map(sf_data, vars_to_map$Pop, "Population Density", "inferno", "log10")
p_hdi <- create_map(sf_data, vars_to_map$HDI, "Human Development Index (HDI)", "viridis", limits = c(NA, 1))
p_gdp <- create_map(sf_data, vars_to_map$GDP, "Gross Domestic Product (GDP)", "plasma", "log10")
p_gini <- create_map(sf_data, vars_to_map$GINI, "GINI Index (Inequality)", "magma", limits = c(NA, 1))

# 5. Combine with Patchwork and Save
combined_plot <- (p_pop + p_hdi) / (p_gdp + p_gini) +
  plot_annotation(
    title = "Global Socioeconomic Context",
    subtitle = "Distribution of key beneficiary and equity indicators",
    theme = theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(b = 15)))
  )

ggsave(out_file, combined_plot, width = 16, height = 10, dpi = 300, bg = "white")
message("Saved combined socioeconomic map to: ", out_file)

```
