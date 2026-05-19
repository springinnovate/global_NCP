# enrich_grid_attributes.R
#
# A one-time utility script to permanently add full country names to the
# master 10km grid. This resolves the technical debt of relying on `iso3`
# codes and complex, repeated joins in downstream analysis scripts.
#
# Workflow:
# 1. Creates a backup of the original grid.
# 2. Loads the master grid and the country correspondence table.
# 3. Joins the full country name (`nev_name`) using `iso3` as the key.
# 4. Renames `nev_name` to `country` for standardization.
# 5. Overwrites the original grid file with the enriched version.

library(sf)
library(dplyr)
library(here)

# Ensure the project root is correctly identified
if (!file.exists(here::here(".here"))) {
  message("Creating .here file to set project root.")
  file.create(here::here(".here"))
}

# Load project paths helper
source(here::here("R", "paths.R"))

# --- Configuration ---
master_grid_path <- file.path(data_dir(), "vector_basedata", "AOOGrid_10x10km_land_4326_clean.gpkg")
country_corr_path <- file.path(data_dir(), "vector_basedata", "cartographic_ee_r264_correspondence.gpkg")
backup_path <- sub("\\.gpkg$", "_backup.gpkg", master_grid_path)

# --- Safety Checks ---
if (!file.exists(master_grid_path)) {
  stop("FATAL: Master grid not found at: ", master_grid_path)
}
if (!file.exists(country_corr_path)) {
  stop("FATAL: Country correspondence file not found at: ", country_corr_path)
}

# --- 1. Create a backup ---
message("Creating backup of master grid to: ", backup_path)
file.copy(master_grid_path, backup_path, overwrite = TRUE)

# --- 2. Load Data ---
message("Loading master grid...")
master_grid <- st_read(master_grid_path, quiet = TRUE)

message("Loading country correspondence data...")
country_corr <- st_read(country_corr_path, quiet = TRUE) %>%
  st_drop_geometry() %>%
  select(iso3, nev_name)

# --- 3. Enrich and Standardize ---
message("Enriching grid with full country names...")

enriched_grid <- master_grid %>%
  # Remove old country column if it exists to avoid conflicts
  select(-any_of(c("country", "nev_name"))) %>%
  left_join(country_corr, by = "iso3") %>%
  rename(country = nev_name)

# --- 4. Overwrite Original File ---
message("Overwriting original master grid with enriched version...")
st_write(enriched_grid, master_grid_path, delete_dsn = TRUE)

message("Done. The master grid has been successfully updated with the 'country' column.")