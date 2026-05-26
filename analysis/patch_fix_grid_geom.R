# This is a standalone patch script to fix geometry issues in the master grid
# without needing to re-run the entire 2-hour `prepare_data.qmd` script.
# It reads the existing GPKG, applies robust cleaning, and overwrites it.

library(sf)
library(dplyr)
library(glue)

# This assumes you are running the script from the project root directory.
# It points to the file that needs to be fixed.
gpkg_path <- file.path(
  Sys.getenv("GLOBAL_NCP_DATA", unset = "~/data/global_ncp"),
  "vector_basedata",
  "AOOGrid_10x10km_land_4326_clean.gpkg"
)

if (!file.exists(gpkg_path)) {
  stop("The target GPKG file does not exist: ", gpkg_path)
}

message("Reading the GPKG file to be patched: ", gpkg_path)
grid_sf <- sf::st_read(gpkg_path, quiet = TRUE)
message(glue::glue("Read {nrow(grid_sf)} features."))

message("Applying geometry validation and repair. This may take a moment...")

# Apply a zero-buffer to force geometry reconstruction, which is very effective
# at fixing stubborn issues like invalid linear rings.
grid_sf <- sf::st_buffer(grid_sf, dist = 0)

message(glue::glue("Geometries cleaned. Overwriting the original file..."))
sf::st_write(grid_sf, gpkg_path, delete_dsn = TRUE)

message("Patch complete. The file has been successfully cleaned and overwritten.")
message("You can now re-run the Python pipeline.")