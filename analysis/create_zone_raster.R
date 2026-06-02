# This script creates a "zone raster" from the canonical 10km vector grid.
# The Python pipeline can then use this raster for zonal statistics, completely
# bypassing the vector-based geometry issues that have caused long delays.

library(sf)
library(terra)
library(glue)

# --- Configuration ---
# This assumes you are running the script from the project root directory.
# It points to the clean vector grid we want to rasterize.
grid_path <- file.path(
  Sys.getenv("GLOBAL_NCP_DATA", unset = "~/data/global_ncp"),
  "vector_basedata",
  "landgrid_1_clean_enriched.gpkg"
)

# We need a reference raster to define the exact grid (resolution, extent, CRS)
# for the output. This ensures perfect alignment. We'll use one of the raw
# land cover rasters for this, as it covers the globe at the correct resolution.
reference_raster_path <- file.path(
  Sys.getenv("GLOBAL_NCP_DATA", unset = "~/data/global_ncp"),
  "raw", "LandCovers",
  "C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.tif"
)

# Output path for the new zone raster
out_raster_path <- file.path(
  Sys.getenv("GLOBAL_NCP_DATA", unset = "~/data/global_ncp"),
  "processed",
  "10km_zone_raster_fids.tif"
)

# --- Validation & Execution ---
if (!file.exists(grid_path)) stop("The source vector grid does not exist: ", grid_path)
if (!file.exists(reference_raster_path)) stop("The reference raster does not exist: ", reference_raster_path)
dir.create(dirname(out_raster_path), recursive = TRUE, showWarnings = FALSE)

message("Loading source vector grid: ", basename(grid_path))
grid_sf <- sf::st_read(grid_path, quiet = TRUE)

if ("orig_fid" %in% names(grid_sf) && !"fid" %in% names(grid_sf)) names(grid_sf)[names(grid_sf) == "orig_fid"] <- "fid"
if ("grid_fid" %in% names(grid_sf) && !"fid" %in% names(grid_sf)) names(grid_sf)[names(grid_sf) == "grid_fid"] <- "fid"
if ("id" %in% names(grid_sf) && !"fid" %in% names(grid_sf)) names(grid_sf)[names(grid_sf) == "id"] <- "fid"

if (!"fid" %in% names(grid_sf)) {
  message("No 'fid' column found, generating sequential IDs...")
  grid_sf$fid <- seq_len(nrow(grid_sf))
}

message("Loading reference raster to create a template...")
template_raster <- terra::rast(reference_raster_path)

message(glue::glue("Rasterizing {nrow(grid_sf)} features... This may take a few minutes."))
# The `touches = TRUE` argument is critical. It ensures that any pixel that is
# even partially covered by a vector polygon will be assigned that polygon's
# 'fid' value, preserving the full extent of the land-intersecting grid.
zone_raster <- terra::rasterize(grid_sf, template_raster, field = "fid", touches = TRUE)

message("Writing output zone raster to: ", out_raster_path)
terra::writeRaster(zone_raster, out_raster_path, datatype = "INT4S", overwrite = TRUE, gdal = c("COMPRESS=LZW", "TILED=YES"))

message("Zone raster creation complete. The Python pipeline can now be updated to use this file.")