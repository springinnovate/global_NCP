# Rasterizes the 5-service hotspot redesign's overlap columns for handoff to
# Rich's wwf_es_beneficiaries pipeline. Mirrors scripts/gdal_rasterize_hotspots.sh
# exactly (EPSG:8857 reprojection, 10km resolution, Byte type, nodata=255, LZW
# compression) so outputs follow the same convention as the existing 8-service
# rasters -- but implemented via sf::gdal_utils() (which calls the same GDAL
# library sf is already linked against) rather than shelling out to gdal_rasterize/
# ogr2ogr binaries, which are not on PATH in this environment.

library(sf)

data_dir_root <- file.path("data", "processed", "hotspots_5service")
out_dir <- file.path(data_dir_root, "rasters")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

columns <- c("hotspot_count", "count_water", "count_access", "combined_cross")
metrics <- c("abs", "pct")

for (metric in metrics) {
  input_gpkg <- file.path(data_dir_root, metric, "global", sprintf("hotspots_global_5service_%s.gpkg", metric))
  reproj_gpkg <- file.path(data_dir_root, metric, "global", sprintf("hotspots_global_5service_%s_epsg8857.gpkg", metric))

  if (!file.exists(input_gpkg)) {
    message("[WARNING] Input file not found: ", input_gpkg, ". Skipping.")
    next
  }

  message("==========================================================")
  message("Processing metric: ", toupper(metric))
  message("==========================================================")

  message("1. Reprojecting to Equal Earth (EPSG:8857)...")
  sf::gdal_utils(
    util = "vectortranslate",
    source = input_gpkg,
    destination = reproj_gpkg,
    options = c("-f", "GPKG", "-t_srs", "EPSG:8857", "-nln", "hotspots", "-overwrite")
  )
  message("Reprojected vector saved to ", reproj_gpkg)

  message("2. Rasterizing columns...")
  for (col in columns) {
    out_tif <- file.path(out_dir, sprintf("%s_%s.tif", col, metric))
    message("   -> Rasterizing ", col, " to ", out_tif, "...")
    sf::gdal_utils(
      util = "rasterize",
      source = reproj_gpkg,
      destination = out_tif,
      options = c(
        "-l", "hotspots", "-a", col,
        "-tr", "10000", "10000",
        "-a_nodata", "255",
        "-ot", "Byte",
        "-co", "COMPRESS=LZW"
      )
    )
  }
  message("Rasterization complete for ", toupper(metric), ".")
}

message("==========================================================")
message("All rasterization tasks finished! Outputs in ", out_dir)
message("==========================================================")
