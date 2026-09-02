# Rasterizes the 5-service (retention/protection) hotspot columns for handoff to Rich's
# beneficiary-buffer pipeline. Mirrors scripts/gdal_rasterize_hotspots.sh's convention
# (EPSG:8857 reprojection, 10km resolution, Byte type, nodata=255, LZW compression) --
# implemented via sf::gdal_utils() (same GDAL library sf already links against) rather
# than shelling out to gdal_rasterize/ogr2ogr binaries, which aren't on PATH here.
#
# Rewritten 2026-09-01 -- the previous version of this script pointed at a stale
# July 28 file (data/processed/hotspots_5service/, old water/access beneficiary
# categories, old export/risk service names) that had nothing to do with the current
# retention/protection redesign; see docs/pipeline_reference.md row B2 for that
# incident.
#
# Two source files, per user decision 2026-09-01 (confirmed against the actual shared
# Drive folder, which turned out to still need water/access/combined_cross -- Rich's own
# beneficiary-buffer configs threshold directly on those columns, see
# docs/hotspots_rasters_data_dictionary.md):
#   1. analysis/hotspot_extraction.qmd's plain output -- hotspot_count + the 5 individual
#      per-service flags.
#   2. scripts/extract_hotspots.R's output (renamed 2026-09-01 from
#      extract_hotspots_5service.R) -- count_water, count_access, combined_cross.
# Both pre-filtered to hotspot-only rows, so rasterizing naturally leaves every other
# cell as nodata -- correct behavior for a "where are the hotspots" raster.

library(sf)

out_dir <- file.path("data", "processed", "hotspots_5service", "rasters_for_rich")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sources <- list(
  list(
    dir_root = file.path("data", "processed", "hotspots"),
    filename_fmt = "hotspots_global_%s.gpkg",
    columns = c("hotspot_count", "N_retention", "Sed_retention", "C_Prot_service", "Pollination", "Nature_Access")
  ),
  list(
    dir_root = file.path("data", "processed", "hotspots_5service"),
    filename_fmt = "hotspots_global_5service_%s.gpkg",
    columns = c("count_water", "count_access", "combined_cross")
  )
)
metrics <- c("abs", "pct")

for (src in sources) {
  for (metric in metrics) {
    input_gpkg <- file.path(src$dir_root, metric, "global", sprintf(src$filename_fmt, metric))
    reproj_gpkg <- file.path(src$dir_root, metric, "global", sub("\\.gpkg$", "_epsg8857.gpkg", sprintf(src$filename_fmt, metric)))

    if (!file.exists(input_gpkg)) {
      stop("Input file not found: ", input_gpkg, " -- run the matching extraction script first.")
    }

    message("==========================================================")
    message("Processing: ", basename(input_gpkg), " (", toupper(metric), ")")
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
    for (col in src$columns) {
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
    message("Rasterization complete for ", basename(input_gpkg), " ", toupper(metric), ".")
  }
}

message("==========================================================")
message("All rasterization tasks finished! Outputs in ", out_dir)
message("==========================================================")
