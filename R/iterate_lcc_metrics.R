#' Iterate zonal land cover change extraction over year steps
#'
#' Automates the calculation of land cover change metrics across a time series of rasters.
#' It constructs pairwise comparisons between consecutive rasters in the provided list
#' (e.g., T1 vs T2, T2 vs T3) and aggregates the results into a single tidy data frame.
#'
#' @param raster_list A named list of `SpatRaster` objects. Names should correspond to time steps (e.g., years "1992", "1995").
#'   The list is processed in the order provided, so ensure it is sorted chronologically if that is the intent.
#' @param polygons An `sf` object containing the spatial units (e.g., grid cells, watersheds) for zonal extraction.
#' @param id_col Character. The name of the column in `polygons` that uniquely identifies each unit.
#' @param percent Logical. If `TRUE` (default), change metrics are calculated as percentages of the polygon area.
#'   If `FALSE`, they are returned in areal units (usually pixel counts or m2 depending on projection).
#' @param verbose Logical. If `TRUE`, prints messages about the pairs being processed.
#' @param mc Logical. If `TRUE`, enables parallel processing using `parallel::mclapply` (Unix/Linux/macOS only).
#' @param ncores Integer. The number of cores to use when `mc = TRUE`. Ignored if `mc = FALSE`.
#' @param digits Integer. Number of decimal places to round the metrics to. Default is 0.
#'
#' @details
#' This function relies on `extract_lcc_metrics` to perform the pairwise comparison.
#' It assumes that `raster_list` contains layers that are aligned and comparable (same CRS, resolution).
#' The `year_step` column in the output will be formatted as "LaterName_EarlierName" (e.g., "1995_1992").
#'
#' @return A data.frame containing the combined change metrics for all time intervals.
#' @export
#'
#' @examples
#' \dontrun{
#' # iterate_lcc_metrics(raster_list = list("1992" = r1992, "1995" = r1995, ...),
#' #                    polygons = basins_sf,
#' #                    id_col = "id")
#' }
#'
iterate_lcc_metrics <- function(raster_list, polygons, id_col = "id",
                                percent = TRUE, verbose = TRUE, mc = FALSE, ncores = NULL,
                                digits = 0) {
  stopifnot(is.list(raster_list) && all(sapply(raster_list, inherits, "SpatRaster")))

  # Validation: Check if id_col exists
  if (!id_col %in% names(polygons)) {
    stop(glue::glue("Column '{id_col}' not found in the provided polygons object."))
  }

  # Validation: Check if list is named
  if (is.null(names(raster_list))) {
    stop("raster_list must be named (e.g. with years) to generate year_step labels.")
  }

  if (!mc && !is.null(ncores)) {
    warning("`ncores` is ignored because `mc = FALSE`.")
  }

  years <- names(raster_list)
  # Create pairs of sequential years (1 vs 2, 2 vs 3, etc.)
  year_pairs <- purrr::map2(years[-length(years)], years[-1], ~c(.x, .y))

  results <- purrr::map_dfr(year_pairs, function(pair) {
    y1 <- pair[1] # Earlier
    y2 <- pair[2] # Later

    if (verbose) message(glue::glue("Processing transition: {y1} -> {y2} (Label: {y2}_{y1})"))

    extract_lcc_metrics(
      r1 = raster_list[[y1]],
      r2 = raster_list[[y2]],
      polygons = polygons,
      id_col = id_col,
      year_step = paste0(y2, "_", y1),
      percent = percent,
      mc = mc,
      ncores = ncores,
      digits = digits
    )
  })

  return(results)
}
