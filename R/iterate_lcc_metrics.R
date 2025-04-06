#' Iterate zonal land cover change extraction over year steps
#'
#' Wraps extract_zonal_lcc_metrics() over multiple pairs of SpatRaster objects and returns combined results.
#'
#' @param rasters A named list of SpatRaster layers, where names correspond to years (e.g., "1992", "1995", ...).
#' @param polygons An sf object with the spatial units.
#' @param id_col Name of the unique identifier column in `polygons`.
#' @param percent Logical. If TRUE, values are in percentage.
#' @param verbose Logical. If TRUE, prints progress.
#'
#' @return A single data.frame with land cover change metrics for all year steps and polygon units.
#' @export
#'
#' @examples
#' # iterate_lcc_metrics(rasters = list("1992" = r1992, "1995" = r1995, ...),
#' #                           polygons = basins_sf,
#' #                           id_col = "HYBAS_ID")
#'
iterate_lcc_metrics <- function(rasters, polygons, id_col = "HYBAS_ID", 
                                      percent = TRUE, verbose = TRUE) {
  stopifnot(is.list(rasters) && all(sapply(rasters, inherits, "SpatRaster")))
  
  years <- names(rasters)
  year_pairs <- purrr::map2(years[-length(years)], years[-1], ~c(.x, .y))
  
  results <- purrr::map_dfr(year_pairs, function(pair) {
    y1 <- pair[1]; y2 <- pair[2]
    if (verbose) message(glue::glue("Processing {y2}_{y1}..."))
    extract_zonal_lcc_metrics(
      r1 = rasters[[y1]],
      r2 = rasters[[y2]],
      polygons = polygons,
      id_col = id_col,
      year_step = paste0(y2, "_", y1),
      percent = percent
    )
  })
  
  return(results)
}
