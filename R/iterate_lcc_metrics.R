#' Iterate zonal land cover change extraction over year steps
#'
#' Wraps extract_lcc_metrics() over multiple pairs of SpatRaster objects and returns combined results.
#'
#' @param raster_list A named list of SpatRaster layers, where names correspond to years (e.g., "1992", "1995", ...).
#' @param polygons An sf object with the spatial units.
#' @param id_col Name of the unique identifier column in `polygons`.
#' @param percent Logical. If TRUE, values are in percentage.
#' @param verbose Logical. If TRUE, prints progress.
#' @param mc Logical. If TRUE, uses parallel::mclapply for parallel execution.
#' @param ncores Integer. Number of cores to use if `mc = TRUE`. Default is NULL.
#'
#' @return A single data.frame with land cover change metrics for all year steps and polygon units.
#' @export
#'
#' @examples
#' # iterate_lcc_metrics(raster_list = list("1992" = r1992, "1995" = r1995, ...),
#' #                    polygons = basins_sf,
#' #                    id_col = "id")
#'
iterate_lcc_metrics <- function(raster_list, polygons, id_col = "id", 
                                percent = TRUE, verbose = TRUE, mc = FALSE, ncores = NULL) {
  stopifnot(is.list(raster_list) && all(sapply(raster_list, inherits, "SpatRaster")))
  
  if (!mc && !is.null(ncores)) {
    warning("`ncores` is ignored because `mc = FALSE`.")
  }
  
  years <- names(raster_list)
  year_pairs <- purrr::map2(years[-length(years)], years[-1], ~c(.x, .y))
  
  results <- purrr::map_dfr(year_pairs, function(pair) {
    y1 <- pair[1]; y2 <- pair[2]
    if (verbose) message(glue::glue("Processing {y2}_{y1}..."))
    extract_lcc_metrics(
      r1 = raster_list[[y1]],
      r2 = raster_list[[y2]],
      polygons = polygons,
      id_col = id_col,
      year_step = paste0(y2, "_", y1),
      percent = percent,
      mc = mc,
      ncores = ncores
    )
  })
  
  return(results)
}
