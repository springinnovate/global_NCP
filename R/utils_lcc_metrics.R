#' Extract zonal land cover change metrics from raster pairs
##' Extract zonal land cover change metrics from raster pairs
#'
#' Applies diffeR::crosstabm and diffeR::diffTablej to compute land cover change metrics 
#' between two SpatRaster layers over each polygon unit in an `sf` object.
#'
#' @param r1 A SpatRaster object for time t.
#' @param r2 A SpatRaster object for time t+1.
#' @param polygons An sf object containing the spatial units (e.g., watersheds).
#' @param id_col Name of the unique identifier column in `polygons` (as string).
#' @param year_step A string label for the year transition (e.g., "1995_1992").
#' @param percent Logical. If TRUE, values are in percentage; otherwise, in pixel counts.
#' @param mc Logical. If TRUE, uses mclapply for parallel execution (Unix only). Default is FALSE.
#' @param ncores Integer. Number of cores to use if `mc = TRUE`. Default is NULL.
#' @param digits Integer. Number of decimal digits to keep in the change metrics. Default is 0.
#'
#' @return A data.frame with metrics labeled by ID and year_step.
#' @export
#'
#' @importFrom sf st_geometry st_crop st_as_sf
#' @importFrom terra crop mask values
#' @importFrom diffeR crosstabm diffTablej
#' @importFrom parallel mclapply

extract_lcc_metrics <- function(r1, r2, polygons, id_col = "id", 
                                year_step = NULL, percent = TRUE, mc = FALSE, 
                                ncores = NULL, digits = 0) {
  stopifnot(inherits(r1, "SpatRaster"), inherits(r2, "SpatRaster"), inherits(polygons, "sf"))
  stopifnot(nlyr(r1) == 1, nlyr(r2) == 1)
  
  extract_fn <- function(i) {
    poly_i <- polygons[i, ]
    fid <- poly_i[[id_col]]
    
    r1_crop <- mask(crop(r1, poly_i), poly_i)
    r2_crop <- mask(crop(r2, poly_i), poly_i)
    
    if (all(is.na(values(r1_crop))) || all(is.na(values(r2_crop)))) return(NULL)
    
    tab <- crosstabm(r1_crop, r2_crop, percent = percent)
    df <- as.data.frame(diffTablej(tab, analysis = "change", digits = digits))
    df[[id_col]] <- fid
    df$year_step <- year_step
    return(df)
  }
  
  if (mc) {
    all_results <- parallel::mclapply(1:nrow(polygons), extract_fn, mc.cores = ncores)
  } else {
    all_results <- lapply(1:nrow(polygons), extract_fn)
  }
  
  results_df <- do.call(rbind, all_results)
  return(results_df)
}

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
#' @param digits Integer. Number of decimal digits to keep in the change metrics (passed to diffTablej). Default is 0.
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
                                percent = TRUE, verbose = TRUE, mc = FALSE, ncores = NULL,
                                digits = 0) {
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
      ncores = ncores,
      digits = digits
    )
  })
  
  return(results)
}
