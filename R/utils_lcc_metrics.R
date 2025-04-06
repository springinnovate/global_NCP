#' Extract zonal land cover change metrics from raster pairs
#'
#' Applies diffeR::crosstabm and diffeR::difftablej to compute land cover change metrics 
#' between two SpatRaster layers over each polygon unit in an `sf` object.
#'
#' @param r1 A SpatRaster object for time t.
#' @param r2 A SpatRaster object for time t+1.
#' @param polygons An sf object containing the spatial units (e.g., watersheds).
#' @param id_col Name of the unique identifier column in `polygons` (as string).
#' @param year_step A string label for the year transition (e.g., "1995_1992").
#' @param percent Logical. If TRUE, values are in percentage; otherwise, in pixel counts.
#' @param mc Logical. If TRUE, uses mclapply for parallel execution (Unix only). Default is FALSE.
#' @param ncores Integer. Number of cores to use if `mc = TRUE`.
#'
#' @return A data.frame with metrics labeled by ID and year_step.
#' @export
#'
#' @importFrom sf st_geometry st_crop st_as_sf
#' @importFrom terra crop mask values
#' @importFrom diffeR crosstabm diffTablej
#' @importFrom parallel mclapply

extract_lcc_metrics <- function(r1, r2, polygons, id_col = "id", 
                                year_step = NULL, percent = TRUE, mc = FALSE, ncores = NULL) {
  stopifnot(inherits(r1, "SpatRaster"), inherits(r2, "SpatRaster"), inherits(polygons, "sf"))
  stopifnot(nlyr(r1) == 1, nlyr(r2) == 1)
  
  extract_fn <- function(i) {
    poly_i <- polygons[i, ]
    fid <- poly_i[[id_col]]
    
    r1_crop <- mask(crop(r1, poly_i), poly_i)
    r2_crop <- mask(crop(r2, poly_i), poly_i)
    
    if (all(is.na(values(r1_crop))) || all(is.na(values(r2_crop)))) return(NULL)
    
    tab <- crosstabm(r1_crop, r2_crop, percent = percent)
    df <- as.data.frame(diffTablej(tab, analysis = "change"))
    df[[id_col]] <- fid
    df$year_step <- year_step
    return(df)
  }
  
  if (mc) {
    all_results <- parallel::mclapply(1:nrow(polygons), extract_fn, mc.cores = ncores %||% 2)
  } else {
    all_results <- lapply(1:nrow(polygons), extract_fn)
  }
  
  results_df <- do.call(rbind, all_results)
  return(results_df)
}
