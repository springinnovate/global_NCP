#' Parallel crop, mask, and export/return for raster x polygon combos
#'
#' @param rasters A list of SpatRaster objects.
#' @param polygons A list of polygon objects (either sf or SpatVector).
#' @param raster_names Optional vector of names for rasters.
#' @param outpath Optional string directory path (used if mode = "export" or "both").
#' @param mode One of "return", "export", or "both".
#' @param overwriteRaster Logical. Whether to overwrite existing raster files on disk. Default is TRUE.
#'
#' @return Named list of processed rasters (if mode is "return" or "both")
#' @export
cookie_cutter <- function(rasters,
                          polygons,
                          raster_names = NULL,
                          outpath = NULL,
                          mode = "return",
                          overwriteRaster = TRUE) {
  
  stopifnot(mode %in% c("return", "export", "both"))
  
  if (!is.null(raster_names)) {
    stopifnot(length(raster_names) == length(rasters))
  } else {
    raster_names <- lapply(rasters, function(r) names(r)[1])
  }
  
  if (is.null(names(polygons))) {
    polygon_names <- paste0("poly", seq_along(polygons))
  } else {
    polygon_names <- names(polygons)
  }
  
  # Prepare combinations
  combos <- expand.grid(r_idx = seq_along(rasters),
                        p_idx = seq_along(polygons),
                        stringsAsFactors = FALSE)
  
  plan(sequential)  # Debug mode only
  
  results <- vector("list", nrow(combos))
  
  # Cache of reprojected polygons by CRS WKT string
  proj_cache <- list()
  
  for (i in seq_len(nrow(combos))) {
    r_idx <- combos$r_idx[i]
    p_idx <- combos$p_idx[i]
    
    r <- rasters[[r_idx]]
    p <- polygons[[p_idx]]
    r_name <- raster_names[[r_idx]]
    p_name <- polygon_names[[p_idx]]
    combo_name <- paste0(p_name, "_", r_name)
    
    cat("\nProcessing:", combo_name, "\n")
    
    if (inherits(p, "sf")) {
      p <- try(terra::vect(p), silent = TRUE)
      if (inherits(p, "try-error")) {
        message(glue::glue("[FAIL] {combo_name}: failed to convert sf to vect"))
        next
      }
    }
    
    r_crs <- terra::crs(r, proj = TRUE)
    p_crs <- terra::crs(p, proj = TRUE)
    
    if (r_crs != p_crs) {
      cache_key <- paste0("crs_", digest::digest(r_crs))
      if (!cache_key %in% names(proj_cache)) {
        message(glue::glue("[INFO] Reprojecting {p_name} to match CRS of {r_name}"))
        p_proj <- try(terra::project(p, r_crs), silent = TRUE)
        if (inherits(p_proj, "try-error")) {
          message(glue::glue("[FAIL] {combo_name}: reprojection failed"))
          next
        }
        proj_cache[[cache_key]] <- p_proj
      }
      p <- proj_cache[[cache_key]]
    }
    
    masked <- try(terra::mask(r, p), silent = TRUE)
    if (inherits(masked, "try-error")) {
      message(glue::glue("[FAIL] {combo_name}: mask failed"))
      next
    }
    
    if (all(is.na(terra::values(masked)))) {
      message(glue::glue("[SKIP] {combo_name}: result is all NA"))
      next
    }
    
    trimmed <- try(terra::trim(masked), silent = TRUE)
    if (inherits(trimmed, "try-error")) {
      message(glue::glue("[FAIL] {combo_name}: trim failed"))
      next
    }
    
    names(trimmed) <- combo_name
    
    if (mode %in% c("export", "both")) {
      if (is.null(outpath)) stop("Provide 'outpath' for export mode")
      out_file <- file.path(outpath, paste0(combo_name, ".tif"))
      tryCatch({
        terra::writeRaster(trimmed, out_file, overwrite = overwriteRaster)
        message(glue::glue("[OK] Exported: {out_file}"))
      }, error = function(e) {
        message(glue::glue("[FAIL] {combo_name}: writeRaster failed with error: {e$message}"))
      })
    }
    
    if (mode %in% c("return", "both")) {
      results[[i]] <- trimmed
      names(results)[i] <- combo_name
    }
  }
  
  results <- results[!sapply(results, is.null)]
  
  if (mode %in% c("return", "both")) {
    return(results)
  } else {
    invisible(NULL)
  }
}


