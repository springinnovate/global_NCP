#' Compute Percentage Change Between Year Pairs for Selected Variables
#'
#' This function computes percentage change between year pairs for specified services
#' or auto-detects all available year pairs if none are specified. It assumes columns
#' follow the naming convention: <service>_<year><suffix>.
#'
#' @param df A data.frame or sf object.
#' @param year_pairs A list of year pairs. Each element should be a character vector of length 2.
#'        If NULL, will auto-detect all valid year pairs for each service.
#' @param services Optional character vector of service prefixes to include. If NULL, uses all detected.
#' @param suffix Optional suffix string (e.g., "_per_ha"). Default is "".
#' @param round_digits Number of digits to round results. Default is NULL (no rounding).
#'
#' @return A data.frame with added percentage change columns.
#' @export
#'
#' @examples
#' compute_pct_change_all(df, year_pairs = list(c("1992", "2020")), services = c("Usle"))
compute_pct_change <- function(df, year_pairs = NULL, services = NULL, suffix = "", round_digits = NULL) {
  cols <- names(df)
  
  # Auto-detect services and years if not provided
  detected <- stringr::str_match(cols, paste0("^(.*)_([0-9]{4})", suffix, "$"))
  col_info <- tibble::tibble(
    col = cols,
    service = detected[, 2],
    year = detected[, 3]
  ) %>% 
    dplyr::filter(!is.na(service), !is.na(year))
  
  if (is.null(services)) {
    services <- unique(col_info$service)
  }
  
  if (is.null(year_pairs)) {
    years <- sort(unique(col_info$year))
    year_pairs <- purrr::map2(years[-length(years)], years[-1], ~c(.x, .y))
  }
  
  for (service in services) {
    for (years in year_pairs) {
      y1 <- years[1]
      y2 <- years[2]
      
      col1 <- paste0(service, "_", y1, suffix)
      col2 <- paste0(service, "_", y2, suffix)
      new_col <- paste0(service, "_pct_ch_", y2, "_", y1, suffix)
      
      if (!all(c(col1, col2) %in% names(df))) {
        warning(paste("Skipping:", new_col, ": missing columns."))
        next
      }
      
      df[[new_col]] <- ((df[[col2]] - df[[col1]]) / df[[col1]]) * 100
      if (!is.null(round_digits)) {
        df[[new_col]] <- round(df[[new_col]], round_digits)
      }
    }
  }
  
  return(df)
}

