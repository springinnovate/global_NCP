#' Compute Percentage Change Between Two Years for Selected Variables
#'
#' Computes the percentage change between specified pairs of columns for each service.
#' Assumes columns follow the naming convention: <prefix>_<year>[<suffix>]
#'
#' @param df A data.frame or sf object containing the variables.
#' @param services Character vector of service prefixes (e.g., "global_n_export").
#' @param year_pairs A list of year pairs (e.g., list(c("1992", "2020"))).
#' @param suffix Optional suffix string (e.g., "_per_ha"). Default is "".
#'
#' @return The input data.frame with new columns for each computed % change.
#' @export
#'
#' @examples
#' compute_pct_change(df, services = c("global_n_export"), year_pairs = list(c("1992", "2020")))
#'
compute_pct_change <- function(df, services, year_pairs, suffix = "") {
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
    }
  }
  return(df)
}

