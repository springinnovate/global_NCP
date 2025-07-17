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
compute_pct_change <- function(df, suffix = c("_sum", "_mean"), round_digits = NULL, drop_columns = FALSE) {
  suffix_pattern <- paste0("(", paste(suffix, collapse = "|"), ")")
  pattern <- paste0("^(.*)_([0-9]{4})", suffix_pattern, "$")
  
  detected <- stringr::str_match(names(df), pattern)
  valid_idx <- which(!is.na(detected[, 1]))
  
  if (length(valid_idx) == 0) {
    warning("No valid columns detected for percentage change.")
    return(df)
  }
  
  col_info <- tibble::tibble(
    full_col = detected[valid_idx, 1],
    prefix = detected[valid_idx, 2],
    year = detected[valid_idx, 3],
    suffix = detected[valid_idx, 4]
  )
  
  new_cols <- c()
  var_groups <- split(col_info, paste0(col_info$prefix, col_info$suffix))
  
  for (group_name in names(var_groups)) {
    group <- var_groups[[group_name]]
    if (nrow(group) < 2) next
    
    group <- dplyr::arrange(group, as.numeric(group$year))
    col1 <- group$full_col[1]
    col2 <- group$full_col[2]
    
    new_col <- paste0(stringr::str_remove(group$prefix[1], "_$"), "_pct_chg")
    
    if (!all(c(col1, col2) %in% names(df))) next
    
    df[[new_col]] <- ((df[[col2]] - df[[col1]]) / df[[col1]]) * 100
    if (!is.null(round_digits)) {
      df[[new_col]] <- round(df[[new_col]], round_digits)
    }
    
    new_cols <- c(new_cols, new_col)
  }
  
  if (drop_columns) {
    geom_col <- attr(df, "sf_column") %||% "geometry"
    keep_cols <- c("fid", new_cols, geom_col)
    keep_cols <- keep_cols[keep_cols %in% names(df)]
    df <- df[, keep_cols, drop = FALSE]
  }
  
  return(df)
}

