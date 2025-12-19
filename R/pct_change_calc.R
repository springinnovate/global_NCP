#' Compute Absolute and/or Percentage Change Between Two Time Points
#'
#' This function computes absolute and/or percentage change between two time points
#' for variables that follow a specific naming convention: `"variable_YYYY_suffix"`, 
#' where `variable` is the service name or indicator, `YYYY` is a 4-digit year, 
#' and `suffix` is a known ending such as `"_sum"` or `"_mean"`.
#'
#' The function detects variable-year pairs based on matching variable prefixes and suffixes.
#' It supports multiple suffix types and returns either absolute change, percentage change,
#' or both.
#'
#' @param df A `data.frame` or `sf` object with time-stamped variable columns.
#' @param suffix A character vector of valid suffixes (e.g., `c("_sum", "_mean")`). 
#'        Used to detect columns to compare. Defaults to `c("_sum", "_mean")`.
#' @param round_digits Integer. If specified, will round the computed values to this number of digits. Default is `NULL` (no rounding).
#' @param drop_columns Logical. If `TRUE`, drops all original columns except for `"fid"`, computed change columns, and geometry. Default is `FALSE`.
#' @param change_type Type of change to compute. One of `"pct"` (percentage), `"abs"` (absolute), or `"both"` (default).
#' @param pct_mode How to compute percent change. `"baseline"` uses `t1 - t0` divided by `t0`
#'        (default). `"symm"` uses a symmetric percent change: `200 * (t1 - t0) / (|t1| + |t0|)`.
#' @param pct_eps Optional numeric threshold. When `abs(denominator) < pct_eps`, percent change is set
#'        to `NA` to avoid unstable values.
#'
#' @return A `data.frame` or `sf` object with added change columns. If `drop_columns = TRUE`, only `"fid"`, the computed columns, and geometry are kept.
#'
#' @export
#'
#' @examples
#' # Default use: computes both pct and abs changes for _sum and _mean columns
#' compute_variable_change(df)
#'
#' # Compute only percentage change
#' compute_variable_change(df, change_type = "pct")
#'
#' # Compute only absolute change and round to 2 digits
#' compute_variable_change(df, change_type = "abs", round_digits = 2)
#'
#' # Use custom suffix (e.g., "_total")
#' compute_variable_change(df, suffix = "_total", change_type = "both")

compute_change <- function(df, 
                           suffix = c("_sum", "_mean"), 
                           round_digits = NULL, 
                           drop_columns = FALSE,
                           change_type = c("both", "pct", "abs"),
                           pct_mode = c("baseline", "symm"),
                           pct_eps = 0) {
  
  change_type <- match.arg(change_type)
  pct_mode <- match.arg(pct_mode)
  
  suffix_pattern <- paste0("(", paste(suffix, collapse = "|"), ")")
  pattern <- paste0("^(.*)_([0-9]{4})", suffix_pattern, "$")
  
  detected <- stringr::str_match(names(df), pattern)
  valid_idx <- which(!is.na(detected[, 1]))
  
  if (length(valid_idx) == 0) {
    warning("No valid columns detected for change calculation.")
    return(df)
  }
  
  col_info <- tibble::tibble(
    full_col = detected[valid_idx, 1],
    prefix   = detected[valid_idx, 2],
    year     = detected[valid_idx, 3],
    suffix   = detected[valid_idx, 4]
  )
  
  new_cols <- c()
  var_groups <- split(col_info, paste0(col_info$prefix, col_info$suffix))
  
  for (group_name in names(var_groups)) {
    group <- var_groups[[group_name]]
    if (nrow(group) < 2) next
    
    group <- dplyr::arrange(group, as.numeric(group$year))
    col1 <- group$full_col[1]
    col2 <- group$full_col[2]
    
    # Keep both prefix and suffix in the base name
    var_base <- paste0(stringr::str_remove(group$prefix[1], "_$"), group$suffix[1])
    
    # Absolute change
    if (change_type %in% c("abs", "both")) {
      new_col_abs <- paste0(var_base, "_abs_chg")
      df[[new_col_abs]] <- df[[col2]] - df[[col1]]
      if (!is.null(round_digits)) {
        df[[new_col_abs]] <- round(df[[new_col_abs]], round_digits)
      }
      new_cols <- c(new_cols, new_col_abs)
    }
    
    # Percentage change
    if (change_type %in% c("pct", "both")) {
      new_col_pct <- paste0(var_base, "_pct_chg")
      if (pct_mode == "baseline") {
        denom <- df[[col1]]
        pct <- ((df[[col2]] - df[[col1]]) / denom) * 100
      } else {
        denom <- abs(df[[col2]]) + abs(df[[col1]])
        pct <- 200 * (df[[col2]] - df[[col1]]) / denom
      }
      if (pct_eps > 0) {
        pct[abs(denom) < pct_eps] <- NA_real_
      }
      df[[new_col_pct]] <- pct
      if (!is.null(round_digits)) {
        df[[new_col_pct]] <- round(df[[new_col_pct]], round_digits)
      }
      new_cols <- c(new_cols, new_col_pct)
    }
  }
  
  # Remove duplicates before subsetting
  new_cols <- unique(new_cols)
  
  if (drop_columns) {
    geom_col <- attr(df, "sf_column") %||% "geometry"
    keep_cols <- c("fid", new_cols, geom_col)
    keep_cols <- keep_cols[keep_cols %in% names(df)]
    df <- df[, keep_cols, drop = FALSE]
  }
  
  return(df)
}


############### Future developments:
# Support more than two years per variable
# Include year pair explicitly in the new column name (e.g., pct_chg_1992_2020)
