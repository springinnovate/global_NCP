#' Extract hotspots by grouping column(s)
#'
#' Splits a long-form change table by one or more grouping columns and runs
#' [extract_hotspots()] on each subset, returning a tibble with the grouping
#' values and a list-column of hotspot rows.
#'
#' @param df_long Long-format table (e.g., `plt_long`) containing `fid`,
#'   `service`, and the supplied `value_col`.
#' @param group_cols Character vector of column names to group by.
#' @param loss Character vector of services “worse when down”.
#' @param gain Character vector of services “worse when up”.
#' @param value_col Name of numeric column to rank (default `pct_chg`).
#' @param pct_cutoff Percentile cutoff (passed to [extract_hotspots()]).
#' @param threshold_mode Hotspot threshold mode (default `"percent"`).
#' @param rule_mode Hotspot rule interpretation (default `"vectors"`).
#' @param combos Optional named list of combo definitions.
#' @param id_cols Optional columns to carry through summaries.
#'
#' @return Tibble with one row per group value and a `hotspots_df` column.
#' @export
extract_hotspots_by <- function(df_long,
                                group_cols,
                                loss,
                                gain,
                                value_col = "pct_chg",
                                pct_cutoff = 0.05,
                                threshold_mode = c("percent", "count"),
                                rule_mode = c("vectors", "explicit"),
                                combos = NULL,
                                id_cols = NULL) {
  threshold_mode <- match.arg(threshold_mode)
  rule_mode <- match.arg(rule_mode)
  stopifnot(all(group_cols %in% names(df_long)))

  nested <- df_long |>
    tidyr::nest(data = -dplyr::all_of(group_cols))

  nested |>
    dplyr::mutate(
      hotspots_df = purrr::map(
        data,
        ~ {
          hs <- extract_hotspots(
            df             = .x,
            value_col      = value_col,
            pct_cutoff     = pct_cutoff,
            threshold_mode = threshold_mode,
            rule_mode      = rule_mode,
            loss_services  = loss,
            gain_services  = gain,
            combos         = combos,
            id_cols        = id_cols,
            sf_obj         = NULL,
            write_sf_path  = NULL,
            clean_names    = FALSE
          )
          hs$hotspots_df
        }
      )
    ) |>
    dplyr::select(-data)
}
