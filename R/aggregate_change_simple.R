#' Aggregate trimmed change by group (simple, fast)
#'
#' For each service, trims extreme cell values, then aggregates the mean
#' magnitude of absolute and percent change within each group (e.g., region).
#'
#' @param plt_long Data frame with columns: service, abs_chg, pct_chg, and the grouping column.
#' @param group_col Character scalar naming the grouping column (e.g., "region_wb").
#' @param cut_q Numeric in (0,1); per-service trim quantile for |change|, default 0.999.
#' @param drop_zeros Logical; drop group rows where both metrics are 0, default TRUE.
#' @param svc_order Optional character vector to set facet order; extras are appended.
#' @param add_global Logical; if TRUE add a “Global” row per service.
#'
#' @return Tibble with columns: service, {group_col}, abs_mean, pct_mean.
#' @export
aggregate_change_simple <- function(plt_long, group_col,
                                    cut_q = 0.999,
                                    drop_zeros = TRUE,
                                    svc_order = NULL,
                                    add_global = FALSE) {
  stopifnot(is.character(group_col), length(group_col) == 1L)
  stopifnot(group_col %in% names(plt_long))
  
  cells_trim <- plt_long |>
    dplyr::filter(!is.na(.data[[group_col]])) |>
    dplyr::mutate(
      abs_cell = abs(.data$abs_chg),
      pct_cell = abs(.data$pct_chg)
    ) |>
    dplyr::group_by(.data$service) |>
    dplyr::mutate(
      abs_cap = stats::quantile(.data$abs_cell, cut_q, na.rm = TRUE),
      pct_cap = stats::quantile(.data$pct_cell, cut_q, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      abs_trim = pmin(.data$abs_cell, .data$abs_cap),
      pct_trim = pmin(.data$pct_cell, .data$pct_cap)
    )
  
  regs <- cells_trim |>
    dplyr::group_by(.data$service, .data[[group_col]]) |>
    dplyr::summarise(
      abs_mean = mean(.data$abs_trim, na.rm = TRUE),
      pct_mean = mean(.data$pct_trim, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (isTRUE(drop_zeros)) {
    regs <- regs |> dplyr::filter(.data$abs_mean > 0 | .data$pct_mean > 0)
  }
  
  if (!is.null(svc_order)) {
    extras <- setdiff(unique(regs$service), svc_order)
    regs$service <- factor(regs$service, levels = c(svc_order, extras))
  }
  
  if (isTRUE(add_global)) {
    glob <- cells_trim |>
      dplyr::group_by(.data$service) |>
      dplyr::summarise(
        abs_mean = mean(.data$abs_trim, na.rm = TRUE),
        pct_mean = mean(.data$pct_trim, na.rm = TRUE),
        .groups = "drop"
      )
    glob[[group_col]] <- "Global"
    regs <- dplyr::bind_rows(regs, glob)
  }
  
  regs
}
