#' Aggregate trimmed change by group (simple, fast)
#' Keeps facet order from `svc_order` even when adding Global.
#'
#' @param plt_long data.frame with service, abs_chg, pct_chg and the grouping col
#' @param group_col character scalar (e.g., "region_wb")
#' @param cut_q trim quantile for |change| (default 0.999)
#' @param drop_zeros logical; drop rows where both metrics are 0
#' @param svc_order character vector with the *only* services to plot and their order
#' @param include_global logical; add a Global bar per facet
#' @param keep_only_ordered logical; if TRUE (default) drop services not in svc_order
#' @return tibble: service, {group_col}, abs_mean, pct_mean
#' @export
aggregate_change_simple <- function(plt_long, group_col,
                                    cut_q = 0.999,
                                    drop_zeros = TRUE,
                                    svc_order = NULL,
                                    include_global = FALSE,
                                    keep_only_ordered = TRUE) {
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
      .groups  = "drop"
    )
  
  if (isTRUE(drop_zeros)) {
    regs <- regs |>
      dplyr::filter(.data$abs_mean > 0 | .data$pct_mean > 0)
  }
  
  # Facet order control
  if (!is.null(svc_order)) {
    if (isTRUE(keep_only_ordered)) {
      regs <- regs |>
        dplyr::filter(.data$service %in% svc_order)
      regs$service <- factor(regs$service, levels = svc_order)
    } else {
      extras <- setdiff(unique(regs$service), svc_order)
      regs$service <- factor(regs$service, levels = c(svc_order, extras))
    }
  }
  
  if (isTRUE(include_global)) {
    glob <- cells_trim |>
      dplyr::group_by(.data$service) |>
      dplyr::summarise(
        abs_mean = mean(.data$abs_trim, na.rm = TRUE),
        pct_mean = mean(.data$pct_trim, na.rm = TRUE),
        .groups  = "drop"
      )
    # match levels so bind_rows preserves facet order
    if (is.factor(regs$service)) {
      glob$service <- factor(glob$service, levels = levels(regs$service))
    }
    glob[[group_col]] <- "Global"
    regs <- dplyr::bind_rows(regs, glob)
    # re-assert factor levels (safety)
    if (is.factor(regs$service)) {
      regs$service <- factor(regs$service, levels = levels(regs$service))
    }
  }
  
  regs
}
