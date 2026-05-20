#' Aggregate trimmed change by group
#'
#' Computes trimmed means of absolute and percent change per service within a
#' grouping (e.g., World Bank region). Extreme cell values are capped at a
#' per-service quantile (trim) before aggregation.
#'
#' @param plt_long A data frame with columns `service`, `abs_chg`, `pct_chg`,
#'   and the grouping column.
#' @param group_col Character scalar naming the grouping column (e.g., "region_wb").
#' @param cut_q Numeric in (0,1); trim quantile for |change| (default `0.999`).
#' @param drop_zeros Logical; drop rows where both metrics are 0 (default `TRUE`).
#' @param svc_order Optional character vector to set facet/order; if provided,
#'   you can restrict to just these services when `keep_only_ordered = TRUE`.
#' @param include_global Logical; if `TRUE` add a “Global” row per service.
#' @param keep_only_ordered Logical; if `TRUE` (default) drop services not in
#'   `svc_order`. Set to `FALSE` to append any extras after your order.
#'
#' @return A tibble with columns: `service`, `{group_col}`, `abs_mean`, `pct_mean`.
#' @export
# Compat shim: normalize arg names regardless of which version is loaded
agg_change <- function(plt_long, group_col,
                       cut_q = 0.999,
                       drop_zeros = TRUE,
                       svc_order = NULL,
                       svc_order_only = TRUE,
                       include_global = FALSE) {
  fmls <- names(formals(aggregate_change_simple))
  if ("include_global" %in% fmls) {
    # package-style signature
    aggregate_change_simple(
      plt_long = plt_long,
      group_col = group_col,
      cut_q = cut_q,
      drop_zeros = drop_zeros,
      svc_order = svc_order,
      keep_only_ordered = svc_order_only,
      include_global = include_global
    )
  } else {
    # GlobalEnv-style signature
    aggregate_change_simple(
      plt_long = plt_long,
      group_col = group_col,
      cut_q = cut_q,
      drop_zeros = drop_zeros,
      svc_order = svc_order,
      svc_order_only = svc_order_only,
      add_global = include_global
    )
  }
}
#' @rdname agg_change
#' @rdname agg_change
#' @export
aggregate_change_simple <- function(...) {
  lifecycle::deprecate_warn("0.9.0",
                            "aggregate_change_simple()",
                            "agg_change()"
  )
  agg_change(...)
}
