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
