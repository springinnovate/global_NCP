#' Make bar charts of aggregated change by group
#' @param group_col   character(1) column name in plt_long (e.g., "region_wb")
#' @param stub        file stub for outputs
#' @param svc_order   character vector with desired facet order
#' @param include_global logical; add a "Global" reference bar per facet
#' @param sort_bars   logical; reorder groups within each facet by statistic
#' @param sort_stat   "abs_mean" or "pct_mean"
#' @param sort_desc   logical; descending sort if TRUE
#' @param save,show   write files and/or print to device
#' @param out_dir     output directory
#' @param file_tag    optional suffix to avoid overwrites (auto-adds _with-global if needed)
#' @param pin_global  keep "Global" at the top of the order if present
#' @return list(abs_plot, pct_plot, data)
make_change_bars <- function(group_col, stub,
                             svc_order,
                             include_global = FALSE,
                             sort_bars = TRUE,
                             sort_stat = c("abs_mean","pct_mean"),
                             sort_desc = TRUE,
                             save = TRUE, show = FALSE,
                             out_dir = "outputs/plots",
                             file_tag = NULL,
                             pin_global = TRUE) {
  sort_stat <- match.arg(sort_stat)
  
  regs <- aggregate_change_simple(
    plt_long,
    group_col = group_col,
    cut_q = 0.999,
    drop_zeros = TRUE,
    svc_order = svc_order,
    add_global = include_global
  )
  
  # reorder groups within each facet
  regs <- regs %>%
    dplyr::mutate(.grp = .data[[group_col]]) %>%
    dplyr::group_by(.data$service) %>%
    dplyr::mutate(
      grp_fac = if (sort_bars) {
        forcats::fct_reorder(.grp, .data[[sort_stat]], .desc = sort_desc)
      } else {
        forcats::as_factor(.grp)
      }
    ) %>%
    dplyr::ungroup()
  
  if (include_global && pin_global && "Global" %in% regs$grp_fac) {
    regs$grp_fac <- forcats::fct_relevel(regs$grp_fac, "Global", after = 0L)
  }
  
  p_abs <- ggplot2::ggplot(regs, ggplot2::aes(x = grp_fac, y = abs_mean)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ service, ncol = 3, scales = "free_y") +
    ggplot2::labs(
      title = paste0("Total absolute change (trimmed) by ", group_col),
      x = NULL, y = "Mean |absolute change|"
    ) +
    ggplot2::theme_minimal(base_size = 11)
  
  p_pct <- ggplot2::ggplot(regs, ggplot2::aes(x = grp_fac, y = pct_mean)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ service, ncol = 3, scales = "free_y") +
    ggplot2::labs(
      title = paste0("Mean |percent change| (trimmed) by ", group_col),
      x = NULL, y = "Mean |percent change|"
    ) +
    ggplot2::theme_minimal(base_size = 11)
  
  if (save) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    tag <- if (!is.null(file_tag)) paste0("_", file_tag) else if (include_global) "_with-global" else ""
    ggplot2::ggsave(file.path(out_dir, paste0(stub, "_abs_bars", tag, ".png")),
                    p_abs, width = 12, height = 8, dpi = 300)
    ggplot2::ggsave(file.path(out_dir, paste0(stub, "_pct_bars", tag, ".png")),
                    p_pct, width = 12, height = 8, dpi = 300)
  }
  if (show) { print(p_abs); print(p_pct) }
  
  invisible(list(abs_plot = p_abs, pct_plot = p_pct, data = regs))
}

