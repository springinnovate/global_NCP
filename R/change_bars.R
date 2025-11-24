#' Make vertical bar charts of trimmed change by group
#'
#' @param group_col grouping column name (string)
#' @param stub filename stub
#' @param svc_order character vector for facet order (only these are shown by default)
#' @param include_global add a Global bar per facet
#' @param keep_only_ordered default TRUE (hide services not in svc_order)
#' @param sort_bars order x bars within each facet by magnitude
#' @param sort_stat "abs_mean" or "pct_mean"
#' @param sort_desc descending order if TRUE
#' @param save write PNGs
#' @param show print ggplots
#' @param out_dir output directory
#' @param file_tag optional extra suffix for filenames
#' @param width,height,dpi PNG settings
#' @param pin_global if TRUE, place "Global" first on x axis (when present)
#' @export
make_change_bars <- function(group_col, stub,
                             svc_order,
                             include_global = FALSE,
                             keep_only_ordered = TRUE,
                             sort_bars = TRUE,
                             sort_stat = c("abs_mean","pct_mean"),
                             sort_desc = TRUE,
                             save = TRUE, show = FALSE,
                             out_dir = "outputs/plots",
                             file_tag = NULL,
                             width = 12, height = 8, dpi = 300,
                             pin_global = TRUE) {
  sort_stat <- match.arg(sort_stat)
  
  regs <- aggregate_change_simple(
    plt_long,
    group_col         = group_col,
    cut_q             = 0.999,
    drop_zeros        = TRUE,
    svc_order         = svc_order,
    include_global    = include_global,
    keep_only_ordered = keep_only_ordered
  )
  
  # Force grouping labels to character for safe reordering (handles numeric BIOME ids)
  regs <- regs |>
    dplyr::mutate(.grp_chr = as.character(.data[[group_col]])) |>
    dplyr::group_by(.data$service) |>
    dplyr::mutate(
      grp_fac = if (sort_bars) {
        forcats::fct_reorder(.grp_chr, .data[[sort_stat]], .desc = sort_desc)
      } else {
        forcats::as_factor(.grp_chr)
      }
    ) |>
    dplyr::ungroup()
  
  if (include_global && pin_global && any(regs$grp_fac == "Global", na.rm = TRUE)) {
    regs$grp_fac <- forcats::fct_relevel(regs$grp_fac, "Global", after = 0L)
  }
  
  # Vertical bars, facet order from factor levels, free y so scales differ by service
  p_abs <- ggplot2::ggplot(regs, ggplot2::aes(x = grp_fac, y = abs_mean)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ service, ncol = 3, scales = "free_y", drop = TRUE) +
    ggplot2::labs(
      title = paste0("Total absolute change (trimmed) by ", group_col),
      x = NULL, y = "Mean |absolute change|"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
  
  p_pct <- ggplot2::ggplot(regs, ggplot2::aes(x = grp_fac, y = pct_mean)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ service, ncol = 3, scales = "free_y", drop = TRUE) +
    ggplot2::labs(
      title = paste0("Mean |percent change| (trimmed) by ", group_col),
      x = NULL, y = "Mean |percent change|"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
  
  if (save) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    tag <- if (!is.null(file_tag)) paste0("_", file_tag)
    else if (include_global) "_with-global" else "_no-global"
    ggplot2::ggsave(file.path(out_dir, paste0(stub, "_abs_bars", tag, ".png")),
                    p_abs, width = width, height = height, dpi = dpi)
    ggplot2::ggsave(file.path(out_dir, paste0(stub, "_pct_bars", tag, ".png")),
                    p_pct, width = width, height = height, dpi = dpi)
  }
  if (show) { print(p_abs); print(p_pct) }
  
  invisible(list(abs_plot = p_abs, pct_plot = p_pct, data = regs))
}
