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
#' Build & (optionally) save faceted bars of mean change by group
# ---- make_change_bars ----
# Build faceted bars of mean change by group; optional global ref; optional sorting
make_change_bars <- function(group_col,
                             stub,
                             svc_order,
                             include_global    = FALSE,  # <- pkg arg name
                             svc_order_only    = TRUE,   # <- your preferred default
                             sort_bars         = TRUE,
                             out_dir           = "outputs/plots",
                             save              = TRUE,
                             show              = FALSE) {
  stopifnot(is.character(group_col), length(group_col) == 1L)
  stopifnot(group_col %in% names(plt_long))
  
  # 1) Aggregate (uses pkg arg names!)
  regs <- aggregate_change_simple(
    plt_long        = plt_long,
    group_col       = group_col,
    cut_q           = 0.999,
    drop_zeros      = TRUE,
    svc_order       = svc_order,
    keep_only_ordered = svc_order_only,   # <- map your flag
    include_global    = include_global    # <- map your flag
  )
  
  # 2) Keep only the services we asked for (defensive)
  regs <- dplyr::filter(regs, .data$service %in% svc_order)
  
  # 3) Service facet order
  regs$service <- factor(regs$service, levels = svc_order)
  
  # 4) Make x labels stable and (optionally) sort within each facet
  grp_sym <- rlang::sym(group_col)
  regs <- regs |>
    dplyr::mutate(!!grp_sym := as.character(.data[[group_col]]))
  
  if (isTRUE(include_global)) {
    regs <- regs |>
      dplyr::mutate(!!grp_sym := forcats::fct_relevel(.data[[group_col]], "Global", after = Inf))
  } else {
    regs <- regs |>
      dplyr::mutate(!!grp_sym := factor(.data[[group_col]]))
  }
  
  if (isTRUE(sort_bars)) {
    regs <- regs |>
      dplyr::group_by(.data$service) |>
      dplyr::mutate(!!grp_sym := forcats::fct_reorder(.data[[group_col]], .data$pct_mean, .fun = mean, .desc = TRUE)) |>
      dplyr::ungroup()
    if (isTRUE(include_global)) {
      regs <- regs |>
        dplyr::mutate(!!grp_sym := forcats::fct_relevel(.data[[group_col]], "Global", after = Inf))
    }
  }
  
  # 5) Plot (percent by default here; switch to abs_mean if you prefer)
  p <- ggplot2::ggplot(regs, ggplot2::aes(x = .data[[group_col]], y = .data$pct_mean)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::facet_wrap(~ service, ncol = 3, scales = "free_y") +
    ggplot2::labs(
      title = paste0("Average trimmed percent change by ", group_col),
      x     = NULL,
      y     = "Mean |percent change| (trimmed)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      strip.text   = ggplot2::element_text(face = "bold")
    )
  
  # 6) Save / show
  if (isTRUE(save)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    suffix <- if (include_global) "_with_global" else ""
    out <- file.path(out_dir, paste0(stub, "_pct_mean_bars", suffix, ".png"))
    ggplot2::ggsave(out, p, width = 12, height = 8, dpi = 300)
  }
  if (isTRUE(show)) print(p)
  
  invisible(p)
}
