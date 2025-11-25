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
# ---- BAR PLOTS (abs or pct) ----------------------------------------------
make_change_bars <- function(group_col,
                             stub,
                             svc_order,
                             metric            = c("pct","abs"),
                             include_global    = FALSE,
                             svc_order_only    = TRUE,
                             sort_bars         = TRUE,
                             cut_q             = 0.999,
                             out_dir           = "outputs/plots",
                             save              = TRUE,
                             show              = FALSE,
                             title_prefix      = NULL) {
  
  stopifnot(is.character(group_col), length(group_col) == 1L)
  stopifnot(group_col %in% names(plt_long))
  metric <- match.arg(metric)
  
  # 1) Aggregate trimmed change (uses your package/helper)
  regs <- agg_change(
    plt_long       = plt_long,
    group_col      = group_col,
    cut_q          = cut_q,
    drop_zeros     = TRUE,
    svc_order      = svc_order,
    svc_order_only = svc_order_only,
    include_global = include_global
  )
  
  # 2) keep only requested services; set facet order
  regs <- dplyr::filter(regs, .data$service %in% svc_order)
  regs$service <- factor(regs$service, levels = svc_order)
  
  # 3) x labels stable + put Global at the end when present
  grp_sym <- rlang::sym(group_col)
  regs <- regs |> dplyr::mutate(!!grp_sym := as.character(.data[[group_col]]))
  if (isTRUE(include_global)) {
    regs <- regs |> dplyr::mutate(!!grp_sym := forcats::fct_relevel(.data[[group_col]], "Global", after = Inf))
  } else {
    regs <- regs |> dplyr::mutate(!!grp_sym := factor(.data[[group_col]]))
  }
  
  # 4) optionally sort bars (within each facet) by the selected y variable
  yvar <- if (metric == "pct") "pct_mean" else "abs_mean"
  if (isTRUE(sort_bars)) {
    regs <- regs |>
      dplyr::group_by(.data$service) |>
      dplyr::mutate(!!grp_sym := forcats::fct_reorder(.data[[group_col]], .data[[yvar]], .fun = mean, .desc = TRUE)) |>
      dplyr::ungroup()
    if (isTRUE(include_global)) {
      regs <- regs |> dplyr::mutate(!!grp_sym := forcats::fct_relevel(.data[[group_col]], "Global", after = Inf))
    }
  }
  
  # 5) Plot
  y_lab  <- if (metric == "pct") "Mean |% change| (trimmed)" else "Mean |absolute change| (trimmed)"
  title  <- if (is.null(title_prefix)) {
    paste0("Average trimmed ", if (metric=="pct") "percent" else "absolute", " change by ", group_col)
  } else title_prefix
  subtitle <- sprintf("Trim: |x| capped at %.1fth percentile per service; %sGlobal included",
                      cut_q*100, if (include_global) "" else "no ")
  
  p <- ggplot2::ggplot(regs, ggplot2::aes(x = .data[[group_col]], y = .data[[yvar]])) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::facet_wrap(~ service, ncol = 3, scales = "free_y") +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = y_lab) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      strip.text  = ggplot2::element_text(face = "bold")
    )
  
  # 6) Save / show
  if (isTRUE(save)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    suffix <- paste0("_", metric, if (include_global) "_with_global" else "")
    outfile <- file.path(out_dir, paste0(stub, "_change_bars", suffix, ".png"))
    ggplot2::ggsave(outfile, p, width = 12, height = 8, dpi = 300)
  }
  if (isTRUE(show)) print(p)
  
  invisible(p)
}
