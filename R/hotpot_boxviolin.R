# plot_hotspot_boxviolin.R

#' Faceted hotspot vs non-hotspot box/violin overlays
#'
#' For each service, facet by socio variable and compare hotspot vs non-hotspot
#' distributions using violin and/or boxplot pairs, with optional jitter samples.
#'
#' @param plt_long Hotspot rows (e.g., top/bottom thresholded subset).
#' @param inverse_df Complement rows (non-hotspots).
#' @param socio_labels Named character vector: names are variables to plot; values are facet labels.
#' @param drop_services Character vector of services to exclude.
#' @param trim_p Length-2 numeric, pct_chg quantile trim per service, e.g. c(0.01, 0.99).
#' @param y_trans Optional transform for y: "identity","log10","sqrt","log1p".
#' @param show_violin,show_box,show_jitter Logical toggles for layers.
#' @param violin_trim Logical; if TRUE, trim violin tails.
#' @param jitter_frac_nonhotspot Numeric in [0,1], fraction of non-hotspot rows to jitter per facet.
#' @param jitter_max_hotspot Optional cap on hotspot jitter points per facet (NULL = all).
#' @param width,height,dpi Figure size for ggsave.
#' @param out_dir Output directory.
#' @param filename_suffix File suffix.
#' @param run_id Optional string stamped into filename and caption.
#' @param use_ragg Logical; if TRUE, use ragg::agg_png for reliability.
#' @param hide_inline Logical; hide inline plots when knitting.
#' @param print_interactive Logical; print plots when interactive and not knitting.
#' @return Invisibly, vector of written file paths.
#' @export
plot_hotspot_boxviolin <- function(
    plt_long, inverse_df, socio_labels,
    drop_services = NULL,
    trim_p = c(0.01, 0.99),
    y_trans = c("identity","log10","sqrt","log1p")[1],
    show_violin = TRUE,
    show_box = TRUE,
    show_jitter = FALSE,
    violin_trim = TRUE,
    jitter_frac_nonhotspot = 0.02,
    jitter_max_hotspot = 2000,
    width = 10, height = 6, dpi = 300,
    out_dir = "output_charts",
    filename_suffix = "_boxviolin.png",
    run_id = NULL,
    use_ragg = TRUE,
    hide_inline = TRUE,
    print_interactive = TRUE
) {
  if (hide_inline && isTRUE(getOption("knitr.in.progress"))) {
    knitr::opts_chunk$set(fig.show = "hide")
  }
  
  socio_vars <- names(socio_labels)
  if (!is.null(drop_services) && length(drop_services)) {
    inverse_df <- dplyr::filter(inverse_df, !.data$service %in% drop_services)
  }
  
  # tag and combine
  hot_df  <- dplyr::mutate(plt_long,   hotspot_flag = "Hotspot")
  non_df  <- dplyr::mutate(inverse_df, hotspot_flag = "Non-hotspot")
  both_df <- dplyr::bind_rows(hot_df, non_df)
  
  # long, numeric coercion, common pct_chg trim per service
  df_long <- both_df %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(socio_vars),
      names_to = "socio_var", values_to = "socio_val"
    ) %>%
    dplyr::mutate(
      pct_chg   = as.numeric(.data$pct_chg),
      socio_val = as.numeric(.data$socio_val)
    ) %>%
    dplyr::filter(is.finite(.data$pct_chg), is.finite(.data$socio_val)) %>%
    dplyr::group_by(.data$service) %>%
    dplyr::mutate(
      pct_low  = stats::quantile(.data$pct_chg, trim_p[1], na.rm = TRUE),
      pct_high = stats::quantile(.data$pct_chg, trim_p[2], na.rm = TRUE)
    ) %>% dplyr::ungroup() %>%
    dplyr::filter(.data$pct_chg >= .data$pct_low, .data$pct_chg <= .data$pct_high) %>%
    dplyr::mutate(socio_label = socio_labels[as.character(.data$socio_var)])
  
  # drop panels with no variance to avoid violin warnings
  drop_flat_facets <- function(d) {
    d %>% dplyr::group_by(.data$socio_label, .data$hotspot_flag) %>%
      dplyr::filter(dplyr::n_distinct(.data$socio_val) > 1) %>%
      dplyr::ungroup()
  }
  
  safe_name <- function(x) {
    x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
    gsub("_+", "_", x)
  }
  
  if (is.null(run_id)) run_id <- paste0("run", format(Sys.time(), "%Y%m%d_%H%M%S"))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  services <- unique(df_long$service)
  out_files <- character(0)
  
  for (svc in services) {
    d <- df_long %>% dplyr::filter(.data$service == svc)
    if (nrow(d) == 0) next
    
    d_violin <- drop_flat_facets(d)
    
    # jitter samples (optional)
    jitter_d <- NULL
    if (show_jitter) {
      set.seed(42)
      nh <- d %>% dplyr::filter(.data$hotspot_flag == "Non-hotspot") %>%
        dplyr::group_by(.data$socio_label) %>%
        dplyr::slice_sample(prop = jitter_frac_nonhotspot) %>% dplyr::ungroup()
      hs <- d %>% dplyr::filter(.data$hotspot_flag == "Hotspot")
      if (!is.null(jitter_max_hotspot)) {
        hs <- hs %>% dplyr::group_by(.data$socio_label) %>%
          dplyr::slice_sample(n = min(jitter_max_hotspot, dplyr::n()), replace = FALSE) %>%
          dplyr::ungroup()
      }
      jitter_d <- dplyr::bind_rows(nh, hs)
    }
    
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$hotspot_flag, y = .data$socio_val)) +
      { if (show_violin) ggplot2::geom_violin(
        data = d_violin,
        ggplot2::aes(fill = .data$hotspot_flag),
        trim = violin_trim, color = "grey30", linewidth = 0.2,
        width = 0.9, alpha = 0.7
      ) } +
      { if (show_box) ggplot2::geom_boxplot(
        ggplot2::aes(fill = .data$hotspot_flag),
        width = 0.25, outlier.shape = NA,
        color = "grey10", linewidth = 0.3,
        alpha = 0.9, position = ggplot2::position_dodge(width = 0.8)
      ) } +
      { if (show_jitter) ggplot2::geom_point(
        data = jitter_d,
        size = 0.3, alpha = 0.25,
        position = ggplot2::position_jitter(width = 0.15, height = 0, seed = 1),
        color = "grey10"
      ) } +
      ggplot2::scale_fill_manual(
        values = c("Non-hotspot" = "#2D708EFF",  # viridis-ish blue
                   "Hotspot"     = "#D43D51FF")  # warm red
      ) +
      ggplot2::facet_wrap(~ socio_label, scales = "free_y", nrow = 2) +
      ggplot2::labs(
        title = paste("Hotspot vs non-hotspot distributions:", svc),
        x = NULL, y = "Socioeconomic value",
        caption = paste0("id=", run_id, " | trim=", paste(trim_p, collapse = ","), " | y_trans=", y_trans)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text   = ggplot2::element_text(face = "bold"),
        plot.title   = ggplot2::element_text(hjust = 0.5),
        axis.text.x  = ggplot2::element_text(size = 9),
        legend.title = ggplot2::element_blank(),
        legend.position = "right",
        plot.caption = ggplot2::element_text(size = 7, color = "grey30")
      )
    
    # y transform if requested
    if (!is.null(y_trans) && y_trans != "identity") {
      if (y_trans == "log1p") {
        p <- p + ggplot2::scale_y_continuous(trans = "log1p")
      } else {
        p <- p + ggplot2::scale_y_continuous(trans = y_trans)
      }
    }
    
    png_file <- file.path(out_dir, paste0(safe_name(svc), "_", run_id, filename_suffix))
    if (use_ragg) {
      ggplot2::ggsave(png_file, p, width = width, height = height, dpi = dpi, bg = "white",
                      device = ragg::agg_png)
    } else {
      ggplot2::ggsave(png_file, p, width = width, height = height, dpi = dpi, bg = "white")
    }
    
    if (print_interactive && interactive() && !isTRUE(getOption("knitr.in.progress"))) print(p)
    out_files <- c(out_files, png_file)
    rm(p); gc()
  }
  
  invisible(out_files)
}
