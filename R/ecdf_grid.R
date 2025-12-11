plot_ecdf_grid <- function(hotspots_df, inverse_df, var_id, top_k = 4,
                           transform = c("identity","log1p"),
                           line_size = 0.9,
                           top_services = NULL) {
  transform <- match.arg(transform, c("identity","log1p"))
  
  base <- dplyr::bind_rows(
    dplyr::mutate(hotspots_df, group = "hotspot"),
    dplyr::mutate(inverse_df,  group = "nonhotspot")
  ) |>
    dplyr::select(service, group, val = .data[[var_id]]) |>
    dplyr::filter(is.finite(.data$val))
  
  if (is.null(top_services)) {
    # pick top-K by D quickly (per var)
    find_D <- function(d) {
      x <- d$val[d$group=="hotspot"]; y <- d$val[d$group=="nonhotspot"]
      if (length(x) < 2 || length(y) < 2) return(NA_real_)
      suppressWarnings(as.numeric(stats::ks.test(x, y, exact = FALSE)$statistic))
    }
    top_services <- base |>
      dplyr::group_by(.data$service) |>
      dplyr::summarise(D = find_D(dplyr::cur_data_all()), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$D)) |>
      dplyr::slice_head(n = top_k) |>
      dplyr::pull(.data$service)
  } else {
    top_services <- unique(top_services)
  }
  
  dd <- dplyr::filter(base, .data$service %in% top_services)
  if (transform == "log1p") dd <- dplyr::mutate(dd, val = log1p(.data$val))
  
  ggplot2::ggplot(
    dd,
    ggplot2::aes(x = .data$val, color = .data$group, group = .data$group)
  ) +
    ggplot2::stat_ecdf(linewidth = line_size, na.rm = TRUE) +
    ggplot2::facet_wrap(~ service, scales = "free_x") +
    ggplot2::scale_color_manual(values = c(nonhotspot = "#2D708E", hotspot = "#D43D51"),
                                guide = ggplot2::guide_legend(title = NULL)) +
    ggplot2::labs(title = paste("ECDF overlays —", var_id,
                                if (transform=="log1p") "(log1p)" else ""),
                  x = var_id, y = "F(value)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top")
}

save_ecdf_grids <- function(
    hotspots_df, inverse_df,
    vars,
    ks_res = NULL,                 # optional: reuse D to pick services
    service_selection = c("top_k","all","custom"),
    services_custom = NULL,        # used when service_selection = "custom"
    top_k = 6,
    transform_default = c("auto","identity","log1p")[1],
    transform_by_var = NULL,       # named chr vec: c(var_name = "log1p", ...)
    line_size = 0.9,
    out_dir = "output_charts/ecdf",
    prefix = "ecdf",
    width = 10, height = 7, dpi = 300,
    use_ragg = TRUE,
    verbose = TRUE
) {
  service_selection <- match.arg(service_selection)
  transform_default <- match.arg(transform_default)
  
  safe_name <- function(x) {
    x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
    gsub("_+", "_", x)
  }
  
  save_png_safe <- function(file, plot, width, height, dpi, bg = "white", use_ragg = TRUE) {
    ok <- FALSE
    if (use_ragg && requireNamespace("ragg", quietly = TRUE)) {
      try({
        ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi, bg = bg,
                        device = ragg::agg_png)
        ok <- TRUE
      }, silent = TRUE)
    }
    if (!ok) {
      ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi, bg = bg,
                      device = "png")
    }
  }
  
  # pick services per var from ks_res (if provided)
  top_by_var <- NULL
  if (!is.null(ks_res)) {
    top_by_var <- ks_res |>
      dplyr::arrange(.data$var, dplyr::desc(.data$D)) |>
      dplyr::group_by(.data$var) |>
      dplyr::summarise(services_all = list(unique(.data$service)), .groups = "drop")
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  decide_transform <- function(v) {
    if (!is.null(transform_by_var) && v %in% names(transform_by_var)) {
      return(match.arg(transform_by_var[[v]], c("identity","log1p")))
    }
    if (transform_default != "auto") {
      return(match.arg(transform_default, c("identity","log1p")))
    }
    # name-based quick guess
    if (grepl("gdp|pop|count|sum|_sum$|_E2020_", v, ignore.case = TRUE)) return("log1p")
    # quick numeric peek
    vv <- dplyr::bind_rows(
      dplyr::transmute(hotspots_df, val = as.numeric(.data[[v]])),
      dplyr::transmute(inverse_df,  val = as.numeric(.data[[v]]))
    )$val
    vv <- vv[is.finite(vv)]
    if (length(vv) > 1000) vv <- vv[sample.int(length(vv), 1000)]
    if (length(vv) == 0) return("identity")
    if (all(vv >= 0, na.rm = TRUE)) {
      q99 <- stats::quantile(vv, 0.99, na.rm = TRUE)
      med <- stats::median(vv[vv > 0], na.rm = TRUE)
      if (is.finite(q99) && (q99 > 1000 || (is.finite(med) && med > 0 && q99/med > 50))) {
        return("log1p")
      }
    }
    return("identity")
  }
  
  out_files <- character(0)
  
  for (v in vars) {
    tfm <- decide_transform(v)
    
    # choose services to facet
    services <- NULL
    if (service_selection == "all") {
      # all services present in either table for this var
      services <- sort(unique(dplyr::bind_rows(
        hotspots_df |> dplyr::select(service),
        inverse_df  |> dplyr::select(service)
      )$service))
    } else if (service_selection == "custom") {
      if (is.null(services_custom) || !length(services_custom)) {
        stop("Provide services_custom when service_selection = 'custom'.")
      }
      services <- unique(services_custom)
    } else { # "top_k"
      if (!is.null(top_by_var)) {
        hit <- top_by_var$services_all[top_by_var$var == v]
        if (length(hit)) {
          services_all <- hit[[1]]
          k <- if (is.infinite(top_k)) length(services_all) else as.integer(top_k)
          k <- max(1L, min(length(services_all), k))
          services <- services_all[seq_len(k)]
        }
      }
      if (is.null(services)) {
        # fallback: compute quick D and take top_k
        base <- dplyr::bind_rows(
          dplyr::mutate(hotspots_df, group = "hotspot"),
          dplyr::mutate(inverse_df,  group = "nonhotspot")
        ) |>
          dplyr::select(service, group, val = .data[[v]]) |>
          dplyr::filter(is.finite(.data$val))
        find_D <- function(d) {
          x <- d$val[d$group=="hotspot"]; y <- d$val[d$group=="nonhotspot"]
          if (length(x) < 2 || length(y) < 2) return(NA_real_)
          suppressWarnings(as.numeric(stats::ks.test(x, y, exact = FALSE)$statistic))
        }
        services <- base |>
          dplyr::group_by(.data$service) |>
          dplyr::summarise(D = find_D(dplyr::cur_data_all()), .groups = "drop") |>
          dplyr::arrange(dplyr::desc(.data$D)) |>
          dplyr::slice_head(n = top_k) |>
          dplyr::pull(.data$service)
      }
    }
    
    p <- plot_ecdf_grid(
      hotspots_df, inverse_df,
      var_id = v,
      top_k = length(services),       # ignored when top_services is provided
      transform = tfm,
      line_size = line_size,
      top_services = services         # <- explicit set of services to facet
    )
    
    suffix <- if (tfm == "log1p") "_log1p" else ""
    file <- file.path(out_dir, paste0(prefix, "_", safe_name(v), suffix, ".png"))
    
    save_png_safe(file, p, width = width, height = height, dpi = dpi, use_ragg = use_ragg)
    if (isTRUE(verbose)) message("Saved: ", file)
    out_files <- c(out_files, file)
  }
  
  invisible(out_files)
}
