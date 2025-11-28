#' Save ECDF overlay grids (hotspots vs non-hotspots) for multiple variables
#'
#' Batch-render and export ECDF overlay plots for a set of variables, faceted by
#' ecosystem service. Each plot shows the empirical CDFs of a chosen variable for
#' hotspot and non-hotspot groups. Services to facet can be selected as the top-K
#' by KS-\eqn{D} per variable (re-using a precomputed KS results table), all services,
#' or a custom list. X-axis transforms (\code{"identity"} or \code{"log1p"}) can be
#' set per variable or chosen automatically for skewed, non-negative variables.
#'
#' @param hotspots_df Data frame of hotspot rows. Must contain a \code{service}
#'   column and the variables listed in \code{vars}.
#' @param inverse_df Data frame of non-hotspot rows (complement), with the same
#'   \code{service} column and variable columns as \code{hotspots_df}.
#' @param vars Character vector of variable (column) names to plot (e.g., GDP, POP).
#' @param ks_res Optional data frame with at least columns \code{var}, \code{service},
#'   and \code{D} (KS statistic). When supplied, the function uses these D values
#'   to choose the top-K services per variable for faceting. If \code{NULL}, a
#'   quick KS-\eqn{D} is computed on the fly from the provided data.
#' @param service_selection One of \code{"top_k"}, \code{"all"}, or \code{"custom"}.
#'   Controls which services appear as facets for each variable.
#' @param services_custom Character vector of services to facet when
#'   \code{service_selection = "custom"}.
#' @param top_k Integer; number of services to facet when
#'   \code{service_selection = "top_k"} (default).
#' @param transform_default One of \code{"auto"}, \code{"identity"}, \code{"log1p"}.
#'   If \code{"auto"}, the function applies a light heuristic (e.g., non-negative,
#'   heavy-tailed totals like GDP/POP get \code{"log1p"}). This transform is
#'   \emph{visual-only} for the ECDF x-axis.
#' @param transform_by_var Optional named character vector mapping variable names
#'   to transforms, e.g. \code{c(gdp = "log1p")}. These overrides take precedence
#'   over \code{transform_default}.
#' @param line_size Numeric line width for the ECDF curves.
#' @param out_dir Output directory for PNG files. Created if missing.
#' @param prefix Filename prefix (e.g., \code{"ecdf"}). Each file is named as
#'   \code{<prefix>_<variable>[_log1p].png}.
#' @param width,height,dpi Numeric PNG size/resolution passed to \code{ggsave()}.
#' @param use_ragg Logical; if \code{TRUE} and \pkg{ragg} is installed, use
#'   \code{ragg::agg_png} for reliable raster output; otherwise falls back to base PNG.
#' @param verbose Logical; if \code{TRUE}, prints the saved file paths.
#'
#' @details
#' The selection of services per variable works as follows:
#' \itemize{
#'   \item If \code{service_selection = "top_k"} and \code{ks_res} is provided,
#'         services are ordered by \code{D} within each \code{var} and the first
#'         \code{top_k} are used.
#'   \item If \code{service_selection = "top_k"} and \code{ks_res} is \code{NULL},
#'         a quick KS-\eqn{D} is computed from the supplied data to pick the top-K.
#'   \item If \code{service_selection = "all"}, all services present in either input
#'         are faceted.
#'   \item If \code{service_selection = "custom"}, \code{services_custom} is used.
#' }
#'
#' The x-axis transform does not affect your KS results; it only improves ECDF
#' readability for skewed variables. Use \code{transform_by_var} for explicit control.
#'
#' @return Invisibly returns a character vector with the paths of the saved PNG files.
#'
#' @examples
#' \dontrun{
#' vars_to_test <- c(
#'   "rast_gdpTot_1990_2020_30arcsec_2020_sum",
#'   "GHS_POP_E2020_GLOBE_sum",
#'   "GlobPOP_Count_30arc_2020_sum"
#' )
#'
#' # (A) Use precomputed KS results to select top-6 services per variable
#' save_ecdf_grids(
#'   hotspots_df, inverse_df,
#'   vars = vars_to_test,
#'   ks_res = ks_res,
#'   service_selection = "top_k",
#'   top_k = 6,
#'   transform_default = "auto",
#'   transform_by_var = c(
#'     "rast_gdpTot_1990_2020_30arcsec_2020_sum" = "log1p",
#'     "GHS_POP_E2020_GLOBE_sum"                 = "log1p"
#'   ),
#'   out_dir = "output_charts/ecdf",
#'   prefix = "ecdf"
#' )
#'
#' # (B) Show all services for each variable, force log1p for everything
#' save_ecdf_grids(
#'   hotspots_df, inverse_df,
#'   vars = vars_to_test,
#'   service_selection = "all",
#'   transform_default = "log1p",
#'   out_dir = "output_charts/ecdf_all",
#'   prefix = "ecdf_all"
#' )
#' }
#'
#' @export
#' @importFrom dplyr bind_rows mutate select filter group_by summarise arrange desc slice_head pull
#' @importFrom ggplot2 ggplot aes stat_ecdf facet_wrap scale_color_manual labs theme_minimal theme ggsave
#' @importFrom stats ks.test quantile median

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
  
  # services per var from ks_res (if provided)
  top_by_var <- NULL
  if (!is.null(ks_res)) {
    top_by_var <- ks_res |>
      dplyr::arrange(.data$var, dplyr::desc(.data$D)) |>
      dplyr::group_by(.data$var) |>
      dplyr::summarise(services_all = list(unique(.data$service)), .groups = "drop")
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  decide_transform <- function(v) {
    # 1) explicit overrides
    if (!is.null(transform_by_var) && v %in% names(transform_by_var)) {
      return(match.arg(transform_by_var[[v]], c("identity","log1p")))
    }
    # 2) forced default
    if (transform_default != "auto") {
      return(match.arg(transform_default, c("identity","log1p")))
    }
    # 3) heuristic: nonnegative + very heavy tail -> log1p
    if (grepl("gdp|pop|count|sum|_sum$|_E2020_", v, ignore.case = TRUE)) return("log1p")
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
      top_services = services
    )
    
    suffix <- if (tfm == "log1p") "_log1p" else ""
    file <- file.path(out_dir, paste0(prefix, "_", safe_name(v), suffix, ".png"))
    
    save_png_safe(file, p, width = width, height = height, dpi = dpi, use_ragg = use_ragg)
    if (isTRUE(verbose)) message("Saved: ", file)
    out_files <- c(out_files, file)
  }
  
  invisible(out_files)
}
