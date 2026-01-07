#' Hotspot and Change Analysis Utilities
#'
#' Helper functions for the global NCP pipeline, consolidated from QMD notebooks.
#'
#' - All functions are documented and grouped by purpose.
#' - Functions not currently used in the main workflow are included at the end, flagged for review.
#'
#' @section Main Workflow Functions:
#'   - vmsg: Verbose message printing
#'   - agg_by_group: Aggregate change metrics by group/service
#'   - compute_global_refs: Compute global reference means
#'   - plot_signed_alt: Plot signed change bars (alt variant)
#'   - slug: Clean strings for safe filenames
#'   - run_one_hotset: Extract and export hotspot sets
#'   - rename_change: Clean service change names
#'   - latest_interim: Find latest file matching pattern
#'
#' @section Legacy/Unclear Functions:
#'   - (Cataloged at the end for later review)

# ---- Main Workflow Functions ----

#' Print a message if verbose is TRUE
#' @param ... Arguments passed to message()
vmsg <- function(...) if (isTRUE(verbose)) message(...)

#' Aggregate change metrics by group/service
#' @param df Data frame
#' @param services Character vector of service names
#' @param groupings Character vector of grouping columns
#' @param cut_q Quantile for trimming
#' @param handle_inf How to handle Inf values ("na" or "cap")
#' @param drop_zero Logical, drop zeros if TRUE
agg_by_group <- function(df, services, groupings, cut_q = 0.999,
                         handle_inf = c("na","cap"), drop_zero = FALSE) {
  handle_inf <- match.arg(handle_inf)
  groupings <- intersect(groupings, names(df))
  vmsg("Groupings available: ", paste(groupings, collapse = ", "))
  out <- list()
  for (g in groupings) {
    vmsg("  Grouping: ", g)
    for (svc in services) {
      map_rows <- dplyr::filter(svc_map, canonical == svc)
      if (!nrow(map_rows)) next
      cols_pct <- map_rows$col_pct[map_rows$col_pct %in% names(df)]
      cols_abs <- map_rows$col_abs[map_rows$col_abs %in% names(df)]
      if (!length(cols_pct) && !length(cols_abs)) next

      add_one <- function(col, metric) {
        v <- df[[col]]
        if (metric == "pct" && any(is.infinite(v))) {
          if (handle_inf == "na") v[is.infinite(v)] <- NA_real_
        }
        if (isTRUE(drop_zero)) v[v == 0] <- NA_real_
        cap <- stats::quantile(abs(v), cut_q, na.rm = TRUE)
        v_trim <- pmax(pmin(v, cap), -cap)
        tibble::tibble(
          service = svc,
          group   = df[[g]],
          metric  = metric,
          val     = v_trim
        )
      }

      rows <- list()
      if (length(cols_pct)) rows[[length(rows)+1]] <- add_one(cols_pct[1], "pct")
      if (length(cols_abs)) rows[[length(rows)+1]] <- add_one(cols_abs[1], "abs")
      rows <- dplyr::bind_rows(rows)
      if (!nrow(rows)) next
      rows <- rows |>
        dplyr::filter(!is.na(group)) |>
        dplyr::group_by(service, metric, group) |>
        dplyr::summarise(mean_chg = mean(val, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(grouping = g)
      out[[length(out)+1]] <- rows
    }
  }
  dplyr::bind_rows(out)
}

#' Compute global reference means for each service/metric
#' @param df Data frame
#' @param services Character vector of service names
#' @param cut_q Quantile for trimming
#' @param handle_inf How to handle Inf values ("na" or "cap")
#' @param drop_zero Logical, drop zeros if TRUE
compute_global_refs <- function(df, services, cut_q = 0.999,
                                handle_inf = c("na","cap"), drop_zero = FALSE) {
  handle_inf <- match.arg(handle_inf)
  vmsg("Computing global refs: drop_zero = ", drop_zero)
  out <- list()
  for (svc in services) {
    map_rows <- dplyr::filter(svc_map, canonical == svc)
    if (!nrow(map_rows)) next
    for (metric in c("pct","abs")) {
      cols <- if (metric == "pct") map_rows$col_pct else map_rows$col_abs
      cols <- cols[cols %in% names(df)]
      if (!length(cols)) next
      v <- df[[cols[1]]]
      if (metric == "pct" && any(is.infinite(v))) {
        if (handle_inf == "na") v[is.infinite(v)] <- NA_real_
      }
      if (isTRUE(drop_zero)) v[v == 0] <- NA_real_
      cap <- stats::quantile(abs(v), cut_q, na.rm = TRUE)
      v_trim <- pmax(pmin(v, cap), -cap)
      out[[length(out)+1]] <- tibble::tibble(
        service = svc,
        metric  = metric,
        ref     = mean(v_trim, na.rm = TRUE)
      )
    }
  }
  dplyr::bind_rows(out)
}

#' Plot signed change bars (alt variant)
#' @param df Data frame
#' @param grouping Grouping column
#' @param metric Metric ("abs" or "pct")
#' @param refs Reference values
#' @param variant_label Label for output
#' @param variant_desc Description for plot
#' @param out_dir Output directory
plot_signed_alt <- function(df, grouping, metric,
                            refs,
                            variant_label = "keep0",
                            variant_desc  = "Zeros kept",
                            out_dir = "outputs/plots/signed_alt") {
  d <- dplyr::filter(df, grouping == !!grouping, metric == !!metric)
  if (!nrow(d)) return(invisible(NULL))
  d$service <- factor(d$service, levels = services)
  d$group   <- factor(d$group, levels = sort(unique(d$group)))
  refs_use <- dplyr::filter(refs, metric == !!metric)
  refs_use$service <- factor(refs_use$service, levels = services)
  p <- ggplot(d, aes(group, mean_chg, fill = group)) +
    geom_col(show.legend = FALSE) +
    geom_hline(yintercept = 0, color = "#7f7f7f", linewidth = 0.6) +
    geom_hline(data = refs_use, aes(yintercept = ref),
               linetype = "dashed", color = "#4a4a4a", linewidth = 0.5) +
    facet_wrap(~ service, scales = "free_y", ncol = 3) +
    labs(title = paste0("Signed mean change (alt) — ", grouping, " [", metric, "]"),
         subtitle = variant_desc,
         x = grouping,
         y = if (metric == "pct") "Mean % change (trimmed, signed)" else "Mean absolute change (trimmed, signed)") +
    theme_minimal(base_size = 12) +
    theme(strip.background = element_rect(fill = "#f3f4f6", color = NA),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  fp <- file.path(out_dir, paste0("bars_signed_alt_", variant_label, "_", tolower(grouping), "_", metric, ".png"))
  ggsave(fp, p, width = 12, height = 8, dpi = 300, bg = "white")
  message("Saved alt bars: ", fp)
}

#' Clean a string for safe filenames
#' @param x Input string
slug <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  sub("^_|_$", "", x)
}

#' Extract and export hotspot sets
#' @param df Data frame
#' @param value_col Value column ("abs_chg" or "pct_chg")
#' @param scope Scope label
#' @param group_col Grouping column
#' @param group_val Group value
#' @param sf_obj Geometry sf object
#' @param cfg Hotspot config
run_one_hotset <- function(df, value_col, scope,
                           group_col = NA_character_, group_val = NA_character_,
                           sf_obj, cfg) {
  # ...existing code from QMD...
}

#' Clean service change names (drop _mean/_sum for canonical services)
#' @param nm Character vector of names
rename_change <- function(nm) {
  stringr::str_replace(nm, "_(mean|sum)_(abs|pct)_chg$", "_\2_chg")
}

#' Find the latest file matching a pattern in a directory
#' @param pattern Regex pattern
latest_interim <- function(pattern) {
  files <- list.files(interim_dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) stop("No interim files match pattern: ", pattern)
  files[which.max(file.mtime(files))]
}

# ---- Legacy/Unclear Functions (for review) ----
# (Add any additional helpers here that are not currently used in the main workflow)

#' Example legacy function (not used in main workflow)
#' @param x Example argument
legacy_helper_example <- function(x) {
  # ...function body...
}
