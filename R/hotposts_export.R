#' Write hotspot layers (global & by subregion) and return an index
#'
#' Computes hotspots once per metric and scope (global and/or per group value),
#' writes compact GPKG layers (hotspot features only), and returns a tibble index.
#'
#' @param plt_long Long table with columns: fid, service, abs_chg, pct_chg, and group cols.
#' @param sf_obj   sf grid with a unique `fid` column (defaults to reading processed gpkg).
#' @param cfg      List config (see HOTS_CFG in your QMD): loss, gain, combos, pct_cutoff, etc.
#' @param groupings Character vector of grouping column names (e.g., c("income_grp","region_wb",...)).
#' @param include_global Logical; also run the global (no-group) case, default TRUE.
#' @param metrics  Character vector among c("abs","pct") to run.
#' @param write_layers Logical; write GPKG layers (overrides cfg$write_layers if given).
#' @param write_index  Logical; write CSV index (overrides cfg$write_index if given).
#' @param out_dir Optional output dir (defaults to cfg$out_dir).
#' @param verbose  Logical; print progress.
#'
#' @return Tibble with columns: scope, group_col, group_val, metric, n_hot, gpkg
#' @export
make_hotspots <- function(plt_long,
                          sf_obj = NULL,
                          cfg,
                          groupings = NULL,            # <- was c(...); now NULL = use cfg
                          include_global = TRUE,
                          metrics = c("abs","pct"),
                          write_layers = NULL,
                          write_index  = NULL,
                          out_dir = NULL,
                          verbose = TRUE) {
  
  # Pull groupings from cfg unless explicitly provided
  groupings <- groupings %||% cfg$groupings %||% character(0)
  
  # (optional) warn if any requested grouping columns are missing
  missing_groups <- setdiff(groupings, names(plt_long))
  if (length(missing_groups)) {
    warning("Grouping column(s) not found in plt_long and will be skipped: ",
            paste(missing_groups, collapse = ", "))
    groupings <- intersect(groupings, names(plt_long))
  }
  
  
  stopifnot(all(c("fid","service","abs_chg","pct_chg") %in% names(plt_long)))
  
  # IO options (honor explicit args first, then cfg, then sensible defaults)
  write_layers <- if (!is.null(write_layers)) write_layers else isTRUE(cfg$write_layers)
  write_index  <- if (!is.null(write_index))  write_index  else isTRUE(cfg$write_index)
  out_dir      <- if (!is.null(out_dir)) out_dir else cfg$out_dir %||% file.path(data_dir(), "processed","hotspots")
  
  # Geometry: prefer provided object; fall back to processed gpkg
  if (is.null(sf_obj)) {
    gpkg_grid <- file.path(data_dir(), "processed", "10k_change_calc.gpkg")
    stopifnot(file.exists(gpkg_grid))
    sf_obj <- sf::st_read(gpkg_grid, quiet = TRUE)
  }
  if (!"fid" %in% names(sf_obj)) sf_obj$fid <- seq_len(nrow(sf_obj))
  stopifnot(!any(duplicated(sf_obj$fid)))
  
  # Helper: safe slug for filenames
  slug <- function(x) {
    x <- gsub("[^A-Za-z0-9]+", "_", as.character(x))
    x <- gsub("_+", "_", x)
    gsub("^_|_$", "", x)
  }
  
  # Inner runner for one scope
  run_one <- function(df, metric, scope, group_col = NA_character_, group_val = NA_character_) {
    # Optional filter by group value
    if (!is.na(group_col) && !is.na(group_val)) {
      df <- df[df[[group_col]] %in% group_val, , drop = FALSE]
    }
    if (nrow(df) == 0L) {
      return(tibble::tibble(
        scope = as.character(scope),
        group_col = ifelse(is.na(group_col), NA_character_, as.character(group_col)),
        group_val = ifelse(is.na(group_val), NA_character_, as.character(group_val)),
        metric = metric,
        n_hot = 0L, gpkg = NA_character_
      ))
    }
    
    # Safety: ensure `fid` aligns with geometry
    stopifnot("fid" %in% names(df), "fid" %in% names(sf_obj))
    if (!all(df$fid %in% sf_obj$fid)) {
      missing <- setdiff(unique(df$fid), sf_obj$fid)
      stop(sprintf("Geometry missing %d fid(s), e.g. %s",
                   length(missing), paste(head(missing, 5), collapse = ", ")))
    }
    
    # Metric select
    value_col <- if (metric == "abs") "abs_chg" else "pct_chg"
    
    # Extract with central rules
    hs <- extract_hotspots(
      df             = df,
      value_col      = value_col,
      pct_cutoff     = cfg$pct_cutoff,
      threshold_mode = cfg$threshold_mode,
      rule_mode      = cfg$rule_mode,
      loss_services  = cfg$loss,
      gain_services  = cfg$gain,
      combos         = cfg$combos,
      id_cols        = c("c_fid"),
      sf_obj         = sf_obj,
      write_sf_path  = NULL,
      clean_names    = TRUE
    )
    
    # Output path
    metric_stub <- metric
    folder <- if (is.na(group_col)) file.path(out_dir, metric_stub, "global")
    else                   file.path(out_dir, metric_stub, tolower(group_col))
    if (write_layers) dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    
    file_stub <- if (is.na(group_col)) {
      sprintf("hotspots_global_%s", metric_stub)
    } else {
      sprintf("hotspots_%s_%s_%s", tolower(group_col), slug(group_val), metric_stub)
    }
    out_gpkg <- file.path(folder, paste0(file_stub, ".gpkg"))
    
    n_hot <- 0L
    if (!is.null(hs$hotspots_sf) && nrow(hs$hotspots_sf) > 0) {
      if (write_layers) sf::st_write(hs$hotspots_sf, out_gpkg, quiet = TRUE, delete_dsn = TRUE)
      n_hot <- nrow(hs$hotspots_sf)
    } else {
      out_gpkg <- NA_character_
    }
    
    tibble::tibble(
      scope     = as.character(scope),
      group_col = ifelse(is.na(group_col), NA_character_, as.character(group_col)),
      group_val = ifelse(is.na(group_val), NA_character_, as.character(group_val)),
      metric    = metric_stub,
      n_hot     = n_hot,
      gpkg      = out_gpkg
    )
  }
  
  # Build the run list
  idx_list <- list()
  
  # Global runs
  if (isTRUE(include_global)) {
    for (m in metrics) {
      if (isTRUE(verbose)) message("Global / ", m)
      idx_list[[length(idx_list) + 1L]] <- run_one(plt_long, m, scope = "global")
    }
  }
  
  # Grouped runs
  for (gc in groupings) {
    if (!gc %in% names(plt_long)) next
    vals <- sort(unique(plt_long[[gc]]))
    vals <- vals[!is.na(vals)]
    
    # Coerce to character for index stability
    vals_chr <- as.character(vals)
    
    for (m in metrics) {
      if (isTRUE(verbose)) message("By ", gc, " / ", m, " (", length(vals_chr), " groups)")
      for (v in vals_chr) {
        idx_list[[length(idx_list) + 1L]] <- run_one(plt_long, m, "by_group", gc, v)
      }
    }
  }
  
  idx <- dplyr::bind_rows(idx_list)
  
  if (isTRUE(write_index)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(idx, file.path(out_dir, "_hotspots_index.csv"))
  }
  
  idx
}

# tiny infix for defaults
`%||%` <- function(x, y) if (is.null(x)) y else x
