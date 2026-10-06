# Replace the N_Ret_Ratio rows of outputs/plots/output_plots_diff/*_map_data.csv with the
# aligned recompute (2026-10-01).
#
# Inputs: latest {group}_*.csv in data/raw/n_ret_ratio_aligned/output/ (from
# Python_scripts/rerun_n_ret_ratio_path_a.py). Columns used: mean/stdev/valid_count of
# n_retention_ratio_diff_1992_2020 (the change raster) and mean of n_retention_ratio_{1992,2020}
# (base years, for the symmetric percent change). Formulas and Good/Bad rules follow
# zonal_stats_toolkit/compare_and_plot_changes.R (se = stdev / sqrt(n); SPC = 200 (m20 - m92) /
# (|m20| + |m92|); higher ratio = Good).
#
# Writes a backup <file>.backup_20261001_pre_nratio.csv before overwriting (only if absent).
# Prints old vs new values per group so the change can be reviewed.
# Run from the repo root: Rscript analysis/update_n_ret_ratio_map_data.R

suppressPackageStartupMessages({
  library(readr); library(dplyr)
})

out_dir  <- "data/raw/n_ret_ratio_aligned/output"
map_dir  <- "outputs/plots/output_plots_diff"
groups   <- c(biome = "WWF_biome", country = "nev_name", income_grp = "income_grp", region_wb = "region_wb")
map_file <- c(biome = "biome", country = "country", income_grp = "income_grp", region_wb = "region_wb")

status <- function(x) case_when(x > 0 ~ "Good", x < 0 ~ "Bad", TRUE ~ "Neutral")

for (grp in names(groups)) {
  key <- groups[[grp]]
  # A group's three rasters may come from separate runs (income_grp was run one raster at a time
  # to stay under the toolkit's memory limit): merge every output file for the group on its key,
  # taking each statistic column from the newest file that has it.
  f_all <- sort(list.files(out_dir, paste0("^", grp, "_.*\\.csv$"), full.names = TRUE), decreasing = TRUE)
  if (length(f_all) == 0) stop("no rerun output for ", grp)
  f_map <- file.path(map_dir, paste0(map_file[[grp]], "_map_data.csv"))
  merged <- NULL
  for (f in f_all) {
    d <- read_csv(f, show_col_types = FALSE)
    if (!is.null(merged)) d <- d[, c(key, setdiff(names(d), names(merged))), drop = FALSE]
    if (ncol(d) <= 1) next
    merged <- if (is.null(merged)) d else full_join(merged, d, by = key)
  }
  f_new <- paste(basename(f_all), collapse = " + ")

  new <- merged %>%
    transmute(
      grp_key       = .data[[key]],
      mean_new      = mean_n_retention_ratio_diff_1992_2020,
      stdev_new     = stdev_n_retention_ratio_diff_1992_2020,
      count_new     = valid_count_n_retention_ratio_diff_1992_2020,
      m92           = mean_n_retention_ratio_1992,
      m20           = mean_n_retention_ratio_2020,
      spc_new       = 200 * (m20 - m92) / (abs(m20) + abs(m92))
    ) %>%
    filter(!is.na(grp_key))
  if (anyDuplicated(new$grp_key)) stop("duplicate group keys in ", f_new)

  map <- read_csv(f_map, show_col_types = FALSE, col_types = cols(.default = col_guess()))
  map_key <- names(map)[1]
  is_nr <- map$service == "N_Ret_Ratio"
  idx <- match(map[[map_key]][is_nr], new$grp_key)
  if (anyNA(idx)) stop(grp, ": map_data groups without a rerun row: ",
                       paste(map[[map_key]][is_nr][is.na(idx)], collapse = "; "))

  backup <- sub("\\.csv$", ".backup_20261001_pre_nratio.csv", f_map)
  if (!file.exists(backup)) file.copy(f_map, backup)

  old <- map[is_nr, c(map_key, "mean_val", "sym_pct_change")]
  nw  <- new[idx, ]
  map$mean_val[is_nr]       <- nw$mean_new
  map$stdev_val[is_nr]      <- nw$stdev_new
  map$valid_count[is_nr]    <- nw$count_new
  map$se_val[is_nr]         <- nw$stdev_new / sqrt(nw$count_new)
  map$sym_pct_change[is_nr] <- nw$spc_new
  map$status_abs[is_nr]     <- status(nw$mean_new)
  map$status_pct[is_nr]     <- status(nw$spc_new)

  write_csv(map, f_map, na = "")

  cmp <- tibble(group = old[[map_key]],
                mean_old = old$mean_val, mean_new = nw$mean_new,
                spc_old = old$sym_pct_change, spc_new = nw$spc_new,
                ratio_1992 = nw$m92, ratio_2020 = nw$m20)
  cat("\n==", grp, ":", sum(is_nr), "N_Ret_Ratio rows updated from", basename(f_new), "\n")
  if (grp != "country") print(as.data.frame(cmp), digits = 4, row.names = FALSE)
  else cat("country: sign of mean change flipped in", sum(sign(cmp$mean_old) != sign(cmp$mean_new), na.rm = TRUE),
           "of", nrow(cmp), "countries; sign of SPC flipped in",
           sum(sign(cmp$spc_old) != sign(cmp$spc_new), na.rm = TRUE), "\n")
}
