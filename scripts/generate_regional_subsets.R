#!/usr/bin/env Rscript
#
# generate_regional_subsets.R
#
# Splits hotspot_area_stats.csv and hotspot_multiservice_stats.csv into
# per-group CSV files under data/processed/tables/regional_subsets/.
#
# Run from the project root:
#   Rscript scripts/generate_regional_subsets.R
#
# No heavy data needed — only reads the pre-computed summary CSVs.
# Re-run whenever hotspot_area_stats.csv changes (new InVEST run, new
# services, or a different hotspot threshold).

library(dplyr)
library(readr)
library(here)
library(devtools)

devtools::load_all(quiet = TRUE)
invisible(lapply(
  list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE),
  source
))

safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

area_stats_path <- file.path(data_dir(), "processed", "tables", "hotspot_area_stats.csv")
multi_stats_path <- file.path(data_dir(), "processed", "tables", "hotspot_multiservice_stats.csv")

if (!file.exists(area_stats_path)) {
  stop("hotspot_area_stats.csv not found at: ", area_stats_path,
       "\nRe-run hotspot_synthesis.qmd first.")
}

stats_df <- read_csv(area_stats_path, show_col_types = FALSE)
multi_df <- if (file.exists(multi_stats_path)) {
  read_csv(multi_stats_path, show_col_types = FALSE)
} else {
  message("hotspot_multiservice_stats.csv not found — skipping multi-service subsets.")
  NULL
}

subsets_dir <- file.path(data_dir(), "processed", "tables", "regional_subsets")

for (gv in unique(stats_df$grouping_var)) {
  gv_dir <- file.path(subsets_dir, gv)
  dir.create(gv_dir, recursive = TRUE, showWarnings = FALSE)

  gv_df <- filter(stats_df, grouping_var == gv)

  # Combined: all groups for this variable in one file
  write_csv(gv_df, file.path(gv_dir, paste0("hotspot_area_stats_", gv, ".csv")))

  # Individual group files
  for (grp in unique(gv_df$group)) {
    write_csv(
      filter(gv_df, group == grp),
      file.path(gv_dir, paste0("hotspot_area_stats_", safe_name(grp), ".csv"))
    )
  }

  # Multi-service stats
  if (!is.null(multi_df) && "grouping" %in% names(multi_df)) {
    gv_multi <- filter(multi_df, grouping == gv)
    if (nrow(gv_multi) > 0) {
      write_csv(gv_multi, file.path(gv_dir, paste0("hotspot_multi_stats_", gv, ".csv")))
    }
  }

  message(sprintf("  [%s] %d groups written", gv, length(unique(gv_df$group))))
}

n_files <- length(list.files(subsets_dir, recursive = TRUE, pattern = "\\.csv$"))
message(sprintf("\nDone. %d CSV files in %s", n_files, subsets_dir))
