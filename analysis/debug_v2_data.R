# analysis/debug_v2_data.R
library(sf)
library(dplyr)
library(here)
source(here("R", "paths.R"))

# Config
v2_gpkg <- file.path(data_dir(), "processed", "10k_change_calc_v2.gpkg")

# 1. Inspect V2 GPKG
if (!file.exists(v2_gpkg)) {
  stop("V2 GPKG does not exist: ", v2_gpkg)
}

message("Reading V2 GPKG...")
df <- st_read(v2_gpkg, quiet = TRUE) %>% st_drop_geometry()

message("Dimensions: ", paste(dim(df), collapse = " x "))
message("Columns (head): ", paste(head(names(df)), collapse = ", "))

# Check change columns
chg_cols <- grep("_(abs|pct)_chg$", names(df), value = TRUE)
message("Found ", length(chg_cols), " change columns.")

if (length(chg_cols) > 0) {
  # Check for NAs
  na_counts <- sapply(df[chg_cols], function(x) sum(is.na(x)))
  message("NA counts (top 5):")
  print(head(na_counts))

  # Check for all-NA columns
  all_na <- names(na_counts)[na_counts == nrow(df)]
  if (length(all_na) > 0) {
    message("\n[CRITICAL] The following columns are 100% NA:")
    print(all_na)
    message("Diagnosis: The join in process_zonal_stats_v2.qmd likely failed (FID mismatch).")
  } else {
    message("\nData looks populated. No columns are 100% NA.")
  }
} else {
  message("ERROR: No change columns found!")
}

# Check FIDs
message("\nFID Summary:")
if ("grid_fid" %in% names(df)) print(summary(df$grid_fid))
if ("fid" %in% names(df)) print(summary(df$fid))