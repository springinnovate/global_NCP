# analysis/patch_v2_with_ratios.R
# Purpose: Inject missing ratio columns from V1 GPKG into V2 GPKG to avoid re-running zonal stats.

library(sf)
library(dplyr)
library(here)

# Load paths
source(here::here("R", "paths.R"))

# Define paths
v1_gpkg <- file.path(data_dir(), "processed", "10k_change_calc.gpkg")
v2_gpkg <- file.path(data_dir(), "processed", "10k_change_calc_v2.gpkg")

# Check existence
if (!file.exists(v1_gpkg)) stop("V1 GPKG not found: ", v1_gpkg)
if (!file.exists(v2_gpkg)) stop("V2 GPKG not found: ", v2_gpkg)

message("Reading V1 (Original)...")
v1 <- st_read(v1_gpkg, quiet = TRUE)
v1_df <- st_drop_geometry(v1)

# Normalize ID columns in V1
if ("grid_fid" %in% names(v1_df) && !"fid" %in% names(v1_df)) {
  v1_df <- dplyr::rename(v1_df, fid = grid_fid)
}
if (!"fid" %in% names(v1_df)) {
  message("WARNING: 'fid' not found in V1. Assuming row order and creating sequential IDs.")
  v1_df$fid <- seq_len(nrow(v1_df))
}

message("Reading V2 (Symmetric)...")
v2 <- st_read(v2_gpkg, quiet = TRUE)

# Normalize ID columns in V2
if ("grid_fid" %in% names(v2) && !"fid" %in% names(v2)) {
  v2 <- dplyr::rename(v2, fid = grid_fid)
}
if (!"fid" %in% names(v2)) {
  message("WARNING: 'fid' not found in V2. Assuming row order and creating sequential IDs.")
  v2$fid <- seq_len(nrow(v2))
}

# Identify ratio columns in V1 (looking for *ret_ratio* change columns)
ratio_cols <- grep("ret_ratio.*_chg$", names(v1_df), value = TRUE, ignore.case = TRUE)

message("Found ratio columns in V1: ")
print(ratio_cols)

if (length(ratio_cols) == 0) stop("Could not find ratio change columns in V1.")

# Prepare subset to merge
v1_ratios <- v1_df %>% select(fid, all_of(ratio_cols))

# Remove if already present in V2 (to avoid duplicates)
v2_clean <- v2 %>% select(-any_of(ratio_cols))

# Merge
message("Merging ratios into V2...")
v2_hybrid <- left_join(v2_clean, v1_ratios, by = "fid")

# Verification and Write
added_cols <- ratio_cols[ratio_cols %in% names(v2_hybrid)]
if (length(added_cols) == length(ratio_cols)) {
    message("Verification successful. All ratio columns have been merged.")

    # Rename fid to grid_fid to avoid GPKG reserved keyword conflict
    if ("fid" %in% names(v2_hybrid)) {
      v2_hybrid <- dplyr::rename(v2_hybrid, grid_fid = fid)
    }

    # Write back
    message("Overwriting V2 GPKG with hybrid data...")
    st_write(v2_hybrid, v2_gpkg, delete_dsn = TRUE, quiet = TRUE)
    message("Success. The file has been updated at: ", v2_gpkg)
} else {
    missing_cols <- setdiff(ratio_cols, names(v2_hybrid))
    stop("CRITICAL: Merge failed. The following ratio columns could not be added: ", paste(missing_cols, collapse=", "), ". The GPKG was NOT updated.")
}