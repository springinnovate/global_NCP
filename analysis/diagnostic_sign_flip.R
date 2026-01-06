library(dplyr)
library(purrr)
library(sf)

# Source path configuration to handle external data directories
source("R/paths.R")

# 1. Load the FULL QA dataset (which contains raw years 1995/2020 and the calculated change)
#    Do NOT use '10k_change_calc.gpkg' because it lacks the raw 1995/2020 columns needed for verification.
input_path <- data_processed("10k_grid_ES_change_benef.gpkg")

if (!file.exists(input_path)) {
  stop("Full QA dataset not found at: ", input_path, "\nPlease ensure GLOBAL_NCP_DATA is set correctly in .Renviron.")
}

message("Loading sample from full QA dataset: ", input_path)
# Read a sample to save memory, or remove LIMIT to check everything
check_df <- st_read(input_path, query = "SELECT * FROM '10k_grid_ES_change_benef' LIMIT 5000", quiet = TRUE)

# Ensure fid exists
if (!"fid" %in% names(check_df)) {
  check_df$fid <- seq_len(nrow(check_df))
}

# 2. Define the services to check (based on your canonical list)
services_to_check <- c(
  "usle",
  "nature_access",
  "n_ret_ratio",
  "sed_ret_ratio",
  "coastal_protection_Rt",
  "n_export",
  "n_retention",
  "sed_export",
  "pollination"
)

# Helper function to find column names for a service
check_service_math <- function(df, service_base) {
  cols <- names(df)

  # Regex to find 199x and 2020 columns for this service
  # We look for columns starting with service_base and containing the year
  col_1995 <- grep(paste0("^", service_base, ".*199[0-9].*"), cols, value = TRUE)[1]
  col_2020 <- grep(paste0("^", service_base, ".*2020.*"), cols, value = TRUE)[1]
  col_abs  <- grep(paste0("^", service_base, ".*abs_chg"), cols, value = TRUE)[1]
  col_pct  <- grep(paste0("^", service_base, ".*pct_chg"), cols, value = TRUE)[1]

  if (any(is.na(c(col_1995, col_2020, col_abs, col_pct)))) {
    message(paste("Skipping", service_base, "- could not find all matching columns."))
    return(NULL)
  }

  message(paste("Checking:", service_base, "| Start:", col_1995, "| End:", col_2020, "| Abs:", col_abs, "| Pct:", col_pct))

  # Calculate expected change
  df_calc <- df %>%
    st_drop_geometry() %>%
    select(fid, val_start = all_of(col_1995), val_end = all_of(col_2020), val_abs_chg = all_of(col_abs), val_pct_chg = all_of(col_pct)) %>%
    mutate(
      calc_diff = val_end - val_start,
      diff_check = val_abs_chg - calc_diff,

      # 1. Math Check: Does abs_chg match (End - Start)?
      is_wrong = abs(diff_check) > 1e-5,

      # 2. Sign Flip Check: Does sign(abs) match sign(pct)? (Ignore zeros/NAs)
      #    We flag if they have OPPOSITE signs (product is -1)
      sign_abs = sign(val_abs_chg),
      sign_pct = sign(val_pct_chg),
      is_flipped = (sign_abs * sign_pct == -1),

      # 3. Negative Input Check: Are inputs negative?
      is_neg_input = (val_start < 0 | val_end < 0)
    ) %>%
    filter(is_wrong | is_flipped | is_neg_input)

  if (nrow(df_calc) > 0) {
    message(paste("!!! FOUND ISSUES IN", service_base, ":", nrow(df_calc), "rows."))
    # Add a column to describe the issue
    df_calc <- df_calc %>%
      mutate(
        service = service_base,
        issue_type = case_when(
          is_wrong ~ "Math Error",
          is_flipped ~ "Sign Flip",
          is_neg_input ~ "Negative Input",
          TRUE ~ "Other"
        )
      )
    return(df_calc)
  } else {
    message(paste("OK:", service_base))
    return(NULL)
  }
}

# 3. Run the check
results <- map_dfr(services_to_check, ~check_service_math(check_df, .x))

if (nrow(results) == 0) {
  message("\nSUCCESS: No sign flips or calculation errors found in the sample.")
} else {
  message("\nFAILURE: Issues found. See 'results' dataframe.")
  print(head(results))
}