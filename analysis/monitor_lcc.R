# Script to monitor LCC extraction progress
# Run this in a separate R session/terminal while LC_change.qmd is running.

library(here)
library(dplyr)

# Load project paths
# Ensure we are in the project root
if (!file.exists(here::here("R", "paths.R"))) {
  stop("Please run this script from the project root directory.")
}
source(here::here("R", "paths.R"))

# --- Configuration ---
chunk_dir <- file.path(data_dir(), "processed", "lcc_chunks")
# Total chunks is approx 2.5M cells / 50k chunk size = 51 chunks.
# Adjust this if you changed the chunk size or dataset.
total_chunks_est <- 51

message("==================================================")
message(" LCC Extraction Monitor")
message(" Watching: ", chunk_dir)
message(" Target:   ", total_chunks_est, " chunks (approx)")
message("==================================================")

while (TRUE) {
  # 1. Count completed chunks
  files <- list.files(chunk_dir, pattern = "lcc_chunk_.*\\.rds$", full.names = TRUE)
  n_done <- length(files)

  # 2. Calculate Progress
  pct <- min(100, round((n_done / total_chunks_est) * 100, 1))

  # 3. Estimate Time Remaining
  time_msg <- "| Calculating speed..."

  if (n_done > 1) {
    # Get file info to calculate speed
    info <- file.info(files)
    info <- info[order(info$mtime), ]

    # Use the last few chunks to estimate current speed (e.g., last 5)
    # This adapts if the server load changes
    n_window <- min(n_done, 5)
    recent_info <- tail(info, n_window)

    if (nrow(recent_info) > 1) {
      t_first <- recent_info$mtime[1]
      t_last <- recent_info$mtime[nrow(recent_info)]

      # Duration in minutes for these chunks
      duration_mins <- as.numeric(difftime(t_last, t_first, units = "mins"))

      # Avg mins per chunk
      avg_speed <- duration_mins / (nrow(recent_info) - 1)

      # Remaining
      chunks_left <- max(0, total_chunks_est - n_done)
      mins_left <- chunks_left * avg_speed

      # ETA
      eta <- Sys.time() + as.difftime(mins_left, units = "mins")

      time_msg <- sprintf("| Speed: %.1f min/chunk | ETA: %s (%dh %dm left)",
                          avg_speed, format(eta, "%H:%M"),
                          floor(mins_left/60), round(mins_left %% 60))
    }
  }

  # 4. Print Status Line (overwrite previous line)
  # \r returns to start of line
  cat(sprintf("\r[%s] Chunk %d/%d (%s%%) %s          ",
              format(Sys.time(), "%H:%M:%S"),
              n_done, total_chunks_est, pct, time_msg))

  # 5. Check for completion
  if (n_done >= total_chunks_est) {
    cat("\n\nJob appears complete! (Or target reached)\n")
    break
  }

  # 6. Wait before next check
  Sys.sleep(60)
}