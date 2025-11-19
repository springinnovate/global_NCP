# R/paths.R

# read once at load time
.GLOBAL_NCP_DATA <- Sys.getenv("GLOBAL_NCP_DATA", unset = "")

# sanity check helper (optional)
.assert_data_root <- function() {
  if (.GLOBAL_NCP_DATA == "" || !dir.exists(.GLOBAL_NCP_DATA)) {
    stop(
      "GLOBAL_NCP_DATA is not set or directory not found.\n",
      "Set it in ~/.Renviron, e.g.\n",
      "GLOBAL_NCP_DATA=/home/jeronimo/data/global_ncp"
    )
  }
}

# project-relative path (inside the repo)
project_dir <- function(...) {
  # assumes you run from the project root or an Rproj
  # if you like, swap to `here::here(...)`
  file.path(getwd(), ...)
}

# data root path (outside the repo)
data_dir <- function(...) {
  .assert_data_root()
  file.path(.GLOBAL_NCP_DATA, ...)
}

# convenience shortcuts you’ll use in code -------------
# adjust subfolder names if yours differ
data_raw      <- function(...) data_dir("raw", ...)
data_interim  <- function(...) data_dir("interim", ...)
data_processed<- function(...) data_dir("processed", ...)
data_vectors  <- function(...) data_dir("vector_basedata", ...)
data_hydros   <- function(...) data_dir("hydrosheds", ...)

# repo outputs you *do* keep in git
out_plots     <- function(...) project_dir("outputs", "plots", ...)
out_tables    <- function(...) project_dir("outputs", "tables", ...)
out_logs      <- function(...) project_dir("outputs", "logs", ...)

# make sure an output folder exists before saving
ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}
