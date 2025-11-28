# R/paths.R

# read once at load time
.GLOBAL_NCP_DATA <- Sys.getenv("GLOBAL_NCP_DATA", unset = "")
.path_state <- new.env(parent = emptyenv())
.path_state$project_root <- NULL

.find_project_root <- function() {
  root <- .path_state$project_root
  if (!is.null(root) && dir.exists(root)) {
    return(root)
  }
  cur <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  markers <- c("global_NCP.Rproj", ".git")
  while (nzchar(cur)) {
    marker_hit <- any(file.exists(file.path(cur, markers)))
    if (isTRUE(marker_hit)) {
      .path_state$project_root <- cur
      return(cur)
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  .path_state$project_root <- root
  root
}

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
  file.path(.find_project_root(), ...)
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
