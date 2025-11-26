# scripts/restore_checkpoint.R  (run in VS Code R terminal)
HOTS_CFG <- readRDS(file.path(data_dir(), "processed", "intermediate", "HOTS_CFG_v041.rds"))
plt_long <- qs::qread(file.path(data_dir(), "processed", "intermediate", "plt_long_v041.qs"))
grid_sf  <- qs::qread(file.path(data_dir(), "processed", "intermediate", "grid_sf_v041.qs"))

# quick sanity
stopifnot(nrow(plt_long) > 0, inherits(grid_sf, "sf"))
dplyr::glimpse(plt_long, width = 80)
