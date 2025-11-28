# scripts/save_checkpoint.R  (run in RStudio)
dir.create(file.path(data_dir(), "processed", "intermediate"), showWarnings = FALSE, recursive = TRUE)

# pick only what you need downstream
qs::qsave(plt_long,  file.path(data_dir(), "processed", "intermediate", "plt_long_v041.qs"))
qs::qsave(grid_sf,   file.path(data_dir(), "processed", "intermediate", "grid_sf_v041.qs"))
readr::write_csv(hot_index, file.path(data_dir(), "processed", "hotspots", "_hotspots_index.csv"))

# config as RDS so it’s identical when you restore
saveRDS(HOTS_CFG, file.path(data_dir(), "processed", "intermediate", "HOTS_CFG_v041.rds"))
