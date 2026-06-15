source("R/paths.R")
library(dplyr, warn.conflicts = FALSE)
library(sf)

gpkg_grid <- file.path(data_dir(), "processed", "10k_change_calc.gpkg")
grid_df <- sf::st_read(gpkg_grid, quiet = TRUE) %>% sf::st_drop_geometry()

if("Id" %in% names(grid_df)) {
  grid_df <- grid_df %>% rename(fid = Id)
}
if("orig_fid" %in% names(grid_df)) {
  grid_df <- grid_df %>% rename(fid = orig_fid)
}
if("grid_fid" %in% names(grid_df)) {
  grid_df <- grid_df %>% rename(fid = grid_fid)
}

hotspots_gpkg <- "C:/projects/global_NCP/data/processed/hotspots/pct/global/hotspots_global_pct.gpkg"
hs_df <- sf::st_read(hotspots_gpkg, quiet = TRUE) %>% sf::st_drop_geometry()

pop_exposure <- hs_df %>%
  select(grid_fid, region_wb) %>%
  distinct() %>%
  left_join(grid_df %>% select(fid, GHS_POP_E2020_GLOBE_sum), by = c("grid_fid" = "fid")) %>%
  group_by(region_wb) %>%
  summarise(
    exposed_population = sum(GHS_POP_E2020_GLOBE_sum, na.rm = TRUE)
  )

print(pop_exposure %>% mutate(pop_millions = exposed_population / 1e6))
