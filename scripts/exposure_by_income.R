# Population exposure by income group from the current beneficiary run (2026-07-29, LandScan 2023):
# local residents = LandScan in production hotspot cells; connected beneficiaries = the run's
# union_population raster (downstream 50 km or within 1 h travel) summed over evaluated grid cells.
# Both are assigned to the income group of the 10 km cell where the people live.
#
# Usage: Rscript scripts/exposure_by_income.R   ->  outputs/tables/exposure_by_income.csv

suppressMessages({library(sf); library(terra); library(exactextractr); library(dplyr); library(readr); library(here)})
setwd(here())

ls_r <- rast("C:/Users/JerónimoRodríguezEsc/data/global_ncp/Raw/Beneficiaries/Landscan/landscan-global-2023-assets/landscan-global-2023.tif")
union_r <- rast(Sys.glob("data/processed/beneficiaries/run_2026-07-29_5service/*hotspot_count_1plus_beneficiaries/full_raster_extent_union_population.tif"))

grid <- st_read("data/processed/10k_change_calc.gpkg",
                query = 'SELECT grid_fid, continent, WWF_biome, income_grp, geom FROM "10k_change_calc"', quiet = TRUE)
grid <- filter(grid, is.na(continent) | !continent %in% c("Antarctica", "Seven seas (Open Ocean)"),
               is.na(WWF_biome) | !WWF_biome %in% c("Lakes", "Rock & Ice"))
hot <- st_read("data/processed/hotspots/pct/global/hotspots_global_pct.gpkg",
               query = 'SELECT grid_fid FROM "hotspots_global_pct"', quiet = TRUE) |> st_drop_geometry()

g4326 <- st_transform(grid, crs(ls_r))
grid$connected <- exact_extract(union_r, st_transform(grid, crs(union_r)), "sum", progress = FALSE)
grid$pop <- exact_extract(ls_r, g4326, "sum", progress = FALSE)
grid$local <- ifelse(grid$grid_fid %in% hot$grid_fid, grid$pop, 0)

out <- st_drop_geometry(grid) |>
  mutate(income_grp = coalesce(income_grp, "no income group")) |>
  group_by(income_grp) |>
  summarise(local = sum(local, na.rm = TRUE), connected = sum(connected, na.rm = TRUE),
            population = sum(pop, na.rm = TRUE), .groups = "drop") |>
  mutate(share_of_connected = 100 * connected / sum(connected), share_of_local = 100 * local / sum(local))
write_csv(out, "outputs/tables/exposure_by_income.csv")
print(out |> mutate(across(c(local, connected, population), ~ round(.x / 1e6))), width = 120)
cat(sprintf("totals: local %.0f M, connected %.0f M (run's own global union: 7,383 M)\n",
            sum(out$local) / 1e6, sum(out$connected) / 1e6))
