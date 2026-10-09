# Verify the cell universe every paper number rests on (see docs/cell_universe.md).
# Rebuilds the evaluated universe from 10k_change_calc.gpkg with the same exclusions as
# hotspot_extraction.qmd, re-derives each service's 5% hotspot set, and checks it against the
# production hotspot file. Stops on any mismatch in the counts.
#
# Usage: Rscript analysis/verify_cell_universe.R

suppressMessages({library(sf); library(dplyr); library(here)})
setwd(here())

svc <- c(N_export = "n_export_pct_chg", Sed_export = "sed_export_pct_chg", C_Risk = "c_risk_pct_chg",
         Pollination = "pollination_pct_chg", Nature_Access = "nature_access_pct_chg")
loss <- c("Pollination", "Nature_Access")   # hotspot = steepest decline; others steepest increase

grid <- st_read("data/processed/10k_change_calc.gpkg",
                query = sprintf('SELECT grid_fid, continent, WWF_biome, region_wb, income_grp, %s FROM "10k_change_calc"',
                                paste(svc, collapse = ", ")),
                quiet = TRUE) |> st_drop_geometry()
universe <- grid |>
  filter(is.na(continent) | !continent %in% c("Antarctica", "Seven seas (Open Ocean)"),
         is.na(WWF_biome) | !WWF_biome %in% c("Lakes", "Rock & Ice"))
hot <- st_read("data/processed/hotspots/pct/global/hotspots_global_pct.gpkg",
               query = sprintf('SELECT grid_fid, %s FROM "hotspots_global_pct"', paste(names(svc), collapse = ", ")),
               quiet = TRUE) |> st_drop_geometry()

check <- function(label, got, expected) {
  ok <- got == expected
  cat(sprintf("%-48s %10s  expected %10s  %s\n", label, format(got, big.mark = ","),
              format(expected, big.mark = ","), if (ok) "ok" else "MISMATCH"))
  ok
}

ok <- c(
  check("grid cells", nrow(grid), 1522073),
  check("evaluated cells", nrow(universe), 1372621),
  check("hotspot cells (any service)", nrow(hot), 189932),
  check("hotspot cells outside the evaluated universe", sum(!hot$grid_fid %in% universe$grid_fid), 0)
)
expected_valid <- c(N_export = 1372621, Sed_export = 1372621, C_Risk = 79473, Pollination = 1372621,
                    Nature_Access = 1354611)
expected_hot <- c(N_export = 68632, Sed_export = 68632, C_Risk = 3974, Pollination = 68632,
                  Nature_Access = 67731)
for (s in names(svc)) {
  v <- universe[[svc[[s]]]]
  ok <- c(ok, check(paste(s, "cells with a value"), sum(!is.na(v)), expected_valid[[s]]),
          check(paste(s, "hotspot cells"), sum(hot[[s]] %in% 1), expected_hot[[s]]))
  # set check: the production set should be the 5% tail of this universe (ties at the cutoff can
  # differ by a handful of cells, so report the difference instead of failing on it)
  cut <- quantile(v, if (s %in% loss) 0.05 else 0.95, na.rm = TRUE)
  tail <- universe$grid_fid[!is.na(v) & (if (s %in% loss) v <= cut else v >= cut)]
  cat(sprintf("%-48s %10d cells differ (ties at the cutoff)\n", paste(" ", s, "set vs re-derived 5% tail"),
              length(setdiff(union(tail, hot$grid_fid[hot[[s]] %in% 1]), intersect(tail, hot$grid_fid[hot[[s]] %in% 1])))))
}
cat(sprintf("\nShare of evaluated cells that are a hotspot: %.1f%%\n", 100 * nrow(hot) / nrow(universe)))
if (!all(ok)) stop("cell universe check failed")
cat("all checks passed\n")
