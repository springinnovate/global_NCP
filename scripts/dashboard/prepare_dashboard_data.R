# Slim copy of the 10 km grid for the hotspot explorer (scripts/dashboard/app.R), 2026-10-08.
#
# Keeps the evaluated cells (docs/cell_universe.md: no Antarctica, open ocean, Lakes, Rock & Ice), the
# grouping columns, relative (SPC) and absolute change for the five services, GHS-POP 2020, and each
# cell's bounding box in lon/lat for drawing. Run once, or again whenever 10k_change_calc.gpkg changes.
#
# Usage: Rscript scripts/dashboard/prepare_dashboard_data.R
# Writes: data/processed/dashboard/grid_dashboard.rds, data/processed/dashboard/change_map_layers.rds

suppressPackageStartupMessages({ library(sf); library(dplyr); library(here) })
setwd(here())

services <- c("n_export", "sed_export", "c_risk", "pollination", "nature_access")
cols <- c("grid_fid", "continent", "nev_name", "region_wb", "income_grp", "WWF_biome",
          paste0(services, "_pct_chg"), paste0(services, "_abs_chg"), "GHS_POP_E2020_GLOBE_sum")

g <- st_read("data/processed/10k_change_calc.gpkg",
             query = sprintf('SELECT %s, geom FROM "10k_change_calc"', paste(cols, collapse = ", ")),
             quiet = TRUE)
g <- g |> filter(is.na(continent) | !continent %in% c("Antarctica", "Seven seas (Open Ocean)"),
                 is.na(WWF_biome) | !WWF_biome %in% c("Lakes", "Rock & Ice"))
stopifnot(nrow(g) == 1372621)

# per-cell bounding box from the vertices (fast, vectorised)
xy <- st_coordinates(st_geometry(g))
fid <- xy[, ncol(xy)]             # last column = feature index
bb <- data.frame(
  xmin = tapply(xy[, "X"], fid, min), xmax = tapply(xy[, "X"], fid, max),
  ymin = tapply(xy[, "Y"], fid, min), ymax = tapply(xy[, "Y"], fid, max))

d <- st_drop_geometry(g) |> select(-continent) |> bind_cols(bb) |>
  rename(pop = GHS_POP_E2020_GLOBE_sum) |>
  mutate(income_grp = sub("^[0-9]\\. ", "", income_grp))

dir.create("data/processed/dashboard", recursive = TRUE, showWarnings = FALSE)
saveRDS(d, "data/processed/dashboard/grid_dashboard.rds", compress = FALSE)
message("wrote data/processed/dashboard/grid_dashboard.rds: ", nrow(d), " cells, ",
        format(object.size(d), units = "MB"))

# ---- change maps: Equal Earth pixels (EPSG:8857, 10 km, as the paper's change maps) -------------
# The grid is a regular 10 km lattice in the cylindrical equal-area projection (cell corners sit at a
# constant offset from multiples of 10 km), so it becomes a raster without any resampling. Projecting
# a raster of row numbers once gives, for every Equal Earth pixel, the grid cell it shows; the app
# then maps any service by lookup instead of projecting at each redraw.
suppressPackageStartupMessages(library(terra))
cea <- "+proj=cea +lat_ts=0 +datum=WGS84 +units=m"
ll <- sf_project("EPSG:4326", cea, cbind(d$xmin, d$ymin)) / 10000
off <- c(median(ll[, 1] %% 1), median(ll[, 2] %% 1))
ix <- as.integer(round(ll[, 1] - off[1])); iy <- as.integer(round(ll[, 2] - off[2]))
dup <- duplicated(cbind(ix, iy))
message("lattice: ", sum(dup), " duplicate positions (dropped from the change maps)")
r <- rast(ncols = diff(range(ix)) + 1, nrows = diff(range(iy)) + 1,
          xmin = (min(ix) + off[1]) * 1e4, xmax = (max(ix) + 1 + off[1]) * 1e4,
          ymin = (min(iy) + off[2]) * 1e4, ymax = (max(iy) + 1 + off[2]) * 1e4, crs = cea)
v <- rep(NA_integer_, ncell(r))
v[cellFromRowCol(r, max(iy) - iy[!dup] + 1, ix[!dup] - min(ix) + 1)] <- which(!dup)
values(r) <- v
# 10 km (the paper's maps; the whole-world view) and 5 km (other selections: at 10 km, nearest-cell
# resampling between the two equal-area grids skips ~16% of cells, visible when zoomed in)
lookup <- function(res) {
  p <- as.data.frame(project(r, "EPSG:8857", method = "near", res = res), xy = TRUE, na.rm = TRUE)
  names(p) <- c("x", "y", "row")
  p$row <- as.integer(p$row)
  message(res / 1000, " km: ", nrow(p), " pixels, ", length(unique(p$row)), " of ", nrow(d), " cells shown")
  p
}
pix <- list(world = lookup(10000), local = lookup(5000))

# country borders for the background, simplified to the 10 km scale
base <- st_read("data/vector_basedata/cartographic_ee_r264_correspondence.gpkg", quiet = TRUE) |>
  select(nev_name) |> st_make_valid() |>
  # split polygons that cross 180 degrees (e.g. Kiribati), which otherwise draw a band around the globe
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) |>
  st_transform("EPSG:8857") |> st_simplify(dTolerance = 2000)
base <- base[!st_is_empty(base), ]

saveRDS(list(pix = pix, base = base), "data/processed/dashboard/change_map_layers.rds")
message("wrote data/processed/dashboard/change_map_layers.rds")
