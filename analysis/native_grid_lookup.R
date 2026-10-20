# Lookup rasters linking the native-SPC display grids to the 10 km analysis grid.
#
# Rasterizes the 10 km grid's grid_fid (cell centre rule) onto
#   - the 0.05 deg grid of the native-SPC aggregates (analysis/native_spc.py, factor 18), global extent
#     (rows offset for the rasters that start at 60 N are handled by the reader), and
#   - the ~6 km Eckert IV grid of the nature access aggregate (factor 3),
# so every display block can be given the cell's evaluated-universe flag, country, biome, region and
# cropland expansion when the paper's map statistics are computed from native pixels.
#
# Output: data/processed/native_spc/grid_fid_005deg.tif, grid_fid_natacc_6km.tif
# Run from the repo root (PowerShell): Rscript analysis/native_grid_lookup.R

suppressPackageStartupMessages({library(terra); library(here)})

out_dir <- here("data", "processed", "native_spc")
g <- vect(here("data", "processed", "10k_change_calc.gpkg"),
          query = 'SELECT grid_fid, geom FROM "10k_change_calc"')
g$grid_fid <- as.numeric(g$grid_fid)   # int64 from the gpkg rasterizes to all-NA

# 0.05 deg, same origin as the pollination raster (-180, 90.000718), 7200 x 3600
res <- 18 / 360   # 18 native pixels of 1/360 deg
t1 <- rast(nrows = 3600, ncols = 7200, xmin = -180, xmax = -180 + 7200 * res,
           ymin = 90.000718 - 3600 * res, ymax = 90.000718, crs = "EPSG:4326")
r1 <- rasterize(g, t1, field = "grid_fid")
writeRaster(r1, file.path(out_dir, "grid_fid_005deg.tif"), datatype = "INT4S", overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE"))
message("0.05 deg blocks with a grid cell: ", global(!is.na(r1), "sum")[1, 1])

# nature access aggregate grid (Eckert IV, 6 km)
na <- rast(file.path(out_dir, "nature_access_agg_f3.tif"))[[1]]
t2 <- rast(na)
r2 <- rasterize(project(g, crs(t2)), t2, field = "grid_fid")
writeRaster(r2, file.path(out_dir, "grid_fid_natacc_6km.tif"), datatype = "INT4S", overwrite = TRUE,
            gdal = c("COMPRESS=DEFLATE"))
message("6 km blocks with a grid cell: ", global(!is.na(r2), "sum")[1, 1])
