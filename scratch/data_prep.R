library(sf)
library(terra)
uicn_grid <- st_read('/home/jeronimo/OneDrive/global_NCP/data/reference/grid_UICN.shp')
uicn_grid <- st_make_valid(uicn_grid)


grid2 <- st_make_grid()
uicn_grid <- st_transform(uicn_grid, crs=crs(t))

st_write(uicn_grid, '/home/jeronimo/OneDrive/global_NCP/data/reference/grid_UICN_WGS84.shp', append=FALSE)
uicn_grid <- st_read('/home/jeronimo/OneDrive/global_NCP/data/reference/grid_UICN_WGS84.shp')

t <- rast('/home/jeronimo/OneDrive/global_NCP/data/Spring/Inspring/global_n_export_tnc_esa1992_compressed_md5_728edc.tif')


invalid <- !st_is_valid(uicn_grid)
sum(invalid)


library(sf)

# Approximate 10km in degrees
cell_size_deg <- 10 / 111.32  # ≈ 0.08983

# Define extent: full global bounds
world_bounds <- st_as_sfc(st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = 90), crs = 4326))

# Create the grid
global_grid <- st_make_grid(world_bounds, cellsize = cell_size_deg, square = TRUE) |> st_sf()

# Add unique ID column
global_grid$grid_id <- seq_len(nrow(global_grid))

# Save to shapefile or GeoPackage
st_write(global_grid, "data/reference/grid_wgs84_approx10km.shp")
