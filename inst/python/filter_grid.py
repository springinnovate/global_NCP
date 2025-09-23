import geopandas as gpd

# Load your grid (created with st_make_grid or otherwise)
grid = gpd.read_file("../data/reference/grid_wgs84_approx10km.shp")  # or .shp, but .gpkg is better

# Load your HydroSHEDS vector
hydro = gpd.read_file("../data/reference/hydrosheds_lv6_synth.gpkg")  # use whatever your filename is

# Make sure CRS matches
if grid.crs != hydro.crs:
    hydro = hydro.to_crs(grid.crs)

# Spatial join to keep only grid cells that intersect hydro basins
grid_on_land = gpd.sjoin(grid, hydro, how="inner", predicate="intersects")

# Drop hydro attributes and duplicate index
grid_on_land = grid_on_land.loc[:, grid.columns]

# Save filtered grid
grid_on_land.to_file("../data/reference/grid_land_only.shp", driver="ESRI Shapefile")
