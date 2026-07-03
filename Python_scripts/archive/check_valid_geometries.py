import geopandas as gpd

gdf = gpd.read_file("../data/reference/grid_UICN.shp")

# Check validity of geometries
invalid = ~gdf.is_valid

print(f"Invalid geometries: {invalid.sum()}")

# Optionally see which rows are invalid
print(gdf[invalid])

# Fix invalid geometries (commonly used trick)
gdf["geometry"] = gdf["geometry"].buffer(0)

# Double-check validity again
print(f"Still invalid after fix: {(~gdf.is_valid).sum()}")
