import geopandas as gpd
import numpy as np

# Load the grid with geometry validation disabled
print("Loading grid...")
gdf = gpd.read_file(r"C:\projects\global_NCP\data\vector_basedata\AOOGrid_10x10km_land_4326_clean.gpkg", on_invalid="ignore")
print(f"Total features loaded: {len(gdf)}")

# Filter to valid geometries only
gdf = gdf[gdf.geometry.is_valid]
print(f"Valid geometries: {len(gdf)}")
print(f"CRS: {gdf.crs}\n")

# Check cell dimensions in 4326
print("--- In EPSG:4326 (original) ---")
sample = gdf.head(3)
for idx, row in sample.iterrows():
    minx, miny, maxx, maxy = row.geometry.bounds
    print(f"Cell {idx}: {maxx-minx:.6f}° x {maxy-miny:.6f}°")

# Reproject to 8857 and check actual cell dimensions
print("\n--- In EPSG:8857 (Equal Earth) ---")
gdf_8857 = gdf.to_crs("EPSG:8857")
sample_8857 = gdf_8857.head(3)
for idx, row in sample_8857.iterrows():
    minx, miny, maxx, maxy = row.geometry.bounds
    width = maxx - minx
    height = maxy - miny
    area_km2 = row.geometry.area / 1e6
    print(f"Cell {idx}: {width:.1f}m x {height:.1f}m | Area: {area_km2:.2f} km²")

# Get statistics
print("\n--- Cell Size Statistics (EPSG:8857) ---")
areas = gdf_8857.geometry.area
median_area = np.median(areas)
median_size = np.sqrt(median_area)
mean_size = np.sqrt(np.mean(areas))
print(f"Median cell area: {median_area/1e6:.2f} km²")
print(f"Median cell size: {median_size:.1f} meters")
print(f"Mean cell size: {mean_size:.1f} meters")
