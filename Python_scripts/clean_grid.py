import geopandas as gpd
import os

print("Loading grid...")
gdf = gpd.read_file(r"C:\projects\global_NCP\data\vector_basedata\AOOGrid_10x10km_land_4326_clean.gpkg", on_invalid="ignore")
print(f"Total features: {len(gdf)}")

# Remove invalid geometries
gdf = gdf[gdf.geometry.is_valid]
print(f"Valid geometries: {len(gdf)}")

# Save cleaned version
output_path = r"C:\projects\global_NCP\data\vector_basedata\AOOGrid_10x10km_land_4326_valid.gpkg"
gdf.to_file(output_path, driver="GPKG", layer="AOOGrid_10x10km_land_4326_valid")
print(f"Saved cleaned grid to: {output_path}")
