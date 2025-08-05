# this code is not working, keep it here if some day i need it again!
import geopandas as gpd
import pandas as pd
from shapely import speedups
from pathlib import Path
from tqdm import tqdm

if speedups.available:
    speedups.enable()

# Paths
polygons_fp = "/Users/rodriguez/Library/CloudStorage/OneDrive-WorldWildlifeFund,Inc/global_NCP/data/vector/grid_10k_coastal.gpkg"
points_fp = "/Users/rodriguez/Library/CloudStorage/OneDrive-WorldWildlifeFund,Inc/global_NCP/data/Spring/Inspring/coastal_risk_tnc_esa1992_2020_ch_f.gpkg"
output_fp = "/Users/rodriguez/Library/CloudStorage/OneDrive-WorldWildlifeFund,Inc/global_NCP/data/vector/grid_10k_coastal_calc_py.gpkg"

# Columns to summarize
cols_to_summarize = [
    "Rt_1992",
    "Rt_nohab_all_1992",
    "Rt_service_1992",
    "Rt_ratio_1992",
    "Rt_2020",
    "Rt_nohab_all_2020",
    "Rt_service_2020",
    "Rt_ratio_1992"
]

# Load data
grid = gpd.read_file(polygons_fp).reset_index(drop=True)
grid["fid"] = grid.index.astype(int)
points = gpd.read_file(points_fp)

# Ensure CRS match
if grid.crs != points.crs:
    points = points.to_crs(grid.crs)

# Build spatial index
points_sindex = points.sindex

results = []

# Process polygons in chunks
for _, poly in tqdm(grid.iterrows(), total=len(grid)):
    possible_matches_idx = list(points_sindex.intersection(poly.geometry.bounds))
    possible_matches = points.iloc[possible_matches_idx]
    precise_matches = possible_matches[possible_matches.intersects(poly.geometry)]

    row = {"fid": int(poly["fid"])}
    if precise_matches.empty:
        for col in cols_to_summarize:
            row[f"{col}_mean"] = None
            row[f"{col}_max"] = None
    else:
        for col in cols_to_summarize:
            if col in precise_matches.columns:
                row[f"{col}_mean"] = precise_matches[col].mean(skipna=True)
                row[f"{col}_max"] = precise_matches[col].max(skipna=True)
            else:
                row[f"{col}_mean"] = None
                row[f"{col}_max"] = None

    results.append(row)

summary_df = pd.DataFrame(results)

# Ensure fid column exists
if "fid" not in summary_df.columns:
    raise ValueError("No fid column found in summary_df. Check that results were appended correctly.")

# Merge back with grid
summary_gdf = grid.merge(summary_df, on="fid", how="left", validate="1:1")

# Export
summary_gdf.to_file(output_fp, driver="GPKG")
print(f"✓ Exported summarized file to {output_fp}")
