import os
from pathlib import Path
import geopandas as gpd
import pandas as pd
from exactextract import exact_extract
from exactextract.raster import GDALRasterSource
from ecoshard import taskgraph
import psutil
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

def extract_single_raster(raster_path, vector_path, subfolder, raster_key):
    """Worker function to process a single raster, handling C++ GEOS bottlenecks."""
    logging.info(f"Worker starting: {subfolder} - {raster_key}")
    
    # Load the canonical master grid (already clean and enriched with all attributes)
    gdf = gpd.read_file(vector_path, on_invalid="ignore")
    
    # Define the columns we want to keep
    cols = ['country', 'region_wb', 'income_grp', 'WWF_biome']
    
    # Subset the dataframe to ONLY the columns we need to prevent C++ type crashes on unused fields
    gdf = gdf[cols + ['geometry']].copy()

    # Force the attribute columns to standard string types to avoid C++ unhandled type errors (like Pandas Int64)
    for col in cols:
        gdf[col] = gdf[col].astype(str)

    # Use strategy="raster-sequential" to prevent exactextract memory leaks on complex multipolygons.
    stats_df = exact_extract(
        rast=GDALRasterSource(str(raster_path), 1),
        vec=gdf,
        ops=["sum"],
        include_cols=cols,
        output="pandas",
        strategy="raster-sequential"
    )
    
    stats_df.rename(columns={'sum': 'population'}, inplace=True)
    stats_df['overlap_category'] = subfolder
    stats_df['exposure_type'] = raster_key
    
    # Group by all the requested attributes
    group_cols = ['overlap_category', 'exposure_type'] + cols
    return stats_df.groupby(group_cols, dropna=False)['population'].sum().reset_index()

def extract_population():
    base_dir = Path("data/processed/hotspots/hotspot_beneficiaries")
    vector_path = Path("data/vector_basedata/landgrid_1_clean_enriched_4326.gpkg")
    
    if not vector_path.exists():
        logging.error(f"Vector file not found at {vector_path}")
        return
        
    # Auto-discover overlap-category subfolders instead of a hardcoded list --
    # Rich delivers one folder per beneficiary category (today: the 4 nested
    # "N or more services" tiers; upcoming: water-hotspot and access-hotspot,
    # which aren't nested tiers at all). Dropping in a new folder is enough to
    # have it picked up here; no code change needed.
    subfolders = sorted(p.name for p in base_dir.iterdir() if p.is_dir())
    logging.info(f"Discovered {len(subfolders)} overlap-category folders: {subfolders}")
    
    workspace_dir = Path("summary_pipeline_workspace/population_extraction_v3")
    workspace_dir.mkdir(parents=True, exist_ok=True)
    
    # FORCE n_workers=1 to prevent RAM Out-of-Memory (OOM) crashes when reading the 1.5M row GPKG
    task_graph = taskgraph.TaskGraph(
        str(workspace_dir),
        n_workers=1, 
        reporting_interval=5.0
    )
    
    task_list = []
    all_results = []
    
    for subfolder in subfolders:
        folder_path = base_dir / subfolder
        if not folder_path.exists():
            logging.warning(f"Folder not found: {folder_path}")
            continue
            
        logging.info(f"Processing folder: {subfolder}")
        
        raster_files = {
            'hydrological': 'full_raster_extent_downstream_50k_population.tif',
            'travel_footprint': 'full_raster_extent_within_travel_time_population.tif',
            'combined_total': 'full_raster_extent_union_population.tif'
        }
        
        for raster_key, file_name in raster_files.items():
            raster_path = folder_path / file_name
            if not raster_path.exists():
                logging.warning(f"Raster not found: {raster_path}")
                continue
                
            logging.info(f"Adding task for: {subfolder} -> {raster_key}")
            task = task_graph.add_task(
                func=extract_single_raster,
                args=(raster_path, vector_path, subfolder, raster_key),
                store_result=True,
                task_name=f"extract_{subfolder}_{raster_key}"
            )
            task_list.append(task)
            
    task_graph.join()
    task_graph.close()
    
    # Safely get results, preventing a single failure from crashing the entire CSV write
    all_results = []
    for task in task_list:
        try:
            res = task.get()
            if res is not None:
                all_results.append(res)
        except Exception as e:
            logging.error(f"Task {task.task_name} failed with error: {e}")

    if not all_results:
        logging.error("No data extracted.")
        return
        
    final_df = pd.concat(all_results, ignore_index=True)
    
    global_df = final_df.groupby(['overlap_category', 'exposure_type'])['population'].sum().reset_index()
    global_df['region_wb'] = 'Global'
    global_df['income_grp'] = 'Global'
    global_df['country'] = 'Global'
    global_df['WWF_biome'] = 'Global'
    
    combined_summary = pd.concat([final_df, global_df], ignore_index=True)
    
    output_dir = Path("outputs/tables")
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / "exposure_comparison_compiled.csv"
    
    combined_summary.to_csv(output_path, index=False)
    logging.info(f"Results saved to {output_path}")

if __name__ == '__main__':
    extract_population()
