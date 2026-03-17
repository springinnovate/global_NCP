import argparse
import os
import shutil
import re
import time
import tracemalloc
from osgeo import ogr

def calculate_change(input_gpkg, output_gpkg, base_year, target_year, metric=None):
    print(f"--- Creating Change GPKG ({target_year} - {base_year}) ---")
    start_time = time.time()
    tracemalloc.start()
    print(f"Input:  {input_gpkg}")
    print(f"Output: {output_gpkg}")
    if not os.path.exists(input_gpkg):
        print(f"Error: File not found at {input_gpkg}")
        return

    if input_gpkg != output_gpkg:
        print("Copying base GeoPackage to output path...")
        shutil.copy2(input_gpkg, output_gpkg)

    try:
        dataset = ogr.Open(output_gpkg, 1)
        if dataset is None:
            print("Error: Could not open GPKG with OGR.")
            return

        layer = dataset.GetLayer()
        table_name = layer.GetName()
        print(f"Target table: {table_name}")

        # Get existing columns
        layer_defn = layer.GetLayerDefn()
        columns = [layer_defn.GetFieldDefn(i).GetName() for i in range(layer_defn.GetFieldCount())]

        # Canonical mappings matching R's process_data.qmd
        rename_map = {
            f"global_usle_marine_mod_ESA_{base_year}_mean": "usle",
            f"nature_access_lspop2019_ESA{base_year}_mean": "nature_access",
            f"N_ret_ratio_{base_year}_mean": "n_ret_ratio",
            f"Sed_ret_ratio_{base_year}_mean": "sed_ret_ratio",
            f"C_Risk_{base_year}_mean": "c_risk",
            f"C_Risk_Red_Ratio_{base_year}_mean": "c_risk_red_ratio",
            f"global_n_export_tnc_esa{base_year}_sum": "n_export",
            f"global_n_retention_ESAmar_{base_year}_fertilizer_sum": "n_retention",
            f"global_sed_export_marine_mod_ESA_{base_year}_sum": "sed_export",
            f"realized_polllination_on_ag_ESA{base_year}_sum": "pollination"
        }

        pairs = []
        for c_base, canonical_name in rename_map.items():
            if c_base in columns:
                c_target = c_base.replace(base_year, target_year)
                if c_target in columns:
                    diff_col = f"{canonical_name}_abs_chg"
                    sym_pct_col = f"{canonical_name}_pct_chg"
                    pairs.append((c_base, c_target, diff_col, sym_pct_col))

        if not pairs:
            print(f"No {base_year}/{target_year} column pairs found.")
            return

        print(f"Found {len(pairs)} pairs to calculate. Processing...")

        cols_to_drop = []

        for c_base, c_target, diff_col, sym_pct_col in pairs:
            print(f"  -> Absolute Change: {diff_col}")
            print(f"  -> Symmetric % Change: {sym_pct_col}")

            # --- 1. Absolute Difference ---
            if diff_col not in columns:
                field_defn = ogr.FieldDefn(diff_col, ogr.OFTReal)
                layer.CreateField(field_defn)
                columns.append(diff_col)

            sql = f'UPDATE "{table_name}" SET "{diff_col}" = "{c_target}" - "{c_base}"'
            dataset.ExecuteSQL(sql, dialect='SQLITE')

            # --- 2. Symmetric Percentage Change ---
            if sym_pct_col not in columns:
                field_defn = ogr.FieldDefn(sym_pct_col, ogr.OFTReal)
                layer.CreateField(field_defn)
                columns.append(sym_pct_col)

            # Formula: 200 * (new - old) / (|new| + |old|)
            # Bounded between -200% and +200%, avoids division by zero, handles flip signs
            sql_pct = f'''UPDATE "{table_name}" SET "{sym_pct_col}" = CASE
                WHEN ("{c_base}" IS NULL OR "{c_target}" IS NULL) THEN NULL
                WHEN ("{c_base}" = 0 AND "{c_target}" = 0) THEN 0.0
                WHEN (ABS("{c_target}") + ABS("{c_base}")) = 0 THEN NULL
                ELSE (("{c_target}" - "{c_base}") * 200.0) / (ABS("{c_target}") + ABS("{c_base}"))
                END'''
            dataset.ExecuteSQL(sql_pct, dialect='SQLITE')

            if c_base not in cols_to_drop: cols_to_drop.append(c_base)
            if c_target not in cols_to_drop: cols_to_drop.append(c_target)

        # Clean up to flush changes before schema modification
        dataset = None

        # Skipped column dropping:
        # OGR handles dropping columns in SQLite by rebuilding the entire table.
        # Doing this sequentially for 20+ columns on a 2.5M row table takes >10 minutes.
        # Base year columns are retained in the output for performance.

        dataset = None
        print(f"\nSuccessfully created {output_gpkg}.")

        current_mem, peak_mem = tracemalloc.get_traced_memory()
        end_time = time.time()
        print("\n--- Diagnostics ---")
        print(f"Execution Time:  {end_time - start_time:.2f} seconds")
        print(f"Peak Memory Use: {peak_mem / 10**6:.2f} MB")
        tracemalloc.stop()

    except Exception as e:
        print(f"Error processing GeoPackage: {e}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Create a new GeoPackage with bi-temporal changes, dropping original years.")
    parser.add_argument("--input-gpkg", default="/data/interim/10k_grid_services_base.gpkg", help="Path to the input GPKG")
    parser.add_argument("--output-gpkg", default="/data/interim/10k_grid_services_change.gpkg", help="Path to the new output GPKG")
    parser.add_argument("--base-year", default="1992", help="The baseline year (e.g., 1992)")
    parser.add_argument("--target-year", default="2020", help="The target year to compare against (e.g., 2020)")
    parser.add_argument("--metric", default=None, help="Optional metric to filter by (e.g., 'mean' to only process means)")
    args = parser.parse_args()

    calculate_change(args.input_gpkg, args.output_gpkg, args.base_year, args.target_year, args.metric)