"""GPKG I/O for the bitemporal-change step: OGR/SQLite calls, orchestrating the pure logic
in calculate_bitemporal_change_functions.py.
"""

import os
import shutil
import time
import tracemalloc
from osgeo import ogr

from calculate_bitemporal_change_functions import (
    build_rename_map,
    match_column_pairs,
    build_absolute_diff_sql,
    build_symmetric_pct_change_sql,
)


def calculate_change(input_gpkg_path, output_gpkg_path, base_year, target_year, metric=None):
    """Write absolute and symmetric-percent bitemporal change columns into a copy of the grid GPKG.

    Args:
        input_gpkg_path (str): path to the input GPKG carrying both base_year and target_year
            columns.
        output_gpkg_path (str): path to write the change GPKG to; copied from input_gpkg_path
            first when the two paths differ.
        base_year (str): the baseline year, e.g. "1992".
        target_year (str): the year to compare against, e.g. "2020".
        metric (str): unused — accepted for interface compatibility with the original script.

    Returns:
        None.
    """
    print (f"--- Creating Change GPKG ({target_year} - {base_year}) ---")
    start_time = time.time()
    tracemalloc.start()
    print (f"Input:  {input_gpkg_path}")
    print (f"Output: {output_gpkg_path}")
    if not os.path.exists(input_gpkg_path):
        print (f"Error: File not found at {input_gpkg_path}")
        return

    if input_gpkg_path != output_gpkg_path:
        print ("Copying base GeoPackage to output path...")
        shutil.copy2(input_gpkg_path, output_gpkg_path)

    try:
        dataset = ogr.Open(output_gpkg_path, 1)
        if dataset is None:
            print ("Error: Could not open GPKG with OGR.")
            return

        layer = dataset.GetLayer()
        table_name = layer.GetName()
        print (f"Target table: {table_name}")

        layer_defn = layer.GetLayerDefn()
        columns = [layer_defn.GetFieldDefn(i).GetName() for i in range(layer_defn.GetFieldCount())]

        rename_map = build_rename_map(base_year)
        pairs = match_column_pairs(columns, rename_map, base_year, target_year)

        if not pairs:
            print (f"No {base_year}/{target_year} column pairs found.")
            return

        print (f"Found {len(pairs)} pairs to calculate. Processing...")

        cols_to_drop = []

        for c_base, c_target, diff_col, sym_pct_col in pairs:
            print (f"  -> Absolute Change: {diff_col}")
            print (f"  -> Symmetric % Change: {sym_pct_col}")

            # --- 1. Absolute Difference ---
            if diff_col not in columns:
                field_defn = ogr.FieldDefn(diff_col, ogr.OFTReal)
                layer.CreateField(field_defn)
                columns.append(diff_col)

            dataset.ExecuteSQL(
                build_absolute_diff_sql(table_name, diff_col, c_target, c_base), dialect='SQLITE')

            # --- 2. Symmetric Percentage Change ---
            if sym_pct_col not in columns:
                field_defn = ogr.FieldDefn(sym_pct_col, ogr.OFTReal)
                layer.CreateField(field_defn)
                columns.append(sym_pct_col)

            dataset.ExecuteSQL(
                build_symmetric_pct_change_sql(table_name, sym_pct_col, c_base, c_target),
                dialect='SQLITE')

            if c_base not in cols_to_drop:
                cols_to_drop.append(c_base)
            if c_target not in cols_to_drop:
                cols_to_drop.append(c_target)

        # Clean up to flush changes before schema modification
        dataset = None

        # Skipped column dropping:
        # OGR handles dropping columns in SQLite by rebuilding the entire table.
        # Doing this sequentially for 20+ columns on a 2.5M row table takes >10 minutes.
        # Base year columns are retained in the output for performance.

        dataset = None
        print (f"\nSuccessfully created {output_gpkg_path}.")

        current_mem, peak_mem = tracemalloc.get_traced_memory()
        end_time = time.time()
        print ("\n--- Diagnostics ---")
        print (f"Execution Time:  {end_time - start_time:.2f} seconds")
        print (f"Peak Memory Use: {peak_mem / 10**6:.2f} MB")
        tracemalloc.stop()

    except Exception as e:
        print (f"Error processing GeoPackage: {e}")
