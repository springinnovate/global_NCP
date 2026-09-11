"""Pure bitemporal-change logic for the 10km grid GPKG: no GDAL/OGR, no I/O.

Kept separate from calculate_bitemporal_change_tasks.py so the column-matching and SQL-formula
logic is testable without a GeoPackage or the osgeo bindings at all.
"""


def build_rename_map(base_year):
    """The base_year-parameterised column -> canonical-name mapping.

    Canonical mappings matching R's process_data.qmd. All variables use _mean: rasters are
    per-hectare normalised before extraction, so mean gives a comparable per-ha rate across
    equal-area cells. Regional totals are handled separately via Path A.

    Args:
        base_year (str): the baseline year, e.g. "1992".

    Returns:
        dict: raw base-year column name -> canonical short name.
    """
    return {
        f"global_usle_marine_mod_ESA_{base_year}_mean": "usle",
        f"nature_access_lspop2019_ESA{base_year}_mean": "nature_access",
        f"N_ret_ratio_{base_year}_mean": "n_ret_ratio",
        f"Sed_ret_ratio_{base_year}_mean": "sed_ret_ratio",
        f"C_Risk_{base_year}_mean": "c_risk",
        f"C_Risk_Red_Ratio_{base_year}_mean": "c_risk_red_ratio",
        f"global_n_export_tnc_esa{base_year}_mean": "n_export",
        f"global_n_retention_ESAmar_{base_year}_fertilizer_mean": "n_retention",
        f"global_sed_export_marine_mod_ESA_{base_year}_mean": "sed_export",
        f"realized_polllination_on_ag_ESA{base_year}_mean": "pollination",
    }


def match_column_pairs(columns, rename_map, base_year, target_year):
    """The base/target column pairs actually present in a layer's columns.

    Args:
        columns (list): column names on the layer.
        rename_map (dict): base-year column name -> canonical short name, from build_rename_map.
        base_year (str): the baseline year.
        target_year (str): the year to compare against.

    Returns:
        list: (c_base, c_target, diff_col, sym_pct_col) tuples, one per matched pair.
    """
    pairs = []
    for c_base, canonical_name in rename_map.items():
        if c_base in columns:
            c_target = c_base.replace(base_year, target_year)
            if c_target in columns:
                diff_col = f"{canonical_name}_abs_chg"
                sym_pct_col = f"{canonical_name}_pct_chg"
                pairs.append((c_base, c_target, diff_col, sym_pct_col))
    return pairs


def build_absolute_diff_sql(table_name, diff_col, c_target, c_base):
    """The UPDATE statement writing target-minus-base into diff_col.

    Args:
        table_name (str): the GPKG layer/table name.
        diff_col (str): the column to write into.
        c_target (str): the target-year column.
        c_base (str): the base-year column.

    Returns:
        str: a SQLite UPDATE statement.
    """
    return f'UPDATE "{table_name}" SET "{diff_col}" = "{c_target}" - "{c_base}"'


def build_symmetric_pct_change_sql(table_name, sym_pct_col, c_base, c_target):
    """The UPDATE statement writing symmetric percent change into sym_pct_col.

    Formula: 200 * (new - old) / (|new| + |old|). Bounded between -200% and +200%, avoids
    division by zero, handles sign flips.

    Args:
        table_name (str): the GPKG layer/table name.
        sym_pct_col (str): the column to write into.
        c_base (str): the base-year column.
        c_target (str): the target-year column.

    Returns:
        str: a SQLite UPDATE statement.
    """
    return f'''UPDATE "{table_name}" SET "{sym_pct_col}" = CASE
        WHEN ("{c_base}" IS NULL OR "{c_target}" IS NULL) THEN NULL
        WHEN ("{c_base}" = 0 AND "{c_target}" = 0) THEN 0.0
        WHEN (ABS("{c_target}") + ABS("{c_base}")) = 0 THEN NULL
        ELSE (("{c_target}" - "{c_base}") * 200.0) / (ABS("{c_target}") + ABS("{c_base}"))
        END'''
