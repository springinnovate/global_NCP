"""Unit tests for calculate_bitemporal_change_functions.py.

Pure logic, no GDAL/osgeo needed — runs in the plain local venv, not just inside Docker.
"""

import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'Python_scripts'))

from calculate_bitemporal_change_functions import (
    build_rename_map,
    match_column_pairs,
    build_absolute_diff_sql,
    build_symmetric_pct_change_sql,
)


def test_build_rename_map_uses_base_year_in_keys():
    rename_map = build_rename_map('1992')
    assert 'C_Risk_1992_mean' in rename_map
    assert rename_map['C_Risk_1992_mean'] == 'c_risk'


def test_match_column_pairs_only_returns_complete_pairs():
    columns = [
        'fid', 'geom',
        'global_usle_marine_mod_ESA_1992_mean', 'global_usle_marine_mod_ESA_2020_mean',
        'C_Risk_1992_mean', 'C_Risk_2020_mean',
        'N_ret_ratio_1992_mean',  # no 2020 counterpart on purpose
    ]
    rename_map = build_rename_map('1992')
    pairs = match_column_pairs(columns, rename_map, '1992', '2020')

    canonical_names = {p[2].removesuffix('_abs_chg') for p in pairs}
    assert canonical_names == {'usle', 'c_risk'}
    assert len(pairs) == 2


def test_match_column_pairs_empty_when_no_columns_match():
    assert match_column_pairs(['fid', 'geom'], build_rename_map('1992'), '1992', '2020') == []


def test_build_absolute_diff_sql_subtracts_base_from_target():
    sql = build_absolute_diff_sql('mytable', 'usle_abs_chg', 'usle_2020', 'usle_1992')
    assert sql == 'UPDATE "mytable" SET "usle_abs_chg" = "usle_2020" - "usle_1992"'


def test_build_symmetric_pct_change_sql_is_bounded_formula():
    sql = build_symmetric_pct_change_sql('mytable', 'usle_pct_chg', 'usle_1992', 'usle_2020')
    assert 'CASE' in sql
    assert '200.0' in sql
    assert '"usle_1992" IS NULL OR "usle_2020" IS NULL' in sql
