"""Merge a new zonally-extracted variable into a copy of the canonical change-calc GPKG.

Written 2026-08-31 while adding coastal protection (C_Prot_service) to the 5-service redesign,
as a template for the next time a new/changed variable needs to be merged in without a full
process_data.qmd re-run from raw zonal files (which requires files that, in practice, don't
survive between sessions -- see docs/runbook.md's Step 0).

WHY THIS USES RAW SQL, NOT GEOPANDAS -- READ BEFORE "SIMPLIFYING" THIS SCRIPT:
GeoPackage's `fid` primary-key column is handled inconsistently across geopandas/pyogrio
versions -- it can silently become the DataFrame's row index (unnamed, not even labeled "fid")
instead of a normal column, depending on the specific file and library version installed. Code
that assumes `df["fid"]` exists, or joins `on="fid"`, can fail loudly (best case) or silently
join on the wrong rows (worst case). A `.gpkg` file is a SQLite database, so reading via plain
`sqlite3` sidesteps this entirely -- `fid` is always an unambiguous, ordinary column that way.
See docs/runbook.md's "Recurring risk category: fid/grid_fid handling" section for the second,
independent time this project has been bitten by ID-handling bugs of this general shape.

Usage (edit the CONFIG block below, then run):
    python scripts/merge_new_variable_into_change_calc.py
"""

import shutil
import sqlite3

# =============================== CONFIG =====================================
BASE_GPKG = r"C:\projects\global_NCP\data\processed\10k_change_calc.gpkg"
BASE_TABLE = "10k_change_calc"

NEW_GPKG = r"C:\projects\global_NCP\summary_pipeline_workspace_ha\grid_10km_land_synth_zonal_2026_08_29_02_31_31.gpkg"
NEW_TABLE = "grid_10km_land_synth_zonal_2026_08_29_02_31_31"
# Raw per-year columns in NEW_GPKG to pull in and compute change for.
NEW_VALUE_COLS = ["C_Prot_service_1992_mean", "C_Prot_service_2020_mean"]

# Output: a clearly-named COPY, never overwrite BASE_GPKG directly until the result is verified
# and someone deliberately promotes it (rename/copy over the canonical file by hand).
OUT_GPKG = r"C:\projects\global_NCP\data\processed\10k_change_calc_DRYRUN_coastal.gpkg"

# Canonical change-column prefix + the two output column names this run produces.
CHANGE_PREFIX = "c_prot_service"
YEAR_1992_COL, YEAR_2020_COL = NEW_VALUE_COLS
# ==============================================================================


def main():
    print(f"Copying base file ({BASE_GPKG}) -> {OUT_GPKG} (plain file copy, geometry untouched)...")
    shutil.copyfile(BASE_GPKG, OUT_GPKG)

    con = sqlite3.connect(OUT_GPKG)
    cur = con.cursor()
    cur.execute("ATTACH DATABASE ? AS newdb", (NEW_GPKG,))

    # Hard checks before touching anything -- fail loudly rather than silently mis-joining.
    base_total, base_distinct = cur.execute(
        f'SELECT COUNT(*), COUNT(DISTINCT fid) FROM "{BASE_TABLE}"'
    ).fetchone()
    new_total, new_distinct = cur.execute(
        f'SELECT COUNT(*), COUNT(DISTINCT fid) FROM newdb."{NEW_TABLE}"'
    ).fetchone()
    print(f"Base: {base_total} rows, {base_distinct} distinct fid")
    print(f"New:  {new_total} rows, {new_distinct} distinct fid")
    assert base_distinct == base_total, "base table has duplicate fid values -- stop"
    assert new_distinct == new_total, "new table has duplicate fid values -- stop"
    assert base_total == new_total, (
        f"row count mismatch ({base_total} vs {new_total}) -- these should both be the master "
        "grid's full cell count; if a scoped/filtered extraction was used instead, the LEFT JOIN "
        "below still works (unmatched fids get NULL) but double-check this is expected"
    )

    # GPKG RTree-maintenance triggers fire on ANY update to the table, not just geom/fid changes,
    # and their WHEN-clause calls SpatiaLite's ST_IsEmpty(), which plain sqlite3 doesn't have.
    # Safe to drop on this disposable copy because this script never touches fid or geom.
    for trig in (
        f"rtree_{BASE_TABLE}_geom_update3",
        f"rtree_{BASE_TABLE}_geom_update4",
    ):
        cur.execute(f'DROP TRIGGER IF EXISTS "{trig}"')

    abs_col = f"{CHANGE_PREFIX}_abs_chg"
    pct_col = f"{CHANGE_PREFIX}_pct_chg"
    for col in [*NEW_VALUE_COLS, abs_col, pct_col]:
        cur.execute(f'ALTER TABLE "{BASE_TABLE}" ADD COLUMN "{col}" REAL')

    set_clause = ", ".join(
        f'"{c}" = (SELECT n."{c}" FROM newdb."{NEW_TABLE}" n WHERE n.fid = "{BASE_TABLE}".fid)'
        for c in NEW_VALUE_COLS
    )
    cur.execute(f'UPDATE "{BASE_TABLE}" SET {set_clause}')
    print(f"Rows updated with raw values: {cur.rowcount}")

    # Exact formulas from the paper's Methods (Zonal Summaries and Symmetric Percentage Change):
    #   abs change: delta_S = S2020 - S1992
    #   SPC = (S2020 - S1992) / ((|S2020| + |S1992|) / 2) * 100
    cur.execute(f'''
        UPDATE "{BASE_TABLE}"
        SET "{abs_col}" = "{YEAR_2020_COL}" - "{YEAR_1992_COL}",
            "{pct_col}" = CASE
                WHEN (ABS("{YEAR_1992_COL}") + ABS("{YEAR_2020_COL}")) = 0 THEN NULL
                ELSE ("{YEAR_2020_COL}" - "{YEAR_1992_COL}")
                     / ((ABS("{YEAR_1992_COL}") + ABS("{YEAR_2020_COL}")) / 2.0) * 100
            END
        WHERE "{YEAR_1992_COL}" IS NOT NULL AND "{YEAR_2020_COL}" IS NOT NULL
    ''')
    print(f"Rows with computed change: {cur.rowcount}")
    con.commit()

    # Sanity check: don't just trust a handful of contiguous fids (a real, unlucky no-change
    # stretch looked like a bug at first glance during the coastal run -- verify identical-vs-
    # different counts across the WHOLE non-null set, not a small ORDER-BY-fid sample).
    n_nonnull = cur.execute(f'SELECT COUNT(*) FROM "{BASE_TABLE}" WHERE "{abs_col}" IS NOT NULL').fetchone()[0]
    n_identical = cur.execute(
        f'SELECT COUNT(*) FROM "{BASE_TABLE}" WHERE "{YEAR_1992_COL}" = "{YEAR_2020_COL}"'
    ).fetchone()[0]
    print(f"\nNon-null {abs_col}: {n_nonnull}")
    print(f"Of those, identical 1992==2020: {n_identical} ({100*n_identical/n_nonnull:.1f}%)")
    print("If this is at or near 100%, investigate before trusting the result -- it likely means")
    print("both year rasters were accidentally built from the same source.")

    con.close()
    print(f"\nDone. Copy written to: {OUT_GPKG}")
    print("This is a disposable comparison copy -- it is NOT the canonical file. Promote it")
    print("manually (rename over 10k_change_calc.gpkg) only after verifying the results.")


if __name__ == "__main__":
    main()
