"""Merge sediment retention (via the legacy-grid crosswalk) and re-confirm coastal protection
into one combined dry-run copy of the canonical change-calc GPKG.

Written 2026-08-31, same session as merge_new_variable_into_change_calc.py (read that file's
docstring first -- same raw-sqlite3-not-geopandas rationale applies here).

WHY A CROSSWALK IS NEEDED HERE, UNLIKE THE COASTAL MERGE:
10k_grid_synth_all.gpkg (a March 2026 zonal-extraction intermediate, regeneration later disabled
by default -- see analysis/WORKLOG.md 2026-08-31 entry) already has raw 1992/2020 USLE and
sediment-export levels, which turned out to still survive locally despite the raw *rasters*
themselves being gone (see docs/HANDOFF_2026-08-31.md). But that file is built on the legacy
LC-derived grid (1,691,819 rows), not the current master grid (1,522,073 rows, 10k_change_calc.gpkg)
-- the same grid mismatch that caused the 2026-07-08 LCC striping bug. This script joins through
data/processed/lc_grid_fid_to_master_fid_crosswalk.csv, restricted to valid_match=TRUE rows, which
were independently re-validated this session via each file's own GPKG rtree bounding boxes (not
just trusting the crosswalk's own claimed match_dist_m) -- 99.4% of rows are exact (0.0m) centroid
matches; the many-to-one collisions (~155K master cells with 2-9 source rows) were confirmed to be
exact-value duplicates (a legacy antimeridian artifact), safe to collapse with AVG().

Also carries over the raw N/sediment numerator+denominator components (not just the amounts), so
a future Figure-2/Annex regeneration can aggregate ratios correctly (sum-of-numerator /
sum-of-denominator at each biome/region/country/income-group grouping) instead of naively
averaging the per-cell ratio -- see analysis/WORKLOG.md 2026-08-31 for the full reasoning.

Usage:
    python scripts/merge_sediment_and_coastal_via_crosswalk.py
"""

import csv
import shutil
import sqlite3

# =============================== CONFIG =====================================
BASE_GPKG = r"C:\projects\global_NCP\data\processed\10k_change_calc_BACKUP_2026-08-31.gpkg"
BASE_TABLE = "10k_change_calc"

SYNTH_GPKG = r"C:\projects\global_NCP\data\processed\10k_grid_synth_all.gpkg"
SYNTH_TABLE = "10k_grid_synth_all"
CROSSWALK_CSV = r"C:\projects\global_NCP\data\processed\lc_grid_fid_to_master_fid_crosswalk.csv"

COASTAL_DRYRUN_GPKG = r"C:\projects\global_NCP\data\processed\10k_change_calc_DRYRUN_coastal.gpkg"
COASTAL_TABLE = "10k_change_calc"
COASTAL_COLS = ["c_prot_service_1992_mean", "c_prot_service_2020_mean",
                "c_prot_service_abs_chg", "c_prot_service_pct_chg"]

OUT_GPKG = r"C:\projects\global_NCP\data\processed\10k_change_calc_DRYRUN_all3.gpkg"

# synth_all raw columns -> canonical names to carry over (amounts + ratio components)
SYNTH_COLS = {
    "usle_1992_mean": "usle_1992",
    "usle_2020_mean": "usle_2020",
    "global_sed_export_marine_mod_ESA_1992_mean": "sed_export_raw_1992",
    "global_sed_export_marine_mod_ESA_2020_mean": "sed_export_raw_2020",
    "global_n_export_tnc_esa1992_mean": "n_export_raw_1992",
    "global_n_export_tnc_esa2020_mean": "n_export_raw_2020",
    "global_n_retention_ESAmar_1992_fertilizer_mean": "n_retention_raw_1992",
    "global_n_retention_ESAmar_2020_fertilizer_mean": "n_retention_raw_2020",
}
# ==============================================================================


def spc(new_col, old_col):
    return (f'CASE WHEN (ABS("{old_col}") + ABS("{new_col}")) = 0 THEN NULL '
            f'ELSE ("{new_col}" - "{old_col}") / ((ABS("{old_col}") + ABS("{new_col}")) / 2.0) * 100 END')


def main():
    print(f"Copying base file -> {OUT_GPKG} ...")
    shutil.copyfile(BASE_GPKG, OUT_GPKG)

    con = sqlite3.connect(OUT_GPKG)
    cur = con.cursor()

    for trig in (f"rtree_{BASE_TABLE}_geom_update3", f"rtree_{BASE_TABLE}_geom_update4"):
        cur.execute(f'DROP TRIGGER IF EXISTS "{trig}"')

    base_total, base_distinct = cur.execute(
        f'SELECT COUNT(*), COUNT(DISTINCT fid) FROM "{BASE_TABLE}"'
    ).fetchone()
    assert base_distinct == base_total, "base table has duplicate fid values -- stop"
    print(f"Base: {base_total} rows, {base_distinct} distinct fid")

    # ---------------------------------------------------------------- coastal
    print("\n--- Re-confirming coastal (same master grid, direct fid join) ---")
    cur.execute("ATTACH DATABASE ? AS coastaldb", (COASTAL_DRYRUN_GPKG,))
    c_total, c_distinct = cur.execute(
        f'SELECT COUNT(*), COUNT(DISTINCT fid) FROM coastaldb."{COASTAL_TABLE}"'
    ).fetchone()
    assert c_distinct == c_total, "coastal dryrun table has duplicate fid values -- stop"
    assert c_total == base_total, f"coastal row count {c_total} != base {base_total}"

    for col in COASTAL_COLS:
        cur.execute(f'ALTER TABLE "{BASE_TABLE}" ADD COLUMN "{col}" REAL')
    set_clause = ", ".join(
        f'"{c}" = (SELECT n."{c}" FROM coastaldb."{COASTAL_TABLE}" n WHERE n.fid = "{BASE_TABLE}".fid)'
        for c in COASTAL_COLS
    )
    cur.execute(f'UPDATE "{BASE_TABLE}" SET {set_clause}')
    print(f"Coastal columns copied, rows touched: {cur.rowcount}")
    con.commit()

    # ---------------------------------------------------------- sediment/N via crosswalk
    print("\n--- Loading crosswalk (valid_match=TRUE only) ---")
    with open(CROSSWALK_CSV, newline="") as f:
        reader = csv.DictReader(f)
        cw_rows = [
            (int(r["lc_grid_fid"]), int(r["master_fid"]), float(r["match_dist_m"]))
            for r in reader if r["valid_match"] == "TRUE"
        ]
    print(f"Crosswalk rows (valid only): {len(cw_rows)}")

    cur.execute("CREATE TEMP TABLE crosswalk (lc_grid_fid INTEGER, master_fid INTEGER, match_dist_m REAL)")
    cur.executemany("INSERT INTO crosswalk VALUES (?, ?, ?)", cw_rows)
    cur.execute("CREATE INDEX idx_cw_lc ON crosswalk(lc_grid_fid)")
    cur.execute("CREATE INDEX idx_cw_master ON crosswalk(master_fid)")

    # Refined 2026-08-31: the crosswalk is many-to-one for ~155K master_fid. Most (153,516) are
    # genuine multi-way ties (every matched row within ~0m -- an antimeridian-type exact-duplicate
    # legacy cell). But 1,609 groups have one clearly-best (~0m) match plus a clearly-worse
    # secondary match (4,600-5,900m away) that the crosswalk let through as valid_match=TRUE too --
    # discovered because averaging that bad secondary match into LC-driver data produced 572
    # genuinely conflicting fids in hotspot_extraction.qmd's parallel LC-driver join. The AVG below
    # happened to land on identical values in the one case spot-checked before this fix, but that
    # was luck, not guaranteed -- restrict to each group's best match(es) before aggregating.
    cur.execute("""
        CREATE TEMP TABLE crosswalk_best AS
        SELECT c.lc_grid_fid, c.master_fid
        FROM crosswalk c
        JOIN (SELECT master_fid, MIN(match_dist_m) AS best_dist FROM crosswalk GROUP BY master_fid) b
          ON b.master_fid = c.master_fid
        WHERE c.match_dist_m <= b.best_dist + 1.0
    """)
    n_dropped = len(cw_rows) - cur.execute("SELECT COUNT(*) FROM crosswalk_best").fetchone()[0]
    print(f"Dropped {n_dropped} distant secondary matches, keeping only each group's best match(es).")

    print("Attaching synth_all and aggregating raw levels onto master_fid (AVG collapses exact "
          "duplicate legacy cells, confirmed identical-valued this session) ...")
    cur.execute("ATTACH DATABASE ? AS synthdb", (SYNTH_GPKG,))

    synth_select_cols = ", ".join(f'AVG(s."{raw}") AS "{canon}"' for raw, canon in SYNTH_COLS.items())
    cur.execute(f'''
        CREATE TEMP TABLE agg_synth AS
        SELECT cw.master_fid AS master_fid, {synth_select_cols}
        FROM crosswalk_best cw
        JOIN synthdb."{SYNTH_TABLE}" s ON s.fid = cw.lc_grid_fid
        GROUP BY cw.master_fid
    ''')
    cur.execute("CREATE UNIQUE INDEX idx_agg_master ON agg_synth(master_fid)")
    n_agg = cur.execute("SELECT COUNT(*) FROM agg_synth").fetchone()[0]
    print(f"Aggregated rows (distinct master_fid with a crosswalk match): {n_agg} / {base_total}")

    new_cols = list(SYNTH_COLS.values()) + [
        "sed_retention_1992", "sed_retention_2020", "sed_retention_abs_chg", "sed_retention_pct_chg",
    ]
    for col in new_cols:
        cur.execute(f'ALTER TABLE "{BASE_TABLE}" ADD COLUMN "{col}" REAL')

    set_clause = ", ".join(
        f'"{canon}" = (SELECT a."{canon}" FROM agg_synth a WHERE a.master_fid = "{BASE_TABLE}".fid)'
        for canon in SYNTH_COLS.values()
    )
    cur.execute(f'UPDATE "{BASE_TABLE}" SET {set_clause}')
    print(f"Raw N/sediment component columns populated, rows touched: {cur.rowcount}")

    cur.execute(f'''
        UPDATE "{BASE_TABLE}"
        SET "sed_retention_1992" = "usle_1992" - "sed_export_raw_1992",
            "sed_retention_2020" = "usle_2020" - "sed_export_raw_2020"
        WHERE "usle_1992" IS NOT NULL AND "sed_export_raw_1992" IS NOT NULL
          AND "usle_2020" IS NOT NULL AND "sed_export_raw_2020" IS NOT NULL
    ''')
    print(f"sed_retention (amount) computed, rows: {cur.rowcount}")

    cur.execute(f'''
        UPDATE "{BASE_TABLE}"
        SET "sed_retention_abs_chg" = "sed_retention_2020" - "sed_retention_1992",
            "sed_retention_pct_chg" = {spc("sed_retention_2020", "sed_retention_1992")}
        WHERE "sed_retention_1992" IS NOT NULL AND "sed_retention_2020" IS NOT NULL
    ''')
    print(f"sed_retention change computed, rows: {cur.rowcount}")
    con.commit()

    # ---------------------------------------------------------------- diagnostics
    print("\n--- Diagnostics (whole non-null set, not an ordered-by-fid sample) ---")
    for label, abs_col, y1, y2 in [
        ("sed_retention", "sed_retention_abs_chg", "sed_retention_1992", "sed_retention_2020"),
        ("c_prot_service", "c_prot_service_abs_chg", "c_prot_service_1992_mean", "c_prot_service_2020_mean"),
    ]:
        n_nonnull = cur.execute(f'SELECT COUNT(*) FROM "{BASE_TABLE}" WHERE "{abs_col}" IS NOT NULL').fetchone()[0]
        n_identical = cur.execute(
            f'SELECT COUNT(*) FROM "{BASE_TABLE}" WHERE "{y1}" = "{y2}"'
        ).fetchone()[0]
        pct = 100 * n_identical / n_nonnull if n_nonnull else float("nan")
        print(f"{label}: non-null={n_nonnull}, identical 1992==2020={n_identical} ({pct:.1f}%)")

    con.close()
    print(f"\nDone. Combined dry-run written to: {OUT_GPKG}")
    print("Disposable comparison copy -- NOT canonical. Promote manually only after review.")


if __name__ == "__main__":
    main()
