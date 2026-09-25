"""Step 1 diagnostic: does the EVI-based Kc preserve the same RELATIVE pattern across land-cover
classes as the validated NDVI-based Kamble regression would, or does the EVI/NDVI scale mismatch
also distort the pattern (not just the absolute level)?

Computes zonal-mean NDVI per class per month on Becky's own LULC (same method 07_build_
biophysical_table.py already uses for Borneo), applies the SAME Kamble formula used throughout
this project, and compares against the already-computed EVI-based Kc already sitting in
ph_biophysical_table_ncp_kc_cn.csv.
"""
import csv
import glob
import numpy as np
import rasterio
from rasterio.warp import Resampling, reproject

LULC_PATH = "data/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif"
NDVI_DIR = "data/swy/philippines/inputs/mod13a3_ndvi_2020"
EXISTING_TABLE = "data/swy/philippines/INPUTS_SP/ph_biophysical_table_ncp_kc_cn.csv"

QA_GOOD_MAX = 0b01

ndvi_files = sorted(glob.glob(f"{NDVI_DIR}/MOD13A3.061__1_km_monthly_NDVI_doy2020*.tif"))
qa_files = sorted(glob.glob(f"{NDVI_DIR}/MOD13A3.061__1_km_monthly_VI_Quality_doy2020*.tif"))
assert len(ndvi_files) == 12 and len(qa_files) == 12, f"{len(ndvi_files)}/{len(qa_files)}"

with rasterio.open(ndvi_files[0]) as ref:
    ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape

with rasterio.open(LULC_PATH) as lulc_src:
    lulc_resampled = np.empty(ref_shape, dtype=lulc_src.dtypes[0])
    reproject(
        source=rasterio.band(lulc_src, 1), destination=lulc_resampled,
        src_transform=lulc_src.transform, src_crs=lulc_src.crs,
        dst_transform=ref_transform, dst_crs=ref_crs, resampling=Resampling.mode,
    )

classes = sorted(int(c) for c in np.unique(lulc_resampled) if c not in (0, 255))
monthly_class_ndvi = {c: [] for c in classes}

for ndvi_f, qa_f in zip(ndvi_files, qa_files):
    with rasterio.open(ndvi_f) as nsrc:
        ndvi = nsrc.read(1).astype("float32")
        nodata = nsrc.nodata
    with rasterio.open(qa_f) as qsrc:
        qa = qsrc.read(1)
    good = ((qa & 0b11) <= QA_GOOD_MAX) & (ndvi != nodata)
    ndvi_scaled = ndvi * 0.0001
    for c in classes:
        mask_c = good & (lulc_resampled == c)
        monthly_class_ndvi[c].append(float(ndvi_scaled[mask_c].mean()) if mask_c.sum() > 0 else np.nan)

ndvi_kc = {}
for c in classes:
    vals = np.array(monthly_class_ndvi[c])
    if np.isnan(vals).any() and not np.isnan(vals).all():
        vals[np.isnan(vals)] = np.nanmean(vals)
    kc_ndvi = [max(0.05, round(1.457 * float(v) - 0.1725, 3)) for v in vals]
    ndvi_kc[c] = {"ndvi_mean_annual": round(float(np.nanmean(vals)), 3), "kc_ndvi": kc_ndvi}

with open(EXISTING_TABLE, newline="") as f:
    evi_rows = list(csv.DictReader(f))

FIXED = {3, 5, 7}  # Built-up, Fishpond, Inland Water -- fixed Kc, never EVI/NDVI-derived
veg = []
for row in evi_rows:
    lulc_id = int(row["lulc_id"])
    if lulc_id in FIXED or lulc_id not in ndvi_kc:
        continue
    kc_evi = [float(row[f"Kc_{i+1}"]) for i in range(12)]
    kc_ndvi = ndvi_kc[lulc_id]["kc_ndvi"]
    veg.append({
        "lulc_id": lulc_id, "desc": row["lulc_description"],
        "kc_evi_annual": sum(kc_evi) / 12, "kc_ndvi_annual": sum(kc_ndvi) / 12,
    })

veg.sort(key=lambda r: -r["kc_ndvi_annual"])
print("\n== Per-class annual-mean Kc: EVI-based (current) vs NDVI-based (Kamble-validated) ==")
print(f"{'class':<18}{'Kc_evi':>10}{'Kc_ndvi':>10}")
for r in veg:
    print(f"{r['desc']:<18}{r['kc_evi_annual']:>10.3f}{r['kc_ndvi_annual']:>10.3f}")

evi_vals = np.array([r["kc_evi_annual"] for r in veg])
ndvi_vals = np.array([r["kc_ndvi_annual"] for r in veg])
pearson_r = np.corrcoef(evi_vals, ndvi_vals)[0, 1]


def rank(a):
    order = np.argsort(-a)
    ranks = np.empty_like(order, dtype=float)
    ranks[order] = np.arange(1, len(a) + 1)
    return ranks


evi_rank, ndvi_rank = rank(evi_vals), rank(ndvi_vals)
spearman_r = np.corrcoef(evi_rank, ndvi_rank)[0, 1]

print(f"\nPearson r (linear agreement): {pearson_r:.3f}")
print(f"Spearman r (rank-order agreement, i.e. is the PATTERN preserved): {spearman_r:.3f}")

print("\n== Class ranking comparison (1 = highest Kc) ==")
print(f"{'class':<18}{'EVI_rank':>10}{'NDVI_rank':>10}{'diff':>8}")
for r, er, nr in sorted(zip(veg, evi_rank, ndvi_rank), key=lambda x: x[2]):
    print(f"{r['desc']:<18}{er:>10.0f}{nr:>10.0f}{abs(er-nr):>8.0f}")

OUT_CSV = "analysis/scratch_evi_vs_ndvi_kc_by_class.csv"
with open(OUT_CSV, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["lulc_id", "class", "kc_evi_annual", "kc_ndvi_annual", "evi_rank", "ndvi_rank"])
    for r, er, nr in zip(veg, evi_rank, ndvi_rank):
        w.writerow([r["lulc_id"], r["desc"], round(r["kc_evi_annual"], 4), round(r["kc_ndvi_annual"], 4), int(er), int(nr)])
print(f"\nWritten to {OUT_CSV}")
