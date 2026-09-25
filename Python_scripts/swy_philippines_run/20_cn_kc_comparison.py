"""The CN/Kc factorial against WWF-SIPA's parameters, on the current Kc (per-pixel NDVI Kamble +
Negron Juarez forest + Oliveira grassland/shrub, 07g). Replaces the 2026-09-21 comparison
(14_factorial_scatter_comparison.py), whose "our Kc" cells used the old per-class EVI Kc.

    our CN + our Kc   09l   workspace_becky_inputs_90m_ndvi_perpixel_kc
    her CN + our Kc   09m   workspace_becky_inputs_90m_herCN_ndvi_perpixel_kc
    our CN + her Kc   09f   workspace_becky_inputs_90m_ourCN_herKc   (our CN unchanged since, still valid)
    her CN + her Kc   09h   workspace_becky_inputs_90m_herCN_herKc   (pipeline check, still valid)

All four share DEM, grid, climate, rain events and soil group; only the biophysical inputs differ.

Reference (2026-09-25, user decision): her CN + her Kc run through this pipeline on the 90m DEM
(09h), not WWF-SIPA's own 30m output. 09h reproduces the 30m run closely (r=0.92 QF and B) and sits
on the same grid as the other runs, so these comparisons isolate the CN/Kc choice from resolution
and pipeline differences. The other three cells are compared against it. QF depends on CN only, so
only the our-CN cell differs from the reference for QF.

Outputs:
    docs/reports/swy/swy_ph_cn_kc_scatter_b.png (3 panels), swy_ph_cn_kc_scatter_qf.png (1 panel)
    data/swy/philippines/comparison_maps/cn_kc_summary.csv       (AOI ratio/r per cell)
    data/swy/philippines/comparison_maps/cn_kc_by_class.csv      (per class per cell)
"""
import os

import numpy as np
import pandas as pd
import rasterio
from rasterio.warp import Resampling, reproject

WS = "data/swy/philippines"
RUNS = {
    # current run (09n): our CN, our per-pixel Kc incl. FAO-56 paddy on Annual Crop. The pre-paddy
    # run (09l) is left out of the figures on purpose (user decision 2026-09-25: noise for the reader)
    "our CN + our Kc": (f"{WS}/workspace_becky_inputs_90m_paddy_kc", "ph_paddy_kc"),
    "her CN + our Kc": (f"{WS}/workspace_becky_inputs_90m_herCN_ndvi_perpixel_kc", "ph_herCN_ndvi_perpixel_kc"),
    "our CN + her Kc": (f"{WS}/workspace_becky_inputs_90m_ourCN_herKc", "ph_becky_inputs_90m_ourCN_herKc"),
}
QF_RUNS = ["our CN + our Kc"]  # QF depends on CN only; her-CN runs have the reference's QF
REFERENCE_WS = f"{WS}/workspace_becky_inputs_90m_herCN_herKc"
REFERENCE_LABEL = "her CN + her Kc (90m)"
BASELINE = {
    "QF": f"{REFERENCE_WS}/QF_ph_becky_inputs_90m_herCN_herKc.tif",
    "B": f"{REFERENCE_WS}/B_ph_becky_inputs_90m_herCN_herKc.tif",
}
BASELINE_LULC_PATH = f"{WS}/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif"
MASK_DIR = f"{WS}/comparison_maps"
REPORT_DIR = "docs/reports/swy"

LULC_CLASSES = {
    1: "Annual Crop", 2: "Brush/Shrubs", 3: "Built-up", 4: "Closed Forest", 5: "Fishpond",
    6: "Grassland", 7: "Inland Water", 8: "Mangrove Forest", 9: "Marshland/Swamp",
    10: "Open Forest", 11: "Open/Barren", 12: "Perennial Crop",
}
LULC_COLORS = {
    1: "#e6ab02", 2: "#a6761d", 3: "#666666", 4: "#1b9e77", 5: "#377eb8",
    6: "#66a61e", 7: "#1f78b4", 8: "#7570b3", 9: "#984ea3", 10: "#4daf4a",
    11: "#e7298a", 12: "#d95f02",
}
N_PLOT_SAMPLE = 200_000


def _resample_to_ref(src_path, ref_path, resampling):
    with rasterio.open(ref_path) as ref:
        ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape
    with rasterio.open(src_path) as src:
        dst = np.full(ref_shape, np.nan, dtype="float64")
        reproject(
            source=rasterio.band(src, 1), destination=dst,
            src_transform=src.transform, src_crs=src.crs,
            dst_transform=ref_transform, dst_crs=ref_crs,
            resampling=resampling, src_nodata=src.nodata, dst_nodata=np.nan,
        )
    return dst


def _read(path):
    with rasterio.open(path) as src:
        arr = src.read(1).astype("float64")
        if src.nodata is not None:
            arr[np.isclose(arr, src.nodata)] = np.nan
        return arr, src.transform


def compare(var, run_names):
    paths = {name: f"{RUNS[name][0]}/{var}_{RUNS[name][1]}.tif" for name in run_names}
    ref_path = paths[run_names[0]]
    runs, transforms = {}, set()
    for name, path in paths.items():
        runs[name], t = _read(path)
        transforms.add(tuple(t))
    assert len(transforms) == 1, f"{var}: runs are not on the same grid"

    baseline = _resample_to_ref(BASELINE[var], ref_path, Resampling.average)
    lulc_all = _resample_to_ref(BASELINE_LULC_PATH, ref_path, Resampling.mode)
    with rasterio.open(os.path.join(MASK_DIR, f"{var.lower()}_valid_mask.tif")) as m:
        valid = m.read(1) == 1
    valid &= np.isfinite(baseline) & np.isfinite(lulc_all)
    for arr in runs.values():
        valid &= np.isfinite(arr)

    x = baseline[valid]
    lulc = lulc_all[valid].astype(int)
    ys = {name: arr[valid] for name, arr in runs.items()}
    del runs, baseline, lulc_all

    summary, by_class = [], []
    for name, y in ys.items():
        summary.append({"var": var, "combination": name, "n_pixels": x.size,
                        "ratio": y.mean() / x.mean(), "r": np.corrcoef(x, y)[0, 1]})
        for code, cname in LULC_CLASSES.items():
            m = lulc == code
            if m.sum() < 100:
                continue
            by_class.append({"var": var, "combination": name, "class": cname, "n_pixels": int(m.sum()),
                             "baseline_mean": x[m].mean(), "run_mean": y[m].mean(),
                             "ratio": y[m].mean() / x[m].mean(), "r": np.corrcoef(x[m], y[m])[0, 1]})

    rng = np.random.default_rng(42)
    idx = rng.choice(x.size, N_PLOT_SAMPLE, replace=False) if x.size > N_PLOT_SAMPLE else np.arange(x.size)
    lim = max(np.percentile(x[idx], 99.9), max(np.percentile(y[idx], 99.9) for y in ys.values())) * 1.05

    import matplotlib.pyplot as plt

    ncols = 2 if len(run_names) == 4 else min(len(run_names), 3)
    nrows = int(np.ceil(len(run_names) / ncols))
    fig, axes = plt.subplots(nrows, ncols, figsize=(5.4 * ncols + 0.5, 5.6 * nrows), dpi=120, squeeze=False)
    for ax, name in zip(axes.flat, run_names):
        y = ys[name]
        for code, cname in LULC_CLASSES.items():
            m = lulc[idx] == code
            if m.any():
                ax.scatter(x[idx][m], y[idx][m], s=2, alpha=0.25, color=LULC_COLORS[code],
                           label=cname, linewidths=0, rasterized=True)
        ax.plot([0, lim], [0, lim], "k--", linewidth=1)
        ax.set_xlim(0, lim)
        ax.set_ylim(0, lim)
        ax.set_aspect("equal")
        s = next(r for r in summary if r["combination"] == name)
        ax.set_title(f"{name}\nratio {s['ratio']:.2f}, r = {s['r']:.2f}", fontsize=11)
        ax.set_xlabel(f"{REFERENCE_LABEL} {var} (mm/yr)")
        ax.set_ylabel(f"This pipeline {var} (mm/yr)")
    for ax in list(axes.flat)[len(run_names):]:
        ax.set_visible(False)
    handles, labels = axes.flat[0].get_legend_handles_labels()
    leg = fig.legend(handles, labels, loc="lower center", ncol=6, markerscale=6, fontsize=8, frameon=False)
    for h in leg.legend_handles:
        h.set_alpha(1)
    fig.tight_layout(rect=(0, 0.12, 1, 1))
    out_png = os.path.join(REPORT_DIR, f"swy_ph_cn_kc_scatter_{var.lower()}.png")
    fig.savefig(out_png, bbox_inches="tight")
    plt.close(fig)
    print(f"Written to {out_png}")
    return summary, by_class


def main():
    summary, by_class = [], []
    for var, names in (("QF", QF_RUNS), ("B", list(RUNS))):
        s, c = compare(var, names)
        summary += s
        by_class += c
    summary_df, class_df = pd.DataFrame(summary), pd.DataFrame(by_class)
    summary_df.to_csv(os.path.join(MASK_DIR, "cn_kc_summary.csv"), index=False)
    class_df.to_csv(os.path.join(MASK_DIR, "cn_kc_by_class.csv"), index=False)
    print("\n== AOI ==")
    print(summary_df.round(3).to_string(index=False))
    print("\n== B ratio by class ==")
    print(class_df[class_df["var"] == "B"].pivot(index="class", columns="combination", values="ratio")
          .round(2)[list(RUNS)].to_string())
    print(f"(reference for all ratios/r: {REFERENCE_LABEL})")


if __name__ == "__main__":
    main()
