# R helper structure

Most functions are already split into thematic files, but here’s a quick map so we can gradually consolidate without breaking the package structure:

- **Hotspot core**:  
  `extract_hotspots.R`, `hotposts_export.R`, `get_hotspots.R`, `identify_hotspots.R`, `KS_helpers.R`, `ks_hotspots.R`.
- **Plotting** (bar/violin/KS/ECDF):  
  `change_bars.R`, `hotspot_violins.R`, `plot_hotspots.R`, `plot_hotspot_boxviolin.R`, `plot_hotspot_density_bin2d.R`, `plot_ks_*`, `save_ECDF.R`, `ecdf_grid.R`.
- **Data prep / geometry utilities**:  
  `agg_change*.R`, `pct_change_calc.R`, `paths.R`, `cookiecutteR.R`, `coalesced_left_join_sf.R`, `rast_alignR.R`, `iterate_lcc_metrics.R`, etc.

When moving helpers into subdirectories, remember:

1. Update `NAMESPACE` exports (via roxygen) so `devtools::load_all()` still works.
2. Keep file names referenced by `source()` (e.g., `analysis/hotspot_extraction.qmd`) in sync if you relocate them.
3. Run `R CMD check` or the critical chunks after any restructuring.

Feel free to rename files with clearer prefixes (e.g., `plot_*.R`, `hotspot_*.R`) as we clean things up; just make sure roxygen headers still point to the new file paths.
