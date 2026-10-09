# SPC vs absolute-change hotspots: overlap, where the two sets differ, and a map for sediment export
# (lab deck, 2026-10-09). Hotspots are each service's 5% most adverse cells over the evaluated 10 km
# cells, ranked once by SPC and once by absolute change (the paper's definition; this reproduces its
# overlap table). Both sets have the same size, so the overlap is the same read either way round.
#
# The 1992 baseline is recovered from the two change columns: SPC = 200 (b - a) / (a + b) and
# abs = b - a give a + b = 200 abs / SPC, so a = (a + b - abs) / 2 (undefined where SPC = 0).
#
# Usage: Rscript scripts/mapping/make_deck_metric_overlap.R
# Writes: outputs/tables/hotspot_metric_overlap_profile.csv, docs/presentations/images/metric_overlap_sed_export.png

suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(here) })

d <- readRDS(here("data", "processed", "dashboard", "grid_dashboard.rds"))
layers <- readRDS(here("data", "processed", "dashboard", "change_map_layers.rds"))
adverse_up <- c(n_export = TRUE, sed_export = TRUE, pollination = FALSE, nature_access = FALSE)
label <- c(n_export = "Nitrogen export", sed_export = "Sediment export", pollination = "Pollination",
           nature_access = "Nature access")

flag <- function(v, up) {
  cut <- quantile(v, if (up) 0.95 else 0.05, na.rm = TRUE, names = FALSE)
  !is.na(v) & (if (up) v >= cut else v <= cut)
}
top_region <- function(f) names(sort(table(d$region_wb[f]), decreasing = TRUE))[1]

sets <- list()
rows <- lapply(names(adverse_up), function(s) {
  p <- d[[paste0(s, "_pct_chg")]]; a <- d[[paste0(s, "_abs_chg")]]
  fp <- flag(p, adverse_up[[s]]); fa <- flag(a, adverse_up[[s]])
  sets[[s]] <<- list(spc_only = fp & !fa, both = fp & fa, abs_only = fa & !fp)
  base <- (ifelse(p != 0, 200 * a / p, NA) - a) / 2
  data.frame(service = label[[s]], hotspots = sum(fp), overlap_pct = 100 * sum(fp & fa) / sum(fp),
             baseline_spc_only = median(base[fp & !fa], na.rm = TRUE),
             baseline_abs_only = median(base[fa & !fp], na.rm = TRUE),
             spc_median_spc_only = median(p[fp & !fa]), spc_median_abs_only = median(p[fa & !fp]),
             people_M_spc_only = sum(d$pop[fp & !fa], na.rm = TRUE) / 1e6,
             people_M_both = sum(d$pop[fp & fa], na.rm = TRUE) / 1e6,
             people_M_abs_only = sum(d$pop[fa & !fp], na.rm = TRUE) / 1e6,
             top_region_spc_only = top_region(fp & !fa), top_region_abs_only = top_region(fa & !fp))
})
tab <- bind_rows(rows)
print(tab)
write.csv(tab, here("outputs", "tables", "hotspot_metric_overlap_profile.csv"), row.names = FALSE)

# ---- map: sediment export -----------------------------------------------------------------------
st <- sets$sed_export
cat_cell <- ifelse(st$both, "Both", ifelse(st$spc_only, "SPC only", ifelse(st$abs_only, "Absolute only", NA)))
px <- layers$pix$world
px$cat <- factor(cat_cell[px$row], levels = c("SPC only", "Both", "Absolute only"))
px <- px[!is.na(px$cat), ]
ylim <- c(-6.3e6, 8.4e6)
p <- ggplot() +
  geom_sf(data = layers$base, fill = "gray93", colour = "gray82", linewidth = 0.08) +
  geom_raster(data = px, aes(x, y, fill = cat)) +
  scale_fill_manual(values = c("SPC only" = "#F07D00", "Both" = "#3B3B3B", "Absolute only" = "#2C7FB8"),
                    name = "Sediment export\nhotspots") +
  guides(fill = guide_legend(ncol = 1)) +
  coord_sf(crs = "EPSG:8857", xlim = c(-1.25e7, 1.45e7), ylim = ylim, expand = FALSE, datum = NA) +
  theme_void() +
  theme(legend.position = "inside", legend.position.inside = c(0.01, 0.1), legend.justification = c(0, 0),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22),
        legend.key.size = unit(0.9, "cm"), plot.background = element_rect(fill = "white", colour = NA))
out <- here("docs", "presentations", "images", "metric_overlap_sed_export.png")
suppressWarnings(ggsave(out, p, width = 16, height = 16 * diff(ylim) / 2.7e7, dpi = 200, bg = "white"))
message("wrote ", out)
