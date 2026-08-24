# ==============================================================================
# Colombia — Priority Overlap: Critical Natural Assets ∩ Hotspots of Change
# ==============================================================================
#
# The IADB workshop deck's own notes (docs/presentations/
# idb_wwf_workshop1_case_study.qmd, "Opportunities" slide) already made this
# point and the Colombia report initially missed it: critical natural assets
# and hotspots of change are NOT two disconnected layers to show side by side
# -- "the overlap between 'valuable' and 'changing fastest' is the strongest
# possible case for intervention." This script computes that overlap directly
# for Colombia instead of just juxtaposing the two maps, to give road-corridor
# prioritization an actual signal: where is both true at once.
#
# Method: zonal-extract the critical-natural-assets raster (Chaplin-Kramer et
# al. 2022, value >2 = critical, the paper's 90% threshold) onto the 10km
# analysis grid using the same majority-rule (>=50% of cell area) threshold
# already used for the beneficiary-buffer masks elsewhere in this project, so
# a 10km cell counts as a critical-asset cell if >=50% of its area is >2.
# Cross-tabbed against the existing 5-service hotspot-of-change flag.
# ==============================================================================

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(here)

source(here("R", "paths.R"))

out_dir <- here("outputs", "plots", "colombia_report")
tbl_dir <- here("data", "processed", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

wwf_green      <- "#007930"
wwf_dark_green <- "#004D1E"
wwf_orange     <- "#F07D00"
wwf_teal       <- "#009191"
wwf_yellow     <- "#F5D200"

headline_svcs <- c("Pollination", "Sed_export", "N_export", "Nature_Access", "C_Risk")

# ------------------------------------------------------------------------------
# 1. Colombia grid + hotspot flag (any of 5 headline services)
# ------------------------------------------------------------------------------

message("Loading Colombia grid...")
grid_col <- st_read(here("data", "processed", "10k_change_calc.gpkg"), quiet = TRUE) %>%
  filter(nev_name == "Colombia") %>%
  select(grid_fid, geom)

hs <- st_read(here("data", "processed", "hotspots", "pct", "nev_name",
                    "hotspots_nev_name_Colombia_pct.gpkg"), quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(is_hotspot_any = rowSums(across(all_of(headline_svcs)), na.rm = TRUE) > 0) %>%
  filter(is_hotspot_any) %>%
  select(grid_fid, is_hotspot_any)

grid_col <- grid_col %>%
  left_join(hs, by = "grid_fid") %>%
  mutate(is_hotspot_any = tidyr::replace_na(is_hotspot_any, FALSE))

message("  -> ", sum(grid_col$is_hotspot_any), " hotspot-of-change cells")

# ------------------------------------------------------------------------------
# 2. Zonal-extract critical natural assets onto the same grid
# ------------------------------------------------------------------------------

message("Zonal-extracting critical natural assets raster...")
raster_path <- file.path(data_dir(), "external", "critical_natural_assets", "local_NCP_all_targets",
                          "local_NCP_land_all_targets_md5_7ccece.tif")
r <- rast(raster_path)
col_bbox <- st_bbox(grid_col)
r_col <- crop(r, ext(col_bbox["xmin"] - 0.5, col_bbox["xmax"] + 0.5,
                      col_bbox["ymin"] - 0.5, col_bbox["ymax"] + 0.5))
r_crit_bin <- r_col > 2  # binary: fine-resolution cell is a critical natural asset

grid_col_vect <- vect(grid_col)
frac_crit <- terra::extract(r_crit_bin, grid_col_vect, fun = mean, na.rm = TRUE, ID = FALSE)
grid_col$frac_critical <- frac_crit[[1]]
grid_col$frac_critical[is.na(grid_col$frac_critical)] <- 0
grid_col$is_critical <- grid_col$frac_critical >= 0.5

message("  -> ", sum(grid_col$is_critical), " critical-natural-asset cells (>=50% of area)")

# Continuous priority rank (1-20), not just the binary >2 flag -- the binary
# threshold alone flattens a real intensity gradient. Empirically (2026-08-13
# check), Orinoquia's critical cells skew heavily toward "barely above
# threshold" (66.8% rank<5) vs. the rest of Colombia (25.1% rank<5), so a flat
# green fill overrepresents how critical that region actually is relative to
# e.g. the paramo cluster. Used below to modulate fill opacity so cell-level
# intensity remains visible instead of being collapsed into a single hue.
mean_rank <- terra::extract(r_col, grid_col_vect, fun = mean, na.rm = TRUE, ID = FALSE)
grid_col$mean_rank <- mean_rank[[1]]
grid_col$mean_rank[is.na(grid_col$mean_rank)] <- 0

# ------------------------------------------------------------------------------
# 3. Cross-tab and the headline overlap statistic
# ------------------------------------------------------------------------------

grid_col <- grid_col %>%
  mutate(category = case_when(
    is_critical & is_hotspot_any  ~ "Prioridad (ambos)",
    is_critical & !is_hotspot_any ~ "Solo activo crítico",
    !is_critical & is_hotspot_any ~ "Solo hotspot de cambio",
    TRUE ~ "Ninguno"
  ))

xtab <- grid_col %>% st_drop_geometry() %>% count(category) %>%
  mutate(pct = n / sum(n) * 100)

n_critical <- sum(grid_col$is_critical)
n_both <- sum(grid_col$is_critical & grid_col$is_hotspot_any)
pct_critical_also_hotspot <- if (n_critical > 0) (n_both / n_critical) * 100 else NA

n_hotspot <- sum(grid_col$is_hotspot_any)
pct_hotspot_also_critical <- if (n_hotspot > 0) (n_both / n_hotspot) * 100 else NA

summary_stats <- tibble::tibble(
  metric = c("n_critical_cells", "n_hotspot_cells", "n_both",
             "pct_critical_also_hotspot", "pct_hotspot_also_critical"),
  value = c(n_critical, n_hotspot, n_both, pct_critical_also_hotspot, pct_hotspot_also_critical)
)
print(summary_stats)
print(xtab)

write.csv(xtab, file.path(tbl_dir, "colombia_priority_overlap_xtab.csv"), row.names = FALSE)
write.csv(summary_stats, file.path(tbl_dir, "colombia_priority_overlap_summary.csv"), row.names = FALSE)
message("Saved overlap tables.")

# ------------------------------------------------------------------------------
# 4. Map: 4-category priority overlap
# ------------------------------------------------------------------------------

cat_colors <- c(
  "Prioridad (ambos)"      = "#8B0000",  # dark red -- the actual priority signal
  "Solo activo crítico"    = wwf_green,
  "Solo hotspot de cambio" = wwf_orange,
  "Ninguno"                = "gray92"
)
cat_levels <- c("Prioridad (ambos)", "Solo activo crítico", "Solo hotspot de cambio", "Ninguno")
grid_col$category <- factor(grid_col$category, levels = cat_levels)

subtitle_txt <- sprintf("%.1f%% de los hotspots de cambio de Colombia caen dentro de un activo natural crítico — la señal de mayor prioridad para la planificación vial", pct_hotspot_also_critical)
subtitle_txt <- paste(strwrap(subtitle_txt, width = 55), collapse = "\n")

p <- ggplot(grid_col) +
  geom_sf(aes(fill = category, alpha = mean_rank), color = NA) +
  scale_fill_manual(values = cat_colors, name = NULL) +
  scale_alpha(range = c(0.3, 1), guide = "none") +
  labs(
    title = "Colombia — Dónde coinciden valor y cambio",
    subtitle = subtitle_txt,
    caption = "El tono más claro/oscuro dentro de cada color refleja el rango de prioridad subyacente (1-20):\nun cruce con tono claro es apenas crítico, uno oscuro es fuertemente crítico."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = wwf_dark_green),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30", lineheight = 1.2),
    plot.caption = element_text(size = 8, hjust = 0.5, color = "gray45", margin = margin(t = 10)),
    legend.position = "right",
    legend.text = element_text(size = 9.5),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(file.path(out_dir, "colombia_priority_overlap_map.png"), p, width = 9.5, height = 10, dpi = 200, bg = "white")
message("Saved priority overlap map.")
message("\nDone.")
