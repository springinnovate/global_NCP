# ==============================================================================
# Colombia — Per-Service Priority Overlap: Critical Natural Assets ∩ Hotspots
# ==============================================================================
#
# make_colombia_priority_overlap.R answers "does the AGGREGATE 12-NCP critical-
# asset layer overlap with hotspots of change (any of 5 services)?" -- 57.9% of
# hotspot cells are also critical. This script asks the sharper, per-service
# version raised in a meeting with Becky Chaplin-Kramer: for each individual
# service, does THAT service's own hotspot-of-change cells fall inside THAT
# same service's own single-NCP critical-asset solution (not the pooled one)?
#
# Per-service solutions come from the paper's OSF repo (r5xz7), "solutions"
# folder, single-NCP scenarios (90% target only, binary 0/1, not the 1-20 rank
# used for the aggregate raster). Letter-code mapping documented in
# data/external/critical_natural_assets/per_service_solutions/README.md.
# These rasters are in Eckert IV equal-area (global template, NA outside the
# solved country) -- trimmed + reprojected to EPSG:4326 (nearest-neighbor, to
# preserve the binary classification) to match this project's WGS84 grid.
#
# Same >=50% cell-area majority rule as the aggregate overlap script.
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

solutions_dir <- file.path(data_dir(), "external", "critical_natural_assets", "per_service_solutions")

services <- list(
  list(flag_col = "Pollination",   solution_file = "solution_B1_pollination_col_target-90.tif",             label_es = "Polinización"),
  list(flag_col = "Sed_export",    solution_file = "solution_D1_sediment_retention_500km_col_target-90.tif", label_es = "Exportación de sedimentos"),
  list(flag_col = "N_export",      solution_file = "solution_A1_nitrogen_retention_500km_col_target-90.tif", label_es = "Exportación de nitrógeno"),
  list(flag_col = "Nature_Access", solution_file = "solution_Z_nature_access_1hr_col_target-90.tif",         label_es = "Acceso a la naturaleza"),
  list(flag_col = "C_Risk",        solution_file = "solution_S_coastal_risk_reduction_col_target-90.tif",    label_es = "Riesgo costero")
)

# ------------------------------------------------------------------------------
# 1. Colombia grid + per-service hotspot flags
# ------------------------------------------------------------------------------

message("Loading Colombia grid + per-service hotspot flags...")
grid_col <- st_read(here("data", "processed", "10k_change_calc.gpkg"), quiet = TRUE) %>%
  filter(nev_name == "Colombia") %>%
  select(grid_fid, geom)

hs <- st_read(here("data", "processed", "hotspots", "pct", "nev_name",
                    "hotspots_nev_name_Colombia_pct.gpkg"), quiet = TRUE) %>%
  st_drop_geometry() %>%
  select(grid_fid, Pollination, Sed_export, N_export, Nature_Access, C_Risk)

grid_col_vect_base <- grid_col %>% left_join(hs, by = "grid_fid")

# ------------------------------------------------------------------------------
# 2. Per-service zonal extraction + cross-tab
# ------------------------------------------------------------------------------

results <- list()

for (svc in services) {
  message("Processing ", svc$flag_col, " (", svc$solution_file, ")...")

  r <- rast(file.path(solutions_dir, svc$solution_file))
  r <- trim(r)
  r_wgs84 <- project(r, "EPSG:4326", method = "near")

  g <- grid_col_vect_base %>% select(grid_fid, all_of(svc$flag_col))
  g_vect <- vect(g)

  frac_crit <- terra::extract(r_wgs84, g_vect, fun = mean, na.rm = TRUE, ID = FALSE)
  g$frac_critical <- frac_crit[[1]]
  g$frac_critical[is.na(g$frac_critical)] <- 0
  g$is_critical_svc <- g$frac_critical >= 0.5
  g$is_hotspot_svc <- g[[svc$flag_col]] %>% tidyr::replace_na(FALSE)

  n_critical <- sum(g$is_critical_svc)
  n_hotspot <- sum(g$is_hotspot_svc)
  n_both <- sum(g$is_critical_svc & g$is_hotspot_svc)
  pct_hotspot_also_critical <- if (n_hotspot > 0) (n_both / n_hotspot) * 100 else NA
  pct_critical_also_hotspot <- if (n_critical > 0) (n_both / n_critical) * 100 else NA

  results[[svc$flag_col]] <- tibble::tibble(
    service = svc$flag_col,
    label_es = svc$label_es,
    n_critical_cells = n_critical,
    n_hotspot_cells = n_hotspot,
    n_both = n_both,
    pct_hotspot_also_critical = pct_hotspot_also_critical,
    pct_critical_also_hotspot = pct_critical_also_hotspot
  )
}

summary_df <- bind_rows(results)
print(summary_df)

write.csv(summary_df, file.path(tbl_dir, "colombia_per_service_overlap_summary.csv"), row.names = FALSE)
message("Saved: colombia_per_service_overlap_summary.csv")

# ------------------------------------------------------------------------------
# 3. Chart: per-service overlap vs. the aggregate (12-NCP pooled) headline
# ------------------------------------------------------------------------------

agg_path <- file.path(tbl_dir, "colombia_priority_overlap_summary.csv")
agg_pct <- NA
if (file.exists(agg_path)) {
  agg_tbl <- read.csv(agg_path)
  agg_pct <- agg_tbl$value[agg_tbl$metric == "pct_hotspot_also_critical"]
}

p <- ggplot(summary_df, aes(x = pct_hotspot_also_critical, y = reorder(label_es, pct_hotspot_also_critical))) +
  geom_col(fill = wwf_green, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%  (%d/%d)", pct_hotspot_also_critical, n_both, n_hotspot_cells)),
            hjust = -0.05, size = 3.6, color = "gray20") +
  { if (!is.na(agg_pct)) geom_vline(xintercept = agg_pct, linetype = "dashed", color = wwf_orange) } +
  scale_x_continuous(expand = expansion(mult = c(0, 0.32)), limits = c(0, NA)) +
  labs(
    title = "Colombia — Coincidencia valor-cambio, servicio por servicio",
    subtitle = paste(strwrap(sprintf(
      "De los hotspots de cambio de CADA servicio, ¿qué %% cae en el activo natural crítico de ESE MISMO servicio (no el agregado)? Línea punteada = %.1f%%, el valor agregado de 12 NCP.",
      agg_pct), width = 85), collapse = "\n"),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = wwf_dark_green),
    plot.subtitle = element_text(size = 9.5, color = "gray30", margin = margin(b = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11.5, face = "bold"),
    axis.text.x = element_blank()
  )

ggsave(file.path(out_dir, "colombia_per_service_overlap.png"), p, width = 9.5, height = 4.5, dpi = 200, bg = "white")
message("Saved: colombia_per_service_overlap.png")

message("\nDone.")
