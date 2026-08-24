# ==============================================================================
# Colombia — Critical Natural Assets: Global Share + Intra-Country Biome Breakdown
# ==============================================================================
#
# Parallel statistical treatment to what already exists for hotspots of change
# (colombia_share_vs_expected_*.png, colombia_biome_relative_intensity_*.png):
# the report frames critical natural assets and hotspots of change as two
# integrated layers, so critical assets should get the same "share vs.
# expected" and "which biome concentrates it" analysis, not just a map with a
# one-line caption.
#
# Denominator note: unlike the hotspot-of-change stats (which use this
# project's own 10km analysis grid, ~1.37M valid land cells), this uses the
# critical-natural-assets raster's OWN native land mask (non-NA cells,
# ~43.5M globally at ~2.5km resolution) as "eligible land" -- it's a
# separately-published dataset (Chaplin-Kramer et al. 2022) with its own land
# definition, not derived from this project's grid. Computed directly in
# raster space (global(), not a 1.37M-polygon zonal extract) for speed.
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

raster_path <- file.path(data_dir(), "external", "critical_natural_assets", "local_NCP_all_targets",
                          "local_NCP_land_all_targets_md5_7ccece.tif")
r <- rast(raster_path)

# ------------------------------------------------------------------------------
# 1. Global share vs. expected (Colombia's % of world's critical-asset land
#    vs. Colombia's % of world's eligible land, per this raster's own mask)
# ------------------------------------------------------------------------------

message("Computing global critical-asset cell counts...")
r_crit <- r > 2
n_eligible_global <- as.numeric(global(r, fun = "notNA")[1, 1])
n_critical_global <- as.numeric(global(r_crit, fun = "sum", na.rm = TRUE)[1, 1])

grid_col <- st_read(here("data", "processed", "10k_change_calc.gpkg"), quiet = TRUE) %>%
  filter(nev_name == "Colombia")
col_bbox <- st_bbox(grid_col)
ext_col <- ext(col_bbox["xmin"] - 0.5, col_bbox["xmax"] + 0.5,
                col_bbox["ymin"] - 0.5, col_bbox["ymax"] + 0.5)
r_col <- crop(r, ext_col)
r_crit_col <- r_col > 2

n_eligible_col <- as.numeric(global(r_col, fun = "notNA")[1, 1])
n_critical_col <- as.numeric(global(r_crit_col, fun = "sum", na.rm = TRUE)[1, 1])

pct_share_global <- (n_critical_col / n_critical_global) * 100
pct_expected_global <- (n_eligible_col / n_eligible_global) * 100
relative_intensity <- pct_share_global / pct_expected_global

global_stats <- tibble::tibble(
  metric = c("n_critical_global", "n_critical_colombia", "n_eligible_global", "n_eligible_colombia",
             "pct_share_global", "pct_expected_global", "relative_intensity"),
  value = c(n_critical_global, n_critical_col, n_eligible_global, n_eligible_col,
            pct_share_global, pct_expected_global, relative_intensity)
)
print(global_stats)
write.csv(global_stats, file.path(tbl_dir, "colombia_cna_global_share.csv"), row.names = FALSE)
message("Saved: colombia_cna_global_share.csv")

# Chart: same visual language as colombia_share_vs_expected_*.png
share_df <- tibble::tibble(
  metric_label = c("% de activos críticos globales en Colombia", "% de tierra elegible global que es Colombia"),
  value = c(pct_share_global, pct_expected_global)
)
p_share <- ggplot(share_df, aes(y = "Activos naturales\ncríticos", x = value, fill = metric_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.5) +
  geom_text(aes(label = sprintf("%.2f%%", value)),
            position = position_dodge(width = 0.75), hjust = -0.15, size = 3.8, color = "gray20") +
  scale_fill_manual(values = c(
    "% de activos críticos globales en Colombia" = wwf_green,
    "% de tierra elegible global que es Colombia" = "gray70"
  ), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(
    title = "Participación de Colombia en activos naturales críticos globales",
    subtitle = paste(strwrap(sprintf("Intensidad relativa: %.2f× — Colombia concentra %s de lo que su área por sí sola predeciría.",
                                       relative_intensity, ifelse(relative_intensity > 1, "más", "menos")), width = 80), collapse = "\n"),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = wwf_dark_green),
    plot.subtitle = element_text(size = 10.5, color = "gray30", margin = margin(b = 10)),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold")
  )
ggsave(file.path(out_dir, "colombia_cna_share_vs_expected.png"), p_share, width = 9, height = 3.8, dpi = 200, bg = "white")
message("Saved: colombia_cna_share_vs_expected.png")

# ------------------------------------------------------------------------------
# 2. Intra-Colombia biome breakdown for critical natural assets
#    (reuses the frac_critical zonal extraction already used for the priority
#    overlap -- adds WWF_biome grouping instead of the hotspot cross-tab)
# ------------------------------------------------------------------------------

message("Zonal-extracting critical-asset fraction per grid cell for biome breakdown...")
grid_col_biome <- grid_col %>% select(grid_fid, WWF_biome)
r_crit_bin <- r_col > 2
grid_col_vect <- vect(grid_col_biome)
frac_crit <- terra::extract(r_crit_bin, grid_col_vect, fun = mean, na.rm = TRUE, ID = FALSE)
grid_col_biome$frac_critical <- frac_crit[[1]]
grid_col_biome$frac_critical[is.na(grid_col_biome$frac_critical)] <- 0
grid_col_biome$is_critical <- grid_col_biome$frac_critical >= 0.5

biome_short <- c(
  "Tropical & Subtropical Moist Broadleaf Forests" = "Bosque húmedo tropical",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands" = "Sabanas/Llanos",
  "Tropical & Subtropical Dry Broadleaf Forests" = "Bosque seco tropical",
  "Deserts & Xeric Shrublands" = "Desierto/Xérico",
  "Montane Grasslands & Shrublands" = "Páramo/Montano",
  "Mangroves" = "Manglares"
)

biome_df <- grid_col_biome %>%
  st_drop_geometry() %>%
  filter(!is.na(WWF_biome)) %>%
  group_by(WWF_biome) %>%
  summarise(n_total = n(), n_critical = sum(is_critical), .groups = "drop") %>%
  filter(n_total >= 50) %>%
  mutate(
    expected_share_intra = (n_total / sum(n_total)) * 100,
    pct_share_intra = (n_critical / sum(n_critical)) * 100,
    relative_intensity_intra = pct_share_intra / expected_share_intra,
    biome_es = recode(WWF_biome, !!!biome_short)
  )
print(biome_df)
write.csv(biome_df, file.path(tbl_dir, "colombia_cna_biome_stats.csv"), row.names = FALSE)
message("Saved: colombia_cna_biome_stats.csv")

p_biome <- ggplot(biome_df, aes(x = relative_intensity_intra, y = reorder(biome_es, relative_intensity_intra),
                                  fill = relative_intensity_intra > 1)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  geom_text(aes(label = sprintf("%.2f×", relative_intensity_intra)), hjust = -0.15, size = 3.6, color = "gray20") +
  scale_fill_manual(values = c("TRUE" = wwf_green, "FALSE" = wwf_teal), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "¿Qué bioma concentra los activos naturales críticos dentro de Colombia?",
    subtitle = paste(strwrap("Intensidad relativa intra-Colombia: participación del bioma en los activos críticos de Colombia vs. su participación en el área total de Colombia (biomas con ≥50 celdas).", width = 85), collapse = "\n"),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = wwf_dark_green),
    plot.subtitle = element_text(size = 9.5, color = "gray30", margin = margin(b = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11.5, face = "bold")
  )
ggsave(file.path(out_dir, "colombia_cna_biome_relative_intensity.png"), p_biome, width = 9, height = 4.5, dpi = 200, bg = "white")
message("Saved: colombia_cna_biome_relative_intensity.png")

message("\nDone.")
