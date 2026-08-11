# ==============================================================================
# Colombia — Intra-Country Biome Analysis
# ==============================================================================
#
# Extension of the Colombia CLEC/Sandra report (2026-08-11/12): instead of
# comparing Colombia against the rest of the world, this compares Colombia's
# own biomes against each other -- which biome carries a disproportionate
# share of Colombia's own hotspots of change, relative to that biome's own
# share of Colombia's eligible land for that service. Same logic as the
# country-vs-world calc in hotspot_area_stats_Colombia.csv, nested one level
# down (biome-vs-Colombia instead of country-vs-world), and using the same
# valid-cells-per-service fix (WORKLOG.md 2026-08-11, Coastal Risk
# denominator bug) so Coastal Risk's narrow coastal eligibility is handled
# correctly here too.
# ==============================================================================

library(sf)
library(dplyr)
library(tidyr)
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
wwf_olive      <- "#7B8327"

headline_svcs <- c("Pollination", "Sed_export", "N_export", "Nature_Access", "C_Risk")

# ------------------------------------------------------------------------------
# 1. Valid (eligible) cells per biome per service, within Colombia
# ------------------------------------------------------------------------------

message("Loading plt_long, filtering to Colombia...")
plt_long <- readRDS(here("data", "processed", "plt_long.rds"))

valid_col <- plt_long %>%
  filter(nev_name == "Colombia", service %in% headline_svcs) %>%
  select(fid, service, WWF_biome) %>%
  distinct() %>%
  filter(!is.na(WWF_biome))

n_total_biome <- valid_col %>%
  count(service, WWF_biome, name = "n_total")

n_total_colombia <- valid_col %>%
  count(service, name = "n_total_col")

# ------------------------------------------------------------------------------
# 2. Hotspot cells per biome per service, within Colombia
# ------------------------------------------------------------------------------

message("Loading Colombia hotspot gpkg...")
hs <- st_read(here("data", "processed", "hotspots", "pct", "nev_name",
                    "hotspots_nev_name_Colombia_pct.gpkg"), quiet = TRUE) %>%
  st_drop_geometry()

hs_long <- hs %>%
  select(grid_fid, WWF_biome, all_of(headline_svcs)) %>%
  pivot_longer(all_of(headline_svcs), names_to = "service", values_to = "is_hot") %>%
  filter(is_hot == 1, !is.na(WWF_biome))

n_hot_biome <- hs_long %>%
  count(service, WWF_biome, name = "n_hot")

n_hot_colombia <- hs_long %>%
  count(service, name = "n_hot_col")

# ------------------------------------------------------------------------------
# 3. Assemble intra-Colombia relative intensity table
# ------------------------------------------------------------------------------

stats <- n_total_biome %>%
  left_join(n_hot_biome, by = c("service", "WWF_biome")) %>%
  mutate(n_hot = tidyr::replace_na(n_hot, 0)) %>%
  left_join(n_total_colombia, by = "service") %>%
  left_join(n_hot_colombia, by = "service") %>%
  mutate(
    n_hot_col = tidyr::replace_na(n_hot_col, 0),
    pct_area_biome = (n_hot / n_total) * 100,
    expected_share_intra = (n_total / n_total_col) * 100,
    pct_share_intra = ifelse(n_hot_col > 0, (n_hot / n_hot_col) * 100, 0),
    relative_intensity_intra = ifelse(expected_share_intra > 0,
                                       pct_share_intra / expected_share_intra, NA)
  ) %>%
  arrange(service, desc(relative_intensity_intra))

out_csv <- file.path(tbl_dir, "colombia_biome_area_stats.csv")
write.csv(stats, out_csv, row.names = FALSE)
message("Saved: ", out_csv)
print(stats %>% filter(service %in% c("Pollination", "Sed_export")) %>%
        select(service, WWF_biome, n_total, n_hot, expected_share_intra, pct_share_intra, relative_intensity_intra))

# ------------------------------------------------------------------------------
# 4. Chart: relative intensity by biome, for the 2 headline services
# ------------------------------------------------------------------------------

biome_short <- c(
  "Tropical & Subtropical Moist Broadleaf Forests" = "Bosque húmedo tropical",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands" = "Sabanas/Llanos",
  "Tropical & Subtropical Dry Broadleaf Forests" = "Bosque seco tropical",
  "Deserts & Xeric Shrublands" = "Desierto/Xérico",
  "Montane Grasslands & Shrublands" = "Páramo/Montano",
  "Mangroves" = "Manglares"
)

plot_df <- stats %>%
  filter(service %in% c("Pollination", "Sed_export"), n_total >= 50) %>%
  mutate(
    biome_es = recode(WWF_biome, !!!biome_short),
    service_es = recode(service, Pollination = "Polinización", Sed_export = "Exportación de sedimentos")
  )

p <- ggplot(plot_df, aes(x = relative_intensity_intra, y = reorder(biome_es, relative_intensity_intra), fill = relative_intensity_intra > 1)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  geom_text(aes(label = sprintf("%.2f×", relative_intensity_intra)), hjust = -0.15, size = 3.3, color = "gray20") +
  scale_fill_manual(values = c("TRUE" = wwf_orange, "FALSE" = wwf_teal), guide = "none") +
  facet_wrap(~ service_es, ncol = 1, scales = "free_y") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "¿Qué bioma concentra los hotspots de cambio dentro de Colombia?",
    subtitle = "Intensidad relativa intra-Colombia: participación del bioma en los hotspots de cambio de Colombia\nvs. su participación en el área elegible de Colombia para ese servicio (biomas con ≥50 celdas elegibles)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13.5, color = wwf_dark_green),
    plot.subtitle = element_text(size = 9.5, color = "gray30", margin = margin(b = 10)),
    strip.text = element_text(face = "bold", size = 11, color = wwf_dark_green),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10)
  )

ggsave(file.path(out_dir, "colombia_biome_relative_intensity.png"), p, width = 9, height = 7, dpi = 200, bg = "white")
message("Saved biome relative-intensity chart.")

# ------------------------------------------------------------------------------
# 5. Reference map: Colombia's biomes (for context)
# ------------------------------------------------------------------------------

message("Building biome reference map...")
grid_col <- st_read(here("data", "processed", "10k_change_calc.gpkg"), quiet = TRUE) %>%
  filter(nev_name == "Colombia", !is.na(WWF_biome)) %>%
  mutate(biome_es = recode(WWF_biome, !!!biome_short))

biome_colors <- c(
  "Bosque húmedo tropical" = wwf_green,
  "Sabanas/Llanos"         = wwf_yellow <- "#F5D200",
  "Bosque seco tropical"   = wwf_olive,
  "Desierto/Xérico"        = "#C9A66B",
  "Páramo/Montano"         = wwf_teal,
  "Manglares"              = wwf_orange
)

p_biome <- ggplot(grid_col) +
  geom_sf(aes(fill = biome_es), color = NA) +
  scale_fill_manual(values = biome_colors, name = "Bioma (WWF)") +
  labs(title = "Colombia — Biomas (WWF)", subtitle = "Referencia geográfica para el análisis de intensidad relativa por bioma") +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = wwf_dark_green),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )
ggsave(file.path(out_dir, "colombia_biome_map.png"), p_biome, width = 9.5, height = 10, dpi = 200, bg = "white")
message("Saved biome reference map.")

message("\nDone.")
