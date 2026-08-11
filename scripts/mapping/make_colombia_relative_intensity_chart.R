# ==============================================================================
# Colombia Statistical Panel — percent-vs-percent framing (not bare multiplier)
# ==============================================================================
#
# Per the standing preference (project_relative_intensity_reframing memory):
# state disproportionality as "X% of global hotspots concentrated in Y% of
# eligible land," not as a bare ratio like "1.83x". Uses the CORRECTED
# hotspot_area_stats_Colombia.csv (post Coastal Risk denominator fix,
# WORKLOG.md 2026-08-11).
# ==============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(here)

source(here("R", "paths.R"))

out_dir <- here("outputs", "plots", "colombia_report")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

wwf_green      <- "#007930"
wwf_dark_green <- "#004D1E"
wwf_orange     <- "#F07D00"
wwf_teal       <- "#009191"

stats <- read.csv(here("data", "processed", "tables", "regional_subsets", "nev_name",
                        "hotspot_area_stats_Colombia.csv"), stringsAsFactors = FALSE)

svc_labels <- c(
  Pollination   = "Pollination",
  Sed_export    = "Sediment export",
  N_export      = "Nitrogen export",
  Nature_Access = "Nature access",
  C_Risk        = "Coastal risk"
)

svc_order <- c("Pollination", "Sed_export", "N_export", "Nature_Access", "C_Risk")

plot_df <- stats %>%
  filter(service %in% svc_order) %>%
  mutate(service_label = factor(svc_labels[service], levels = rev(svc_labels[svc_order]))) %>%
  select(service_label, pct_share, expected_share, relative_intensity) %>%
  pivot_longer(c(pct_share, expected_share), names_to = "metric", values_to = "value") %>%
  mutate(metric_label = ifelse(metric == "pct_share",
                                "% of global hotspots located in Colombia",
                                "% of eligible global land that is Colombia"))

p <- ggplot(plot_df, aes(y = service_label, x = value, fill = metric_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = sprintf("%.2f%%", value)),
            position = position_dodge(width = 0.75), hjust = -0.15, size = 3.4, color = "gray20") +
  scale_fill_manual(values = c(
    "% of global hotspots located in Colombia" = wwf_orange,
    "% of eligible global land that is Colombia" = "gray70"
  ), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "Colombia's share of global hotspots vs. its share of eligible land",
    subtitle = "Bars close in length = proportionate. Orange bar longer = Colombia over-represented among global hotspots for that service.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = wwf_dark_green),
    plot.subtitle = element_text(size = 10, color = "gray30", margin = margin(b = 10)),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold")
  )

ggsave(file.path(out_dir, "colombia_share_vs_expected.png"), p, width = 10, height = 6, dpi = 200, bg = "white")
message("Saved: ", file.path(out_dir, "colombia_share_vs_expected.png"))

# ------------------------------------------------------------------------------
# Coverage (pct_area) panel: "of Colombia's OWN eligible land, what % is a hotspot"
# ------------------------------------------------------------------------------

cov_df <- stats %>%
  filter(service %in% svc_order) %>%
  mutate(service_label = factor(svc_labels[service], levels = rev(svc_labels[svc_order])))

p_cov <- ggplot(cov_df, aes(y = service_label, x = pct_area)) +
  geom_col(fill = wwf_teal, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct_area)), hjust = -0.15, size = 3.6, color = "gray20") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Hotspot coverage within Colombia",
    subtitle = "% of Colombia's own eligible land (per service) in extreme decline, 1992–2020",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = wwf_dark_green),
    plot.subtitle = element_text(size = 10, color = "gray30", margin = margin(b = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold")
  )

ggsave(file.path(out_dir, "colombia_coverage.png"), p_cov, width = 9, height = 5.5, dpi = 200, bg = "white")
message("Saved: ", file.path(out_dir, "colombia_coverage.png"))
