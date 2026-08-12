# ==============================================================================
# Colombia Statistical Panel — percent-vs-percent framing (not bare multiplier)
# ==============================================================================
#
# Per the standing preference (project_relative_intensity_reframing memory):
# state disproportionality as "X% of global hotspots concentrated in Y% of
# eligible land," not as a bare ratio like "1.83x". Uses the CORRECTED
# hotspot_area_stats_Colombia.csv (post Coastal Risk denominator fix,
# WORKLOG.md 2026-08-11).
#
# Split into "focus" (Pollination, Sed_export -- the 2 disproportionate
# services) and "others" (N_export, Nature_Access, C_Risk) for the report's
# tabbed chart widget. Labels in Spanish throughout -- the previous version
# baked English text into the PNGs despite the report being Spanish.
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
  Pollination   = "Polinización",
  Sed_export    = "Exportación de sedimentos",
  N_export      = "Exportación de nitrógeno",
  Nature_Access = "Acceso a la naturaleza",
  C_Risk        = "Riesgo costero"
)

focus_svcs  <- c("Pollination", "Sed_export")
other_svcs  <- c("N_export", "Nature_Access", "C_Risk")

make_share_chart <- function(svcs, out_file, subtitle_extra) {
  plot_df <- stats %>%
    filter(service %in% svcs) %>%
    mutate(service_label = factor(svc_labels[service], levels = rev(svc_labels[svcs]))) %>%
    select(service_label, pct_share, expected_share) %>%
    pivot_longer(c(pct_share, expected_share), names_to = "metric", values_to = "value") %>%
    mutate(metric_label = ifelse(metric == "pct_share",
                                  "% de hotspots globales en Colombia",
                                  "% de tierra elegible global que es Colombia"))

  p <- ggplot(plot_df, aes(y = service_label, x = value, fill = metric_label)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    geom_text(aes(label = sprintf("%.2f%%", value)),
              position = position_dodge(width = 0.75), hjust = -0.15, size = 3.6, color = "gray20") +
    scale_fill_manual(values = c(
      "% de hotspots globales en Colombia" = wwf_orange,
      "% de tierra elegible global que es Colombia" = "gray70"
    ), name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(
      title = "Participación de Colombia en hotspots vs. tierra elegible",
      subtitle = paste(strwrap(paste0("Barras de longitud similar = proporcional; naranja más larga = sobrerrepresentación. ", subtitle_extra), width = 85), collapse = "\n"),
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

  ggsave(file.path(out_dir, out_file), p, width = 10, height = max(3.5, length(svcs) * 1.6), dpi = 200, bg = "white")
  message("Saved: ", file.path(out_dir, out_file))
}

make_coverage_chart <- function(svcs, out_file, subtitle_extra) {
  cov_df <- stats %>%
    filter(service %in% svcs) %>%
    mutate(service_label = factor(svc_labels[service], levels = rev(svc_labels[svcs])))

  p_cov <- ggplot(cov_df, aes(y = service_label, x = pct_area)) +
    geom_col(fill = wwf_teal, width = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%", pct_area)), hjust = -0.15, size = 3.8, color = "gray20") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      title = "Cobertura de hotspots dentro de Colombia",
      subtitle = paste(strwrap(paste0("% del área elegible propia de Colombia (por servicio) en declive extremo, 1992–2020. ", subtitle_extra), width = 85), collapse = "\n"),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = wwf_dark_green),
      plot.subtitle = element_text(size = 10.5, color = "gray30", margin = margin(b = 10)),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 12, face = "bold")
    )

  ggsave(file.path(out_dir, out_file), p_cov, width = 9, height = max(3, length(svcs) * 1.4), dpi = 200, bg = "white")
  message("Saved: ", file.path(out_dir, out_file))
}

make_share_chart(focus_svcs, "colombia_share_vs_expected_focus.png",
                  "Los dos servicios con sobrerrepresentación real.")
make_share_chart(other_svcs, "colombia_share_vs_expected_others.png",
                  "Nitrógeno es proporcional; acceso y riesgo costero están subrepresentados.")

make_coverage_chart(focus_svcs, "colombia_coverage_focus.png", "")
make_coverage_chart(other_svcs, "colombia_coverage_others.png", "")

message("\nDone.")
