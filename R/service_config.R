# Single source of truth for this pipeline's service definitions -- name, raw column
# prefix, and "good direction" (whether an INCREASE is favorable) -- for WHATEVER
# service set is currently being analyzed. This is durable infrastructure, not a
# one-off artifact of the 2026-08 retention/protection redesign: when services get
# added, removed, or redefined in the future, edit the lists below in this one file --
# do not let any qmd/R script re-derive its own copy again. Loaded automatically via
# devtools::load_all() like every other R/*.R file, same as get_hotspots.R -- no
# explicit source() needed in consuming qmd/R scripts (mapping scripts under
# scripts/mapping/ are the exception -- they only source R/paths.R by convention, so
# need an explicit source(here("R", "service_config.R")) too).
#
# WHY THIS EXISTS (2026-09-01): the *current* 5-service list was independently
# copy-pasted into analysis/hotspot_extraction.qmd, analysis/hotspot_synthesis.qmd,
# analysis/KS_tests_hotspots.qmd, and two scripts/mapping/*.R files. Three of those
# had silently drifted to the old export/risk names by the time this was found -- each
# rendered clean while computing on stale data, caught only because output *content*
# was spot-checked, not just exit codes. See docs/pipeline_reference.md (row B7) and
# the memory note in project_pipeline_flexibility.md for the full incident writeup.
# That failure mode -- not "the 5-service redesign" specifically -- is what this file
# exists to prevent, for this and every future service-set change.
#
# A rename or a new service now only needs to change in ONE place. Every consumer
# should build its local CFG list by referencing the objects/functions below, not by
# re-typing service names.

# ---- The 5 real hotspot-defining services (retention/protection amounts) ----------
# good_direction: "high" means an increase is favorable. All 5 are currently "high"
# (the paper's Methods: "framed consistently as an amount of benefit provided") --
# but this is a per-service PROPERTY, not a hardcoded assumption baked into every
# consumer, so a future service with the opposite framing (or a deliberately
# improvement-seeking analysis, see hotspot_direction_lists() below) doesn't require
# re-deriving the loss/gain split by hand again.
SERVICE_AMOUNTS <- list(
  list(name = "N_retention",    col_prefix = "n_retention",    good_direction = "high"),
  list(name = "Sed_retention",  col_prefix = "sed_retention",  good_direction = "high"),
  list(name = "C_Prot_service", col_prefix = "c_prot_service", good_direction = "high"),
  list(name = "Pollination",    col_prefix = "pollination",    good_direction = "high"),
  list(name = "Nature_Access",  col_prefix = "nature_access",  good_direction = "high")
)

# ---- The 3 proportional ratio forms ------------------------------------------------
# NOT part of the hotspot definition (see the paper's Hotspot Identification exclusion
# rationale -- collinearity with the amounts, and ratio-direction ambiguity), but still
# individually KS-tested/plotted in several places, so they need a canonical name too.
SERVICE_RATIOS <- list(
  list(name = "N_Ret_Ratio",      col_prefix = "n_ret_ratio",      good_direction = "high"),
  list(name = "Sed_Ret_Ratio",    col_prefix = "sed_ret_ratio",    good_direction = "high"),
  list(name = "C_Risk_Red_Ratio", col_prefix = "c_risk_red_ratio", good_direction = "high")
)

# ---- Legacy export/risk raw variables ----------------------------------------------
# Used ONLY as computational inputs to the ratio forms above (paper Methods: "not
# reported as variables in their own right anywhere in this paper"). Never hotspot-
# defining, never plotted directly. Kept here only so canonical_lookup() below can
# still resolve raw columns that reference them (e.g. for the ratio formulas, or for
# make_native_change_figure.R's deliberately-separate all-8-service reference figure).
SERVICE_LEGACY_RAW <- list(
  list(name = "N_export",   col_prefix = "n_export"),
  list(name = "Sed_export", col_prefix = "sed_export"),
  list(name = "C_Risk",     col_prefix = "c_risk")
)

# ---- Accessors ----------------------------------------------------------------------

#' Canonical names of the 5 real hotspot-defining services, in the established
#' water -> access order (N_retention, Sed_retention, then Pollination, Nature_Access,
#' then C_Prot_service last -- matches hotspot_extraction.qmd's HOTS_CFG$loss ordering).
service_names <- function(services = SERVICE_AMOUNTS) {
  vapply(services, `[[`, character(1), "name")
}

#' Canonical names of the 3 ratio forms, same pattern as service_names().
ratio_names <- function(ratios = SERVICE_RATIOS) {
  vapply(ratios, `[[`, character(1), "name")
}

#' Direction-aware loss/gain split for extract_hotspots()'s loss_services/gain_services
#' arguments (R/get_hotspots.R). `looking_for = "decline"` (default) flags the WORST 5%
#' of each service as a hotspot; `looking_for = "improvement"` flags the BEST 5%
#' instead. Preserves the direction flexibility requested 2026-09-01 -- "decline" is
#' the paper's current analysis choice, not a structural constraint of the pipeline.
hotspot_direction_lists <- function(services = SERVICE_AMOUNTS, looking_for = c("decline", "improvement")) {
  looking_for <- match.arg(looking_for)
  nm_high <- vapply(Filter(function(s) s$good_direction == "high", services), `[[`, character(1), "name")
  nm_low  <- vapply(Filter(function(s) s$good_direction == "low",  services), `[[`, character(1), "name")
  if (looking_for == "decline") {
    list(loss_services = nm_high, gain_services = nm_low)
  } else {
    list(loss_services = nm_low, gain_services = nm_high)
  }
}

#' Raw sqlite column-prefix -> canonical display-name lookup, for the
#' `tolower(service) |> recode(!!!canonical_lookup)` pattern used throughout the
#' pipeline wherever a wide 10k_change_calc.gpkg table gets pivoted to long format.
#' Named service_canonical_lookup() (not canonical_lookup()) deliberately -- every
#' consuming qmd already has a local *variable* called `canonical_lookup` (a plain
#' named vector, used via `!!!canonical_lookup`); naming this function identically
#' would collide once devtools::load_all() makes it visible. Consumers should do
#' `canonical_lookup <- service_canonical_lookup()` once, then use the existing
#' `!!!canonical_lookup` calls unchanged.
service_canonical_lookup <- function() {
  all_services <- c(SERVICE_AMOUNTS, SERVICE_RATIOS, SERVICE_LEGACY_RAW)
  base <- setNames(
    vapply(all_services, `[[`, character(1), "name"),
    vapply(all_services, `[[`, character(1), "col_prefix")
  )
  # Ancillary raw-column aliases that don't fit the service/ratio/legacy split above --
  # mostly historical coastal-protection column names from before C_Prot_service existed.
  ancillary <- c(
    usle                             = "USLE",
    coastal_protection_rt            = "C_Risk",
    coastal_protection_rt_nohab_all  = "C_Risk_NoHab",
    coastal_protection_rt_ratio      = "C_Risk_Red_Ratio",
    rt_ratio                         = "C_Risk_Red_Ratio",
    rt                                = "C_Risk",
    rt_nohab                         = "Rt_nohab",
    c_prot_service                   = "C_Prot_service"
  )
  c(base, ancillary)
}
