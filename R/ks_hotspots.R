#' KS: hotspots vs non-hotspots per service × variable (with direction + summaries)
#'
#' @description
#' For each `service × var`, compare hotspot vs non-hotspot distributions using
#' the two-sample Kolmogorov–Smirnov test. Returns a clean table with:
#' KS D, p-values (two-sided and optional one-sided, plus optional permuted p),
#' FDR-adjusted p (BH), directional effect sizes (AUC, Cliff's δ),
#' and group summaries (means/medians/quantiles, with deltas).
#'
#' @param hotspots_df Long-format table for hotspots (must include `service` and tested `vars`).
#' @param inverse_df  Long-format table for non-hotspots (same `vars` present).
#' @param vars Character vector of variable names to test. If `NULL`, auto-detects
#'   numeric columns common to both inputs, excluding typical meta cols.
#' @param include_services Optional character vector to keep only these services.
#' @param exclude_services Optional character vector of services to drop.
#' @param adjust_method Multiple-testing correction for p-values (default `"BH"`).
#' @param permute_n Integer. If `>0`, compute permutation p by shuffling group labels
#'   within service `permute_n` times and recomputing KS D.
#' @param seed Random seed (sampling/permutations).
#' @param sampling Strategy to control non-hotspot size per service:
#'   `"none"` (use all), `"match_hot"` (sample nonhot to hotspot N),
#'   `"cap_nonhot"` (cap nonhot at `nonhot_cap`).
#' @param nonhot_cap Integer cap when `sampling="cap_nonhot"`.
#' @param min_n Minimum per-group n to run tests (default 2).
#' @param compute_one_sided If TRUE, also add one-sided KS p-values (greater/less) + BH-adjusted.
#' @param add_summaries If TRUE, compute group summaries (means/medians/10th/90th) + deltas.
#' @param out_csv Optional path to write results CSV. If `NULL`, skip writing.
#'
#' @return Tibble with one row per `service × var`:
#' \itemize{
#'   \item \code{service}, \code{var}
#'   \item \code{D}, \code{p_value}, \code{p_perm} (if requested), \code{p_adj}
#'   \item \code{n_hot}, \code{n_non}
#'   \item \code{auc}, \code{cliffs_delta}
#'   \item (if summaries) \code{mean_*}, \code{median_*}, \code{q10_*}, \code{q90_*}, \code{*_delta}
#'   \item (if one-sided) \code{p_hot_greater}, \code{p_hot_less}, \code{p_hot_greater_adj}, \code{p_hot_less_adj}
#' }
#'
#' @section Notes:
#' - KS detects **any** distributional difference (location/scale/shape). AUC (≡ MW-U / probability of superiority)
#'   and Cliff’s δ add **direction** and interpretable effect size (−1..+1).
#' - With very large, tied data, KS asymptotic p-values are approximate; permutation p helps.
#' - Hotspots are **tail-selected** on `% change`; interpret results as associations, not causality.
#'
#' @examples
#' \dontrun{
#' ks_res <- run_ks_hot_vs_non(
#'   hotspots_df = res$hotspots_df,
#'   inverse_df  = res$non_hotspots_df,
#'   vars = c("rast_gdpTot_1990_2020_30arcsec_2020_sum",
#'            "GHS_POP_E2020_GLOBE_sum",
#'            "GlobPOP_Count_30arc_2020_sum"),
#'   sampling = "match_hot",   # helpful when nonhot >> hot
#'   permute_n = 0,            # set to e.g. 500 for perm p
#'   compute_one_sided = TRUE,
#'   out_csv = "output_charts/ks_results_hot_vs_non.csv"
#' )
#' }
#'
#' @importFrom dplyr bind_rows mutate filter select group_by ungroup summarise arrange across left_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom readr write_csv
#' @export
run_ks_hot_vs_non <- function(
    hotspots_df, inverse_df,
    vars = NULL,
    include_services = NULL,
    exclude_services = NULL,
    adjust_method = "BH",
    permute_n = 0,
    seed = 42,
    sampling = c("none","match_hot","cap_nonhot"),
    nonhot_cap = 100000L,
    min_n = 2L,
    compute_one_sided = TRUE,
    add_summaries = TRUE,
    out_csv = "output_charts/ks_results_hot_vs_non.csv"
) {
  sampling <- match.arg(sampling)
  set.seed(seed)
  
  # Auto-detect vars if not supplied
  if (is.null(vars)) {
    meta <- c("fid","c_fid","service","pct_chg","hotspot_binary","hotspot_flag",
              "n_total","n_cut","rank_high","rank_low","flag_high","flag_low")
    num_hot <- names(hotspots_df)[vapply(hotspots_df, is.numeric, logical(1))]
    num_non <- names(inverse_df )[vapply(inverse_df,  is.numeric, logical(1))]
    vars <- setdiff(intersect(num_hot, num_non), meta)
  }
  if (!length(vars)) stop("No test variables found. Provide `vars` or check inputs.")
  
  # Combine & filter services
  df <- dplyr::bind_rows(
    dplyr::mutate(hotspots_df, group = "hotspot"),
    dplyr::mutate(inverse_df,  group = "nonhotspot")
  )
  if (!is.null(include_services)) df <- dplyr::filter(df, .data$service %in% include_services)
  if (!is.null(exclude_services)) df <- dplyr::filter(df, !(.data$service %in% exclude_services))
  
  # Optional downsampling of non-hotspots per service
  if (sampling != "none") {
    df <- df %>%
      dplyr::group_by(.data$service) %>%
      dplyr::group_modify(function(d, ...) {
        dh <- d[d$group == "hotspot", , drop = FALSE]
        dn <- d[d$group == "nonhotspot", , drop = FALSE]
        if (!nrow(dn)) return(d)
        if (sampling == "match_hot") {
          k <- nrow(dh)
          if (k > 0 && nrow(dn) > k) dn <- dn[sample.int(nrow(dn), k), , drop = FALSE]
        } else if (sampling == "cap_nonhot") {
          cap <- as.integer(nonhot_cap)
          if (nrow(dn) > cap) dn <- dn[sample.int(nrow(dn), cap), , drop = FALSE]
        }
        dplyr::bind_rows(dh, dn)
      }) %>% dplyr::ungroup()
  }
  
  # Long format: one row per observation × var
  long <- df %>%
    tidyr::pivot_longer(cols = dplyr::all_of(vars), names_to = "var", values_to = "val") %>%
    dplyr::filter(is.finite(.data$val))
  
  # Helpers ----------------------------------------------------------------
  
  ks_core <- function(x, y, permute_n = 0L) {
    x <- as.numeric(x); y <- as.numeric(y)
    x <- x[is.finite(x)]; y <- y[is.finite(y)]
    n_hot <- length(x); n_non <- length(y)
    if (n_hot < min_n || n_non < min_n) {
      return(list(D = NA_real_, p = NA_real_, p_perm = NA_real_, n_hot = n_hot, n_non = n_non))
    }
    kt <- suppressWarnings(stats::ks.test(x, y, alternative = "two.sided", exact = FALSE))
    D_obs <- as.numeric(kt$statistic); p_asym <- kt$p.value
    
    p_perm <- NA_real_
    if (permute_n > 0L) {
      z <- c(x, y)
      g <- c(rep(1L, n_hot), rep(0L, n_non))
      Dp <- numeric(permute_n)
      for (i in seq_len(permute_n)) {
        g_sh <- sample(g)
        Dp[i] <- suppressWarnings(as.numeric(
          stats::ks.test(z[g_sh==1L], z[g_sh==0L], exact = FALSE)$statistic
        ))
      }
      p_perm <- mean(Dp >= D_obs)
    }
    list(D = D_obs, p = p_asym, p_perm = p_perm, n_hot = n_hot, n_non = n_non)
  }
  
  # AUC (probability of superiority) with double arithmetic to avoid overflow
  auc_ps <- function(x, y) {
    nx <- length(x); ny <- length(y)
    if (nx < min_n || ny < min_n) return(NA_real_)
    r <- rank(c(x, y), ties.method = "average")
    nx_d <- as.numeric(nx); ny_d <- as.numeric(ny)
    Wsum <- sum(r[seq_len(nx)])                 # sum of ranks of hotspots
    U    <- Wsum - nx_d * (nx_d + 1) / 2        # Mann–Whitney U
    as.numeric(U / (nx_d * ny_d))               # probability of superiority
  }
  
  # One-sided p-values (optional)
  ks_onesided <- function(x, y, side = c("greater","less")) {
    side <- match.arg(side)
    suppressWarnings(stats::ks.test(x, y, alternative = side, exact = FALSE)$p.value)
  }
  
  # Compute per (service × var) -------------------------------------------
  
  results <- long %>%
    dplyr::group_by(.data$service, .data$var) %>%
    dplyr::summarise(
      {
        x <- .data$val[.data$group=="hotspot"]; y <- .data$val[.data$group=="nonhotspot"]
        core <- ks_core(x, y, permute_n = permute_n)
        
        # summaries / effect sizes
        mean_hot   = if (core$n_hot >= min_n) mean(x) else NA_real_
        mean_non   = if (core$n_non >= min_n) mean(y) else NA_real_
        median_hot = if (core$n_hot >= min_n) stats::median(x) else NA_real_
        median_non = if (core$n_non >= min_n) stats::median(y) else NA_real_
        q10_hot    = if (core$n_hot >= min_n) as.numeric(stats::quantile(x, 0.10, names=FALSE)) else NA_real_
        q90_hot    = if (core$n_hot >= min_n) as.numeric(stats::quantile(x, 0.90, names=FALSE)) else NA_real_
        q10_non    = if (core$n_non >= min_n) as.numeric(stats::quantile(y, 0.10, names=FALSE)) else NA_real_
        q90_non    = if (core$n_non >= min_n) as.numeric(stats::quantile(y, 0.90, names=FALSE)) else NA_real_
        auc        = auc_ps(x, y)
        cliffs_delta = ifelse(is.na(auc), NA_real_, 2*auc - 1)
        
        # one-sided p-values if requested
        p_hot_greater = if (compute_one_sided && core$n_hot >= min_n && core$n_non >= min_n)
          ks_onesided(x, y, "greater") else NA_real_
        p_hot_less    = if (compute_one_sided && core$n_hot >= min_n && core$n_non >= min_n)
          ks_onesided(x, y, "less") else NA_real_
        
        # pack output
        tibble::tibble(
          D = core$D, p_value = core$p, p_perm = core$p_perm,
          n_hot = core$n_hot, n_non = core$n_non,
          mean_hot, mean_non, median_hot, median_non,
          q10_hot, q90_hot, q10_non, q90_non,
          median_delta = median_hot - median_non,
          mean_delta   = mean_hot   - mean_non,
          auc, cliffs_delta,
          p_hot_greater, p_hot_less
        )
      },
      .groups = "drop"
    ) %>%
    # adjust p-values within each var across services
    dplyr::group_by(.data$var) %>%
    dplyr::mutate(
      p_adj = stats::p.adjust(.data$p_value, method = adjust_method),
      p_hot_greater_adj = if (compute_one_sided) stats::p.adjust(.data$p_hot_greater, method = adjust_method) else NA_real_,
      p_hot_less_adj    = if (compute_one_sided) stats::p.adjust(.data$p_hot_less,    method = adjust_method) else NA_real_
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$var, dplyr::desc(.data$D))
  
  # Optionally drop summaries if not requested
  if (!isTRUE(add_summaries)) {
    results <- dplyr::select(
      results, .data$service, .data$var,
      .data$D, .data$p_value, .data$p_perm, .data$p_adj,
      .data$n_hot, .data$n_non,
      .data$auc, .data$cliffs_delta,
      dplyr::any_of(c("p_hot_greater","p_hot_less","p_hot_greater_adj","p_hot_less_adj"))
    )
  }
  
  # Write CSV if requested
  if (!is.null(out_csv)) {
    dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(results, out_csv)
  }
  
  results
}
