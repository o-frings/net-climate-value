# =============================================================================
# engine/R/_utils.R  —  shared numeric helpers (engine-side, self-contained)
# =============================================================================
# Sourced first by run_engine.R, and explicitly by the standalone sens_*.R scripts.
#
# wmean() replaces four near-duplicate copies (10_schemes, 14_href_sensitivity,
# 11_country as `wm`, sens_additive) that had drifted apart in their guards: one
# omitted the is.na(w) check, one had no empty-set guard at all (so an empty set
# returned 0/0 = NaN), and all four silently renormalised over whatever rows
# survived. Silently dropping a row changes the weighting of a published mean, so
# every rejected input is now an error instead.
# =============================================================================

wmean <- function(x, w, what = "weighted mean") {
  if (length(x) != length(w))
    stop(what, ": x and w differ in length (", length(x), " vs ", length(w), ")")
  if (anyNA(w)) stop(what, ": ", sum(is.na(w)), " NA weight(s)")
  keep <- w > 0
  if (!any(keep)) stop(what, ": no positive weight among ", length(w), " row(s)")
  if (anyNA(x[keep]))
    stop(what, ": ", sum(is.na(x[keep])), " NA value(s) carrying positive weight")
  sum(x[keep] * w[keep]) / sum(w[keep])
}

# --- correlation helpers, shared by the sens_*correlation / c_trend scripts -------
# mean_pair_cor() replaces three copies that had drifted into two different
# behaviours: the two hexagon-based scripts silently dropped zero-variance columns,
# while the country-based one stopped on them. Both are defensible for their own unit
# (a hexagon with no disturbance in a sub-window is real and informative; a
# zero-variance COUNTRY would mean a broken panel), so the choice is now an explicit
# argument rather than an accident of which file you are reading. Drops are counted,
# because silently discarding a unit changes which pairs enter the mean: with the
# MIN_NONZERO filter in place the drop still fires in 2 of 93 hexagon sub-windows.
.corr_env <- new.env(parent = emptyenv())
.corr_env$dropped <- 0L
.corr_env$windows_affected <- 0L

corr_drop_report <- function() {
  list(dropped = .corr_env$dropped, windows_affected = .corr_env$windows_affected)
}
corr_drop_reset <- function() {
  .corr_env$dropped <- 0L; .corr_env$windows_affected <- 0L; invisible(NULL)
}

mean_pair_cor <- function(M, on_zero_variance = c("stop", "drop"),
                          min_units = 3L, min_obs = NULL, unit = "unit") {
  on_zero_variance <- match.arg(on_zero_variance)
  if (!is.null(min_obs) && nrow(M) < min_obs)
    stop("need >= ", min_obs, " observations to correlate; got ", nrow(M))
  sdev <- apply(M, 2, sd)
  if (any(sdev <= 0)) {
    if (on_zero_variance == "stop")
      stop("zero-variance ", unit, " in window: ",
           paste(colnames(M)[sdev <= 0], collapse = ", "))
    .corr_env$dropped <- .corr_env$dropped + sum(sdev <= 0)
    .corr_env$windows_affected <- .corr_env$windows_affected + 1L
    M <- M[, sdev > 0, drop = FALSE]
  }
  if (ncol(M) < min_units) return(NA_real_)
  R <- cor(M)
  mean(R[upper.tri(R)])
}

# Remove each column's own linear time trend, leaving synchronised shocks. Levels
# conflate a shared trend with the shocks a bad pool year consists of.
detrend <- function(M) {
  tt <- seq_len(nrow(M))
  apply(M, 2, function(v) residuals(lm(v ~ tt)))
}

# Per-country hexagon (year x hex) panels from the per-hexagon EFDA table. Hexagons
# with fewer than min_nonzero non-zero years cannot be correlated meaningfully and are
# dropped with the count retained; countries left with fewer than min_units are absent
# from the result. require_common_grid is for callers whose pooled null applies one
# shared year permutation to every country.
hex_panels <- function(H, years, min_nonzero, min_units, require_common_grid = FALSE) {
  mats <- list()
  for (cn in sort(unique(H$country))) {
    d <- H[H$country == cn & H$year %in% years, ]
    M <- tapply(d$lambda_natural, list(as.character(d$year), as.character(d$hex_id)),
                identity)
    if (anyNA(M)) stop("incomplete hexagon panel for ", cn)
    ok <- colSums(M > 0) >= min_nonzero
    Mk <- M[, ok, drop = FALSE]
    if (ncol(Mk) >= min_units) mats[[cn]] <- list(M = Mk, dropped = sum(!ok))
  }
  if (require_common_grid) {
    ny <- unique(vapply(mats, function(z) nrow(z$M), integer(1)))
    if (length(ny) != 1L)
      stop("countries differ in year count; the shared-year permutation needs a ",
           "common year grid")
  }
  mats
}
