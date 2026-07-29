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
