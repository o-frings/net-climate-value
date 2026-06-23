# =============================================================================
# figdata/ed_sensitivity.R — plot-ready tables for ED Fig 1 (PRCC sensitivity)
# =============================================================================
# Two panels of 19_fig_sensitivity_heatmap.R: (cells) per practice x parameter
# mean PRCC heatmap; (marg) marginal mean|PRCC| bar. All numeric prep lives here;
# the figure script only reads these tables and draws.
#   fd_ed_sensitivity_cells — one row per practice x parameter (heatmap tiles)
#   fd_ed_sensitivity_marg  — one row per parameter (marginal bar)
# Both carry integer 'ord' columns for factor ordering and pre-built labels; the
# cells table also carries the shared 'lim' (symmetric fill limit).
# =============================================================================
local({
  pr <- eng("mc_prcc.csv"); pr <- pr[as.logical(pr$is_anchor) & is.finite(pr$prcc), ]
  agg <- aggregate(prcc ~ practice + parameter, pr, mean)   # one cell per practice x param

  labs_p <- c(k0 = "Net discount rate (r–g)", kappa = "Leakage intensity (κ)",
              g = "SCC growth rate (g)", r = "Discount rate (r)",
              U_mult = "Uncertainty multiplier (U)", lambda_mult = "Disturbance rate (λ)",
              eps_mult = "Elasticity multiplier (ε)")
  # parameter order: ascending mean|PRCC| so the strongest driver sits at the top
  marg <- aggregate(abs(prcc) ~ parameter, agg, mean); names(marg)[2] <- "mabs"
  marg$param_label <- labs_p[marg$parameter]
  marg$param_ord   <- order(order(marg$mabs))            # integer rank, ascending mabs

  # carry per-parameter label + order onto the cell table
  agg$param_label <- labs_p[agg$parameter]
  agg <- merge(agg, marg[, c("parameter", "param_ord")], by = "parameter")

  # practice order: ascending mean NCV (lowest-value practices at left)
  ms <- eng("mc_summary.csv"); ms <- ms[as.logical(ms$is_anchor), ]
  ord <- aggregate(mean_share ~ practice, ms, mean)
  ord$practice_ord <- order(order(ord$mean_share))        # integer rank, ascending mean_share
  agg <- merge(agg, ord[, c("practice", "practice_ord")], by = "practice")

  # symmetric fill limit shared by the heatmap
  agg$lim <- ceiling(max(abs(agg$prcc)) * 10) / 10
  # precomputed colour threshold (white text on strong cells) so the plot does no
  # arithmetic. The "%.2f" cell/bar labels are applied as display formatting in the
  # plot from prcc/mabs directly: a precomputed string label cannot survive read.csv,
  # which coerces an all-numeric "0.70" column back to 0.7 and drops trailing zeros.
  agg$label_white <- abs(agg$prcc) > 0.35

  agg <- agg[order(agg$practice_ord, agg$param_ord), ]
  marg <- marg[order(marg$param_ord), ]

  wfd(agg,  "fd_ed_sensitivity_cells")
  wfd(marg, "fd_ed_sensitivity_marg")
})
