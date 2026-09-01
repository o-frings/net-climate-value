# =============================================================================
# figures/ed_pool_buildup.R — pool buffer diversification vs pool size
#   (PURE PLOT — all data prepared in engine/R/figdata/ed_pool_buildup.R)
# =============================================================================
# Single-panel Extended Data figure: how the pooled-buffer / per-project ratio
# decays toward the fully-diversified asymptote as more countries enter the
# pool. Two enrolment orderings (random vs largest-forest-nations-first) bracket
# the realistic ramp. Aesthetic is the validated legacy design; data are the
# engine's pool_buildup sweep. Run via run_figures.R (sources _setup_legacy.R).
# =============================================================================

local({
  # ─── Engine-computed tables: main series, scalars/label, x-axis breaks ───
  d      <- fd("fd_ed_pool_buildup_main")
  meta   <- fd("fd_ed_pool_buildup_meta")
  breaks <- fd("fd_ed_pool_buildup_breaks")

  d <- d[order(d$ord), ]
  d$ordering <- factor(d$ordering, levels = unique(d$ordering[order(d$ord)]))

  fig_buildup <- ggplot(d, aes(K, div_ratio, colour = ordering)) +
    geom_hline(yintercept = meta$asym, linetype = "dashed",
               colour = NATURE_GREY, linewidth = 0.4) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 0.7) +
    annotate("text", x = 1.5, y = 1.0, label = "1 country = per-project (panel a)",
             size = 1.9, colour = NATURE_GREY, hjust = 0, fontface = "italic") +
    annotate("text", x = meta$nC, y = meta$asym_y,
             label = meta$asym_label,
             size = 1.9, colour = NATURE_GREY, hjust = 1, fontface = "italic") +
    scale_colour_manual(values = c("Random enrolment" = "#2c7fb8",
                                   "Largest forest nations first" = "#d95f02"),
                        name = NULL) +
    scale_x_continuous(breaks = breaks$x_break) +
    scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
    labs(x = "Countries represented in the pool",
         y = "Pool buffer /\nper-project") +
    theme_nature(base_size = 9) +
    theme(legend.position = "top")

  save_figure(fig_buildup, "ed_fig_pool_buildup", width = 130, height = 95)
})
