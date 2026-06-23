# =============================================================================
# figures/ed_sensitivity.R — ED Fig 1: PRCC sensitivity heatmap + marginal bar
# =============================================================================
# Faithful two-panel legacy design (19_fig_sensitivity_heatmap.R): a = per-cell
# PRCC heatmap (practices on top x-axis, parameters on y), b = marginal mean|PRCC|
# bar. PURE PLOT: reads engine tables fd_ed_sensitivity_cells / _marg (computed in
# engine/R/figdata/ed_sensitivity.R) and draws. Run via run_figures.R (sources
# _setup_legacy.R first). All ordering uses the integer ord columns the engine emits.
# =============================================================================

local({
  agg  <- fd("fd_ed_sensitivity_cells")
  marg <- fd("fd_ed_sensitivity_marg")

  # parameter order: ascending mean|PRCC| (engine param_ord); strongest at top
  param_levels <- marg$param_label[order(marg$param_ord)]
  marg$param_label <- factor(marg$param_label, levels = param_levels)
  agg$param_label  <- factor(agg$param_label,  levels = param_levels)

  # practice order: ascending mean NCV (engine practice_ord); lowest-value at left
  practice_levels <- unique(agg$practice[order(agg$practice_ord)])
  agg$practice <- factor(agg$practice, levels = practice_levels)

  lim <- agg$lim[1]

  pa <- ggplot(agg, aes(practice, param_label, fill = prcc)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", prcc), colour = label_white),
              size = 2.0, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#333333")) +
    scale_fill_gradient2(low = NATURE_RED, mid = "white", high = NATURE_BLUE,
                         midpoint = 0, limits = c(-lim, lim), name = "PRCC",
                         breaks = c(-0.5, 0, 0.5)) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_nature(base_size = 8) +
    theme(axis.text.x = element_text(angle = 40, hjust = 0, size = 6.5),
          axis.text.y = element_text(size = 7), panel.grid = element_blank(),
          legend.position = "bottom",
          legend.key.height = unit(8, "pt"), legend.key.width = unit(18, "pt"))

  pb <- ggplot(marg, aes(mabs, param_label)) +
    geom_col(fill = NATURE_GREY, width = 0.55) +
    geom_text(aes(label = sprintf("%.2f", mabs)), hjust = -0.15, size = 2.4,
              colour = NATURE_GREY) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25)), limits = c(0, NA)) +
    labs(x = "Mean |PRCC|", y = NULL) +
    theme_nature(base_size = 8) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank())

  fig <- pa + pb + plot_layout(widths = c(4, 1)) +
    plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(size = 10, face = "bold"))
  save_figure(fig, "ed_fig1_sensitivity_heatmap", width = 240, height = 120)
})
