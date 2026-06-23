# =============================================================================
# figures/fig3.R — headline 4-panel figure (faithful port of legacy 17), PURE PLOT
# =============================================================================
# a: butterfly decomposition (buffer·time·leakage left, net issuance right) for
#    scheme-covered practices; b: net-issuance MC boxplots by supply class;
#    c: integrity-gap MC boxplots by scheme; d: parameter tornado. Aesthetic is
#    the validated legacy design; data are engine MC outputs (fd_fig3_*). All
#    numeric preparation lives in engine/R/figdata/fig3.R — this script only
#    reads the plot-ready tables and draws. Run via run_figures.R (which sources
#    _setup_legacy.R first).
# =============================================================================

local({
  # ─── Shared butterfly-decomposition axis title ───
  butterfly_axis_title <- paste0(
    "← <b style='color:#8C8C8C'>Buffer</b>", " · ",
    "<b style='color:#ABABAB'>Time</b>",
    " <span style='color:#ABABAB; font-size:7pt'>(τ = 10–100 yr)</span>", " · ",
    "<b style='color:#606060'>Leakage</b>",
    "  <span style='color:#BBBBBB'>|</span>  ", "Net issuance →")

  # ─── Panel a: butterfly decomposition ───
  ded_long_v2 <- fd("fd_fig3_a_ded")
  net_bar_v2  <- fd("fd_fig3_a_net")
  # restore factor orderings emitted by the engine fragment (ord ascending = level order)
  bar_levels <- net_bar_v2$bar_label[order(net_bar_v2$ord)]
  ded_long_v2$bar_label <- factor(ded_long_v2$bar_label, levels = bar_levels)
  net_bar_v2$bar_label  <- factor(net_bar_v2$bar_label,  levels = bar_levels)
  ded_long_v2$component <- factor(ded_long_v2$component, levels = c("Buffer", "Time", "Leakage"))
  ded_cols_v2 <- c(Leakage = "#808080", Time = "#C8C8C8", Buffer = "#A8A8A8")

  fig_a <- ggplot() +
    geom_col(data = ded_long_v2, aes(x = bar_label, y = -share, fill = component),
             position = position_stack(), width = 0.65, colour = NA) +
    scale_fill_manual(values = ded_cols_v2, name = NULL, guide = "none") +
    ggnewscale::new_scale_fill() +
    geom_col(data = net_bar_v2, aes(x = bar_label, y = net_share, fill = net_share),
             width = 0.65, colour = NA) +
    scale_fill_gradient2(low = "#C0392B", mid = "#E8D5C4", high = "#BDD7EE",
                         midpoint = 0.30, guide = "none") +
    geom_text(data = net_bar_v2, aes(x = bar_label, y = label_y, label = label),
              size = 2.4, colour = "white", fontface = "bold") +
    geom_hline(yintercept = 0, colour = "#CCCCCC", linewidth = 0.3) +
    scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
      labels = c("75%", "50%", "25%", "0%", "25%", "50%", "75%"),
      expand = expansion(mult = c(0.02, 0.02))) +
    coord_flip() +
    labs(x = NULL, y = butterfly_axis_title) +
    theme_nature(base_size = 8) +
    theme(panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#F0F0F0", linewidth = 0.25),
      axis.title.x = ggtext::element_markdown(size = 7.2, colour = NATURE_GREY, margin = margin(t = 8)),
      legend.position = "none", plot.margin = margin(6, 4, 6, 4),
      axis.text.y = element_text(size = 6.5))

  # ─── Panel b: net-issuance MC boxplots by supply class ───
  net_summ <- fd("fd_fig3_b")
  net_summ$ptype <- factor(net_summ$ptype, levels = net_summ$ptype[order(net_summ$ord)])
  rh_b <- 0.28
  fig_b <- ggplot(net_summ, aes(y = ptype)) +
    geom_segment(aes(x = p5_net, xend = p95_net, y = ptype, yend = ptype), colour = "#999999", linewidth = 0.35) +
    geom_segment(aes(x = p5_net, xend = p5_net, y = y_num - rh_b * 0.5, yend = y_num + rh_b * 0.5), colour = "#999999", linewidth = 0.35) +
    geom_segment(aes(x = p95_net, xend = p95_net, y = y_num - rh_b * 0.5, yend = y_num + rh_b * 0.5), colour = "#999999", linewidth = 0.35) +
    geom_rect(aes(xmin = p25_net, xmax = p75_net, ymin = y_num - rh_b, ymax = y_num + rh_b, fill = mean_net), alpha = 0.85, colour = NA) +
    scale_fill_gradient2(low = "#C0392B", mid = "#E8D5C4", high = "#BDD7EE", midpoint = 0, guide = "none") +
    geom_segment(aes(x = median_net, xend = median_net, y = y_num - rh_b, yend = y_num + rh_b), colour = "white", linewidth = 0.6) +
    geom_point(aes(x = mean_net), shape = 21, size = 2, fill = "#333333", colour = "white", stroke = 0.5) +
    geom_text(aes(x = label_x, label = label),
              size = 2.2, hjust = 0, colour = NATURE_GREY) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    coord_cartesian(clip = "off") + labs(x = "Net issuance", y = NULL) +
    theme_nature(base_size = 8) +
    theme(panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#F0F0F0", linewidth = 0.25),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      legend.position = "none", plot.margin = margin(8, 24, 4, 4))

  # ─── Panel c: integrity-gap MC boxplots by scheme ───
  forest_data <- fd("fd_fig3_c")
  forest_data$scheme_name <- factor(forest_data$scheme_name,
                                    levels = forest_data$scheme_name[order(forest_data$ord)])
  rh_c <- 0.28
  fig_c <- ggplot(forest_data, aes(y = scheme_name)) +
    geom_vline(xintercept = 0, colour = "#BBBBBB", linewidth = 0.35) +
    geom_segment(aes(x = p5_gap, xend = p95_gap, y = scheme_name, yend = scheme_name), colour = "#999999", linewidth = 0.35) +
    geom_segment(aes(x = p5_gap, xend = p5_gap, y = y_num - rh_c * 0.5, yend = y_num + rh_c * 0.5), colour = "#999999", linewidth = 0.35) +
    geom_segment(aes(x = p95_gap, xend = p95_gap, y = y_num - rh_c * 0.5, yend = y_num + rh_c * 0.5), colour = "#999999", linewidth = 0.35) +
    geom_rect(aes(xmin = p25_gap, xmax = p75_gap, ymin = y_num - rh_c, ymax = y_num + rh_c, fill = mean_gap), alpha = 0.85, colour = NA) +
    scale_fill_gradient2(low = "#BDD7EE", mid = "#E8D5C4", high = "#C0392B", midpoint = 0, guide = "none") +
    geom_segment(aes(x = median_gap, xend = median_gap, y = y_num - rh_c, yend = y_num + rh_c), colour = "white", linewidth = 0.6) +
    geom_point(aes(x = mean_gap), shape = 21, size = 2, fill = "#333333", colour = "white", stroke = 0.5) +
    geom_text(aes(x = label_x, label = label),
              size = 2.2, hjust = 0, colour = NATURE_GREY) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    coord_cartesian(clip = "off") + labs(x = "Integrity gap", y = NULL) +
    theme_nature(base_size = 8) +
    theme(panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#F0F0F0", linewidth = 0.25),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      legend.position = "none", plot.margin = margin(4, 24, 4, 4))

  # ─── Panel d: parameter tornado (one driver per deduction dimension) ───
  tornado <- fd("fd_fig3_d")
  tornado$label <- factor(tornado$label, levels = tornado$label[order(tornado$ord)])
  fig_d <- ggplot(tornado, aes(x = label, y = rho)) +
    geom_col(width = 0.55, colour = NA, fill = "#999999") +
    geom_hline(yintercept = 0, colour = "#CCCCCC", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.2f", rho), hjust = text_hjust),
              size = 2.2, colour = NATURE_GREY) +
    scale_y_continuous(limits = c(-0.25, 0.95), breaks = seq(-0.2, 0.8, 0.2)) +
    coord_flip(clip = "off") + labs(x = NULL, y = "Rank correlation with NCV") +
    theme_nature(base_size = 8) +
    theme(panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#F0F0F0", linewidth = 0.25),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      legend.position = "none", plot.margin = margin(4, 24, 4, 4))

  fig <- fig_a + (fig_b / fig_c / fig_d + plot_layout(heights = c(1, 1.6, 0.8))) +
    plot_layout(ncol = 2, widths = c(1, 1.3)) +
    plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(size = 11, face = "bold"), plot.tag.position = c(0, 1))

  save_figure(fig, "fig3_headline", width = 240, height = 165)
})
