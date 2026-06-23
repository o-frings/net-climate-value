# =============================================================================
# figures/fig5.R — scenario comparison (faithful port of legacy 17), PURE PLOT
# =============================================================================
# All data prep (medians/quantiles, 5-yr rollmean, gradient-colour mapping,
# ordering, labels) lives in engine/R/figdata/fig5.R and is read here via fd():
#   fd_fig5_area    — panel a per-scenario today distribution (ordered, coloured)
#   fd_fig5_ts      — panel b smoothed area-over-time per scenario x RCP
#   fd_fig5_ts_ends — panel b end labels
# This script only draws. Run via run_figures.R (sources _setup_legacy.R first).
#
# a: horizontal box-plot summary of NCV-adjusted area (Mha) for the 6 CRCF
#    scenarios, rows ordered by MC median area, IQR boxes gradient-filled by
#    median, p5-p95 whiskers, white median line, mean dot, right-side
#    "median [p5, p95]" labels, EU FAWS / EU total forest reference lines.
# b: NCV-adjusted area over time (2025-2100) per scenario x RCP, gradient lines
#    (pure thick / mixed thin, RCP 4.5 solid / RCP 8.5 dashed), p25-p75 ribbons,
#    EU reference lines, direct end labels.
# =============================================================================

local({
  has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)

  # ─── Panel a data (engine-computed) ───
  # Rows are in engine CSV order; the fragment emits y_num (box position),
  # axis_label (legacy row labels, row-aligned to y_num), ord (median rank for
  # factor levels / colour keying), mid_area, and line_colour.
  area_stats   <- fd("fd_fig5_area")
  midpoint     <- area_stats$mid_area[1]              # gradient-fill midpoint
  # Scenario levels in median-rank order (highest median = ord 1) for panel b.
  area_by_rank <- area_stats[order(area_stats$ord), ]
  area_levels  <- area_by_rank$scn_label

  rh_b <- 0.35  # half-height of IQR boxes

  panel_scn_area <- ggplot(area_stats, aes(y = y_num)) +
    # EU forest reference lines (vertical)
    geom_vline(xintercept = 135, linetype = "dashed", colour = "#888888", linewidth = 0.4) +
    geom_vline(xintercept = 160, linetype = "dashed", colour = "#888888", linewidth = 0.4) +
    # Whiskers: p5-p95 with end caps
    geom_segment(aes(x = p5_area, xend = p95_area, y = y_num, yend = y_num),
                 colour = "#999999", linewidth = 0.35) +
    geom_segment(aes(x = p5_area, xend = p5_area,
                     y = y_num - rh_b * 0.5, yend = y_num + rh_b * 0.5),
                 colour = "#999999", linewidth = 0.35) +
    geom_segment(aes(x = p95_area, xend = p95_area,
                     y = y_num - rh_b * 0.5, yend = y_num + rh_b * 0.5),
                 colour = "#999999", linewidth = 0.35) +
    # IQR box with gradient fill keyed to median
    geom_rect(aes(xmin = p25_area, xmax = p75_area,
                  ymin = y_num - rh_b, ymax = y_num + rh_b, fill = med_area),
              alpha = 0.85, colour = NA) +
    scale_fill_gradient2(low = "#BDD7EE", mid = "#E8D5C4", high = "#C0392B",
                         midpoint = midpoint, guide = "none") +
    # White median line
    geom_segment(aes(x = med_area, xend = med_area,
                     y = y_num - rh_b, yend = y_num + rh_b),
                 colour = "white", linewidth = 0.6) +
    # Mean dot
    geom_point(aes(x = mean_area), shape = 21, size = 1.5,
               fill = "#333333", colour = "white", stroke = 0.4) +
    # Right-side label: median [p5, p95]
    geom_text(aes(x = pmax(p95_area, mean_area) + 5,
                  label = sprintf("%.0f  [%.0f, %.0f]", med_area, p5_area, p95_area)),
              size = 1.8, hjust = 0, colour = NATURE_GREY) +
    # Reference line labels (above the top row)
    annotate("text", y = max(area_stats$y_num) + 0.7, x = 133,
             label = "EU FAWS", size = 2.2, colour = "#666666",
             hjust = 1, vjust = 0.5, fontface = "italic") +
    annotate("text", y = max(area_stats$y_num) + 0.7, x = 162,
             label = "EU total forest", size = 2.2, colour = "#666666",
             hjust = 0, vjust = 0.5, fontface = "italic") +
    scale_x_continuous(trans = "sqrt",
                       expand = expansion(mult = c(0.02, 0.12)),
                       breaks = c(50, 100, 150, 200, 250, 300, 400)) +
    scale_y_continuous(breaks = area_stats$y_num,
                       labels = area_stats$axis_label,
                       expand = expansion(add = c(0.5, 0.8))) +
    coord_cartesian(clip = "off") +
    labs(y = NULL, x = "NCV-adj. area (Mha)") +
    theme_nature(base_size = 9) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#F0F0F0", linewidth = 0.25),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(size = 7),
      legend.position = "none",
      plot.margin = margin(6, 6, 6, 4)
    )

  # ─── Panel b data (engine-computed: smoothed, ordered, colour-keyed) ───
  fwd <- fd("fd_fig5_ts")
  # Factor the scenario by the panel-a order column the fragment emitted.
  fwd$scn_label <- factor(fwd$scn_label, levels = area_levels)
  fwd$rcp_label <- factor(fwd$rcp_label, levels = c("RCP 4.5", "RCP 8.5"))
  label_year <- fwd$label_year[1]

  # Gradient line colour per scenario (precomputed in the fragment).
  ts_colours <- setNames(area_stats$line_colour, area_stats$scn_label)

  area_end_labels <- fd("fd_fig5_ts_ends")
  area_end_labels$scn_label <- factor(area_end_labels$scn_label, levels = area_levels)

  panel_scn_ts <- ggplot(fwd, aes(x = year, y = area_mean,
                                  colour = scn_label, linetype = rcp_label)) +
    # EU forest reference lines (horizontal)
    geom_hline(yintercept = 135, linetype = "dashed", colour = "#888888", linewidth = 0.4) +
    geom_hline(yintercept = 160, linetype = "dashed", colour = "#888888", linewidth = 0.4) +
    annotate("text", x = 2027, y = 135, label = "EU FAWS", size = 2.0,
             colour = "#666666", hjust = 0, vjust = 1.5, fontface = "italic") +
    annotate("text", x = 2027, y = 160, label = "EU total forest", size = 2.0,
             colour = "#666666", hjust = 0, vjust = 1.5, fontface = "italic") +
    # MC IQR ribbons (p25-p75) per scenario x RCP
    geom_ribbon(data = fwd[!is.na(fwd$area_p25) & !is.na(fwd$area_p75), ],
                aes(ymin = area_p25, ymax = area_p75,
                    fill = scn_label, group = interaction(scn_label, rcp_label)),
                alpha = 0.18, colour = NA) +
    # Pure scenarios: thicker lines
    geom_line(data = fwd[fwd$is_pure, ], linewidth = 0.7) +
    # Mixed scenarios: thinner lines
    geom_line(data = fwd[!fwd$is_pure, ], linewidth = 0.35) +
    scale_colour_manual(values = ts_colours, guide = "none") +
    scale_fill_manual(values = ts_colours, guide = "none") +
    scale_linetype_manual(values = c("RCP 4.5" = "solid", "RCP 8.5" = "dashed"),
                          guide = "none") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                       expand = expansion(mult = c(0.03, 0.05))) +
    scale_x_continuous(breaks = seq(2030, 2100, 20),
                       expand = expansion(mult = c(0.02, 0.22))) +
    labs(x = "Year", y = "NCV-adj. area (Mha)") +
    theme_nature(base_size = 9) +
    theme(
      panel.grid.major = element_line(colour = "#F0F0F0", linewidth = 0.25),
      axis.title = element_text(size = 7),
      legend.position = "none",
      plot.margin = margin(6, 6, 6, 6)
    )

  # Direct end labels (ggrepel if available, else plain text)
  if (has_ggrepel) {
    panel_scn_ts <- panel_scn_ts +
      ggrepel::geom_text_repel(
        data = area_end_labels,
        aes(x = label_year, y = area_mean, label = short_label),
        hjust = 0, size = 2.0, show.legend = FALSE, fontface = "plain",
        direction = "y", nudge_x = 2, segment.size = 0.15,
        segment.colour = "#CCCCCC", min.segment.length = 0.1,
        box.padding = 0.25, force = 8, force_pull = 0.3,
        xlim = c(label_year + 1, NA), seed = 42, max.overlaps = 20)
  } else {
    panel_scn_ts <- panel_scn_ts +
      geom_text(data = area_end_labels,
                aes(x = label_year + 2, y = area_mean, label = short_label),
                hjust = 0, size = 2.0, show.legend = FALSE)
  }

  # ─── Layout: a = area box plots | b = time series ───
  tag_theme <- theme(plot.tag = element_text(size = 11, face = "bold"),
                     plot.tag.position = c(0, 1))
  panel_scn_area <- panel_scn_area + labs(tag = "a") + tag_theme
  panel_scn_ts   <- panel_scn_ts   + labs(tag = "b") + tag_theme

  fig_scenarios <- (panel_scn_area | panel_scn_ts) + plot_layout(widths = c(1, 1))

  save_figure(fig_scenarios, "fig5_scenario_comparison", width = 240, height = 110)
})
