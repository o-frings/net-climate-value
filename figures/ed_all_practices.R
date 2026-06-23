# =============================================================================
# figures/ed_all_practices.R — Extended Data Fig 2: all-practices decomposition
# =============================================================================
# PURE PLOT. Single-panel butterfly/tornado chart: every practice × biome ×
# species variant (44 rows) grouped into one facet band per practice. Left of
# zero = stacked grey deductions (Buffer · Time · Leakage); right of zero =
# net-issuance bar on a red→beige→blue gradient with a white % label. All data
# prep (medians, derived net_share, leakage clamp, variant labels, factor
# ordering) is done by the engine in engine/R/figdata/ed_all_practices.R and
# read here via fd(); this script only builds factors from the engine's integer
# order columns and draws. Run via run_figures.R (sources _setup_legacy.R first).
# =============================================================================

local({
  # ─── Shared butterfly-decomposition axis title (copied from fig3.R) ───
  butterfly_axis_title <- paste0(
    "← <b style='color:#8C8C8C'>Buffer</b>", " · ",
    "<b style='color:#ABABAB'>Time</b>",
    " <span style='color:#ABABAB; font-size:7pt'>(τ = 10–100 yr)</span>", " · ",
    "<b style='color:#606060'>Leakage</b>",
    "  <span style='color:#BBBBBB'>|</span>  ", "Net issuance →")

  # ─── Engine-computed plot-ready tables ───
  ded_long <- fd("fd_ed_all_practices_ded")
  net_bar  <- fd("fd_ed_all_practices_net")

  # Factor levels straight from the engine's integer order columns (no compute):
  # variant ascending, practice descending, component in stack order.
  variant_levels   <- ded_long$variant[order(ded_long$variant_ord)]
  variant_levels   <- variant_levels[!duplicated(variant_levels)]
  practice_levels  <- ded_long$practice[order(ded_long$practice_ord)]
  practice_levels  <- practice_levels[!duplicated(practice_levels)]
  component_levels <- ded_long$component[order(ded_long$component_ord)]
  component_levels <- component_levels[!duplicated(component_levels)]

  ded_long <- transform(ded_long,
    variant   = factor(variant, levels = variant_levels),
    practice  = factor(practice, levels = practice_levels),
    component = factor(component, levels = component_levels))
  net_bar <- transform(net_bar,
    variant  = factor(variant, levels = variant_levels),
    practice = factor(practice, levels = practice_levels))

  ded_cols <- c(Leakage = "#808080", Time = "#C8C8C8", Buffer = "#A8A8A8")

  fig <- ggplot() +
    geom_col(data = ded_long, aes(x = variant, y = neg_share, fill = component),
             position = position_stack(), width = 0.65, colour = NA) +
    scale_fill_manual(values = ded_cols, name = NULL, guide = "none") +
    ggnewscale::new_scale_fill() +
    geom_col(data = net_bar, aes(x = variant, y = net_share, fill = net_share),
             width = 0.65, colour = NA) +
    scale_fill_gradient2(low = "#C0392B", mid = "#E8D5C4", high = "#BDD7EE",
                         midpoint = 0.30, guide = "none") +
    geom_text(data = net_bar, aes(x = variant, y = label_y, label = label),
              size = 2.6, colour = "white", fontface = "bold") +
    geom_hline(yintercept = 0, colour = "#CCCCCC", linewidth = 0.3) +
    facet_grid(practice ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_y_continuous(
      breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
      labels = c("75%", "50%", "25%", "0%", "25%", "50%", "75%"),
      expand = expansion(mult = c(0.02, 0.02))) +
    coord_flip() +
    labs(x = NULL, y = butterfly_axis_title) +
    theme_nature(base_size = 9) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#F0F0F0", linewidth = 0.25),
      panel.spacing.y = unit(2, "pt"),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, size = 6.5, hjust = 1,
                                       margin = margin(r = 2)),
      strip.background = element_rect(fill = NA, colour = NA),
      axis.title.x = ggtext::element_markdown(size = 7),
      legend.position = "none",
      plot.margin = margin(6, 6, 6, 6))

  save_figure(fig, "ed_fig2_all_practices_decomp", width = 170, height = 240)
})
