# =============================================================================
# figures/ed_parametric.R — parametric vs empirical buffer rates (faithful port
# of legacy 27), engine data. PURE PLOT: reads engine-computed plot-ready table.
# =============================================================================
# Cleveland dot plot: per country (y) and forest type (facet), three points per
# row distinguished by SHAPE = method (parametric structural / empirical VaR99 /
# empirical TVaR99 headline) and COLOUR = biome. Aesthetic is the validated
# legacy design (script 27); data are engine outputs (engine/R/figdata builds
# fd_ed_parametric). Run via run_figures.R (which sources _setup_legacy.R first).
# =============================================================================

local({
  fig_data <- fd("fd_ed_parametric")

  # Restore the factor orderings the engine emitted (method shapes/legend order,
  # shared ascending-by-broadleaf-headline country ordering). Countries absent
  # from that ordering carry NA country_ord and become NA factor levels, exactly
  # as the original levels = rank_order dropped them.
  m_lvl <- fig_data[order(fig_data$method_ord), ]
  fig_data$method  <- factor(fig_data$method, levels = unique(m_lvl$method))
  c_ord <- fig_data[!is.na(fig_data$country_ord), ]
  c_ord <- c_ord[order(c_ord$country_ord), ]
  fig_data$country <- factor(fig_data$country, levels = unique(c_ord$country))

  BIOME_COLOURS_FIG <- c(
    Boreal        = "#4A90D9",
    Temperate     = "#009E73",
    Mediterranean = "#E69F00",
    Temperate_UK  = "#56B4E9")

  p <- ggplot(fig_data,
              aes(x = b_pct, y = country, colour = biome, shape = method)) +
    geom_point(size = 1.6, alpha = 0.85) +
    facet_wrap(~ forest_type, scales = "free_y") +
    scale_colour_manual(values = BIOME_COLOURS_FIG, name = NULL) +
    scale_shape_manual(values = c(16, 4, 17), name = NULL) +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0.01, 0.05))) +
    labs(x = "Buffer rate (H = 40 yr)", y = NULL) +
    theme_minimal(base_size = 8) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.25),
      strip.background   = element_blank(),
      strip.text         = element_text(face = "bold", size = rel(1)),
      legend.position    = "bottom",
      legend.box         = "vertical",
      legend.spacing.y   = unit(0, "pt"),
      legend.margin      = margin(t = 0, b = 0),
      axis.text.y        = element_text(size = rel(0.85)),
      panel.spacing.x    = unit(8, "pt"))

  save_figure(p, "ed_fig_parametric_vs_empirical", width = 220, height = 220)
})
