# =============================================================================
# figures/ed_jrc.R — JRC vs ours per-country buffer-rate forest plot (PURE PLOT)
# =============================================================================
# Per-country comparison of our empirical TVaR99 H40 buffer rate against the
# JRC BP_contribution distribution, faceted by forest type (Broadleaf/Conifer).
# Each country row carries: JRC P10-P90 band (segment), JRC mean (× cross), and
# our rate (○ hollow circle), all coloured by biome. All data prep (biome join,
# percent scaling, forest-type labels, per-facet y-ordering) is done in the
# engine fragment engine/R/figdata/ed_jrc.R; this script only reads the table
# and draws. Run via run_figures.R (which sources _setup_legacy.R first).
# =============================================================================

local({
  # Biome palette (legacy BIOME_COLOURS_FIG, Okabe–Ito plus a UK light blue).
  BIOME_COLOURS_FIG <- c(
    Boreal        = "#4A90D9",
    Temperate     = "#009E73",
    Mediterranean = "#E69F00",
    Temperate_UK  = "#56B4E9"
  )

  # ─── Engine-computed plot-ready table (all values pre-derived) ───
  cmp_plot <- fd("fd_ed_jrc_forest")

  # Factor ordering from the engine-emitted 'ord' (low = bottom of y-axis) and
  # the facet order; no numeric work here.
  cmp_plot$country_root <- factor(cmp_plot$country_root,
                                  levels = unique(cmp_plot$country_root[order(cmp_plot$ord)]))
  cmp_plot$forest_type  <- factor(cmp_plot$forest_type, levels = c("Broadleaf", "Conifer"))

  # ─── Single faceted forest plot ───
  p <- ggplot(cmp_plot, aes(y = country_root, colour = biome)) +
    geom_segment(aes(x = jrc_p10_pct, xend = jrc_p90_pct,
                     yend = country_root),
                 linewidth = 2.5, alpha = 0.25) +
    geom_point(aes(x = jrc_mean_pct), shape = 4, size = 1.3, alpha = 0.7) +
    geom_point(aes(x = b_ours_pct), shape = 21, size = 2,
               fill = "white", stroke = 0.7) +
    facet_wrap(~ forest_type, scales = "free_y") +
    scale_colour_manual(values = BIOME_COLOURS_FIG, name = NULL) +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0.01, 0.05))) +
    labs(x = "Buffer rate — JRC P10-P90 band, JRC mean (×), our rate (○)",
         y = NULL) +
    theme_nature(base_size = 8) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.25),
      strip.background   = element_blank(),
      strip.text         = element_text(face = "bold", size = rel(1)),
      legend.position    = "bottom",
      panel.spacing.x    = unit(8, "pt")
    )

  save_figure(p, "ed_fig_jrc_country_comparison", width = 220, height = 200)
})
