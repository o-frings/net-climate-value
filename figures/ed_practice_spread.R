# =============================================================================
# figures/ed_practice_spread.R — ED figure "ed_fig_buffer_practice_spread"
# (faithful port of legacy 17 "FIGURE 5", engine data — PURE PLOT)
# =============================================================================
# 2x2 small-multiple grid (design "AB / CD / EE"), columns = RCP 4.5 / RCP 8.5.
#   Row a: per-biome buffer-rate spread (median line + min-max + IQR ribbons),
#          dashed "Flat 20%" reference. Row b: per-biome annual disturbance-rate
#          trajectories (Historical + Future projection), dotted 2024 split.
#   Shared bottom biome legend. Aesthetic is the validated legacy design.
#   All numeric preparation is done by the engine fragment
#   engine/R/figdata/ed_practice_spread.R; this script only reads fd() tables
#   and draws. Run via run_figures.R (which sources _setup_legacy.R first).
# =============================================================================

local({
  # ─── Engine-computed plot-ready tables ────────────────────────────────────
  practice_spread <- fd("fd_ed_practice_spread_a")   # panel a: buffer spread (%)
  biome_dist      <- fd("fd_ed_practice_spread_b")   # panel b: disturbance (rate, rate_pct)
  dist_paths      <- fd("fd_ed_practice_spread_paths") # panel b: individual realizations
  meta            <- fd("fd_ed_practice_spread_meta") # scalar plot constants

  pract_br_ylim <- c(meta$pract_br_ylim_lo, meta$pract_br_ylim_hi)
  dist_ylim     <- c(meta$dist_ylim_lo,     meta$dist_ylim_hi)

  # ─── Panel a: per-biome buffer spread (median line + min-max + IQR ribbons) ─
  make_panel_a <- function(rcp_code, show_y_label, col_title = NULL) {
    df <- practice_spread %>% filter(rcp == rcp_code)

    p <- ggplot(df, aes(x = year)) +
      geom_hline(yintercept = meta$baseline_y, linetype = "dashed",
                 colour = NATURE_GREY, linewidth = 0.3) +
      geom_ribbon(aes(ymin = br_min, ymax = br_max, fill = biome), alpha = 0.12) +
      geom_ribbon(aes(ymin = br_p25, ymax = br_p75, fill = biome), alpha = 0.20) +
      geom_line(aes(y = br_med, colour = biome), linewidth = 0.7, alpha = 0.85) +
      scale_colour_manual(values = BIOME_COLOURS) +
      scale_fill_manual(values = BIOME_COLOURS) +
      coord_cartesian(ylim = pract_br_ylim) +
      annotate("text", x = meta$baseline_anno_x, y = meta$baseline_anno_y,
               label = meta$baseline_anno_lab,
               size = 2.0, colour = NATURE_GREY, hjust = 0, fontface = "italic") +
      labs(y = if (show_y_label) "Practice buffer\nrate (%)" else NULL, x = NULL) +
      theme_nature(base_size = 8) +
      theme(axis.title.y = element_text(size = 7),
            axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            legend.position = "none")

    if (!is.null(col_title)) {
      p <- p + ggtitle(col_title) +
        theme(plot.title = element_text(size = 8, face = "bold", hjust = 0.5,
                                        colour = NATURE_GREY))
    }
    p
  }

  fig_a1 <- make_panel_a("RCP45", TRUE,  "RCP 4.5")
  fig_a2 <- make_panel_a("RCP85", FALSE, "RCP 8.5")

  # ─── Panel b: per-biome disturbance trajectories (Historical + Future) ──────
  make_panel_b <- function(rcp_code, show_y_label) {
    rcp_scenario <- paste0("Future: ", rcp_code)
    rcp_label    <- if (rcp_code == "RCP45") "RCP 4.5" else "RCP 8.5"

    df    <- biome_dist %>% filter(scenario %in% c("Historical", rcp_scenario))
    pf    <- dist_paths %>% filter(rcp == rcp_code)   # individual realizations
    y_top <- meta$dist_anno_y

    ggplot(df, aes(x = year, y = rate_pct, colour = biome)) +
      geom_vline(xintercept = meta$split_x, linetype = "dotted", colour = NATURE_GREY,
                 linewidth = 0.3) +
      # Individual MC realizations as thin faint lines: the projected hazard
      # varies year to year just like the observed record. The spaghetti shows
      # that variation and the ensemble spread directly; the central (mean)
      # trajectory is drawn bold on top.
      geom_line(data = pf, aes(group = interaction(draw, biome)),
                linewidth = 0.12, alpha = 0.07, show.legend = FALSE) +
      geom_line(linewidth = 0.5, alpha = 0.9) +
      scale_colour_manual(values = BIOME_COLOURS) +
      coord_cartesian(ylim = dist_ylim) +
      annotate("text", x = meta$obs_anno_x, y = y_top,
               label = "Observed", size = 1.8, colour = NATURE_GREY) +
      annotate("text", x = meta$proj_anno_x, y = y_top,
               label = paste0("Projected\n(", rcp_label, ")"),
               size = 1.8, colour = NATURE_GREY) +
      labs(y = if (show_y_label) "Annual rate\n(% forest area)" else NULL,
           x = "Year") +
      theme_nature(base_size = 8) +
      theme(axis.title.y = element_text(size = 7), legend.position = "none")
  }

  fig_b1 <- make_panel_b("RCP45", TRUE)
  fig_b2 <- make_panel_b("RCP85", FALSE)

  # ─── Shared bottom biome legend (extracted from a dummy line plot) ─────────
  leg_df <- tibble(
    x = rep(c(1, 2), each = 3), y = 1,
    biome = factor(rep(c("Boreal", "Temperate", "Mediterranean"), 2),
                   levels = c("Boreal", "Temperate", "Mediterranean"))
  )
  leg_plot <- ggplot(leg_df, aes(x = x, y = y)) +
    geom_line(aes(colour = biome)) +
    scale_colour_manual(values = BIOME_COLOURS, name = "Biome") +
    theme_void(base_size = 8) +
    theme(legend.position = "bottom", legend.box = "horizontal",
          legend.spacing.x = unit(6, "mm"),
          legend.text = element_text(size = 7, colour = NATURE_GREY),
          legend.title = element_text(size = 7, face = "bold", colour = NATURE_GREY),
          legend.key.width = unit(10, "mm")) +
    guides(colour = guide_legend(nrow = 1))

  leg_wrap <- if (requireNamespace("cowplot", quietly = TRUE)) {
    leg_gg <- suppressWarnings(cowplot::get_legend(leg_plot))
    wrap_elements(leg_gg) +
      theme(plot.margin = margin(0, 0, 0, 0),
            plot.background = element_rect(fill = NA, colour = NA),
            panel.background = element_rect(fill = NA, colour = NA)) +
      coord_cartesian(clip = "off")
  } else {
    # Graceful fallback: keep the same horizontal key without cowplot.
    leg_plot
  }

  # ─── Assemble: rows a (AB) / b (CD) / legend (EE) ─────────────────────────
  fig <- fig_a1 + fig_a2 + fig_b1 + fig_b2 + leg_wrap +
    plot_layout(design = "AB\nCD\nEE", heights = c(1, 1, 0.06)) +
    plot_annotation(tag_levels = list(c("a", "", "b", "", ""))) &
    theme(plot.tag = element_text(size = 10, face = "bold"))

  save_figure(fig, "ed_fig_buffer_practice_spread", width = 180, height = 130)
})
