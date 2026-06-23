# =============================================================================
# figures/ed_scheme_gap.R — Extended Data: scheme integrity-gap decomposition
#   (PURE PLOT — all data prepared in engine/R/figdata/ed_scheme_gap.R)
# =============================================================================
# Single-panel horizontal stacked bar, one bar per carbon scheme. Each bar is
# one verified tonne (100% face value) split into five segments: scheme
# deductions, leakage gap (ΔL), temporality gap (ΔT), buffer gap (Δb), and the
# residual net climate value (proposed_net). Schemes ordered by descending
# integrity gap. Aesthetic is the validated legacy design; data are the engine's
# central scheme deltas. Run via run_figures.R (sources _setup_legacy.R).
# =============================================================================

local({
  # ─── Engine-computed tables: long stack (one row per scheme×component) and
  #     per-scheme gap labels; both carry integer ordering columns. ───
  plot_df  <- fd("fd_ed_scheme_gap_main")
  label_df <- fd("fd_ed_scheme_gap_labels")

  comp_levels   <- plot_df$component[order(plot_df$comp_ord)] |> unique()
  scheme_levels <- plot_df$scheme_name[order(plot_df$scheme_ord)] |> unique()

  plot_df$component   <- factor(plot_df$component,   levels = comp_levels)
  plot_df$scheme_name <- factor(plot_df$scheme_name, levels = scheme_levels)

  fill_cols <- c(
    "Net climate value" = unname(DEDUCTION_COLOURS["Net issuance"]),
    "Buffer gap"        = unname(DEDUCTION_COLOURS["Buffer"]),
    "Temporality gap"   = unname(DEDUCTION_COLOURS["Temporality"]),
    "Leakage gap"       = unname(DEDUCTION_COLOURS["Leakage"]),
    "Scheme deductions" = "#ECECEC"
  )

  p <- ggplot(plot_df,
              aes(x = scheme_name, y = share, fill = component)) +
    geom_col(position = position_stack(reverse = FALSE),
             width = 0.65, colour = "white", linewidth = 0.4) +
    scale_fill_manual(values = fill_cols, name = NULL,
                      guide = guide_legend(nrow = 1, reverse = FALSE)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       expand = expansion(mult = c(0, 0.05))) +
    coord_flip(clip = "off") +
    geom_text(data = label_df,
              aes(x = factor(scheme_name, levels = scheme_levels),
                  y = 1.13, label = gap_label),
              inherit.aes = FALSE,
              hjust = 0, size = 2.6, colour = NATURE_GREY,
              fontface = "bold") +
    labs(x = NULL, y = "Share of one verified tonne (face value)") +
    theme_nature(base_size = 9) %+replace%
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#EEEEEE", linewidth = 0.25),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.key.size    = unit(10, "pt"),
      legend.box.margin  = margin(t = 4),
      plot.margin        = margin(10, 50, 10, 10)
    )

  save_figure(p, "ed_fig_scheme_gap_decomp", width = 170, height = 95)
})
