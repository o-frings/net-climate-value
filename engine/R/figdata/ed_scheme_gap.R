# =============================================================================
# engine/R/figdata/ed_scheme_gap.R — plot-ready data for ed_scheme_gap
# =============================================================================
# Scheme integrity-gap decomposition (Extended Data). Single-panel horizontal
# stacked bar: one verified tonne (100% face value) split into five segments per
# scheme — scheme deductions, leakage gap (delta_L), temporality gap (delta_T),
# buffer gap (delta_b), and the residual net climate value (proposed_net).
# ALL numeric data prep lives here; figures/ed_scheme_gap.R only reads these
# tables and draws. Runs inside 15_figure_data.R (mc_results, eng(), wfd(),
# dplyr/tidyr, label helpers in scope). Source: scheme_gap_decomposition.csv.
# =============================================================================

local({
  # ─── Engine decomposition: one row per scheme; the 5-segment stack sums to 1 ─
  d <- eng("scheme_gap_decomposition.csv")

  # Component factor levels (stack/legend order) → integer 'comp_ord' the plot
  # uses to rebuild the factor.
  comp_levels <- c(
    "Net climate value", "Buffer gap",
    "Temporality gap", "Leakage gap",
    "Scheme deductions"
  )

  # Scheme order by total gap (ascending pct_gap → largest gap on top after
  # coord_flip) → integer 'scheme_ord' the plot uses to rebuild the factor.
  scheme_order <- d$scheme_name[order(d$pct_gap)]

  plot_df <- d %>%
    transmute(
      scheme_name,
      pct_gap,
      `Net climate value` = proposed_net,
      `Buffer gap`        = delta_b,
      `Temporality gap`   = delta_T,
      `Leakage gap`       = delta_L,
      `Scheme deductions` = 1 - scheme_net
    ) %>%
    pivot_longer(-c(scheme_name, pct_gap),
                 names_to = "component", values_to = "share") %>%
    mutate(
      comp_ord   = match(component, comp_levels),
      scheme_ord = match(scheme_name, scheme_order)
    )

  # Per-scheme gap labels (one row per scheme) + ordering integer.
  label_df <- d %>%
    transmute(
      scheme_name,
      pct_gap,
      gap_label  = sprintf("gap %.0f%%", pct_gap * 100),
      scheme_ord = match(scheme_name, scheme_order)
    )

  wfd(plot_df,  "fd_ed_scheme_gap_main")
  wfd(label_df, "fd_ed_scheme_gap_labels")
})
