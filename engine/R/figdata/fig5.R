# =============================================================================
# engine/R/figdata/fig5.R — plot-ready tables for fig5 (scenario comparison)
# =============================================================================
# Builds the data behind fig5's two panels so figures/fig5.R only plots.
# In scope (from 15_figure_data.R): mc_results, eng(), wfd(), dplyr/tidyr,
# label helpers from _labels.R.
#   fd_fig5_area     — panel a: per-scenario today distribution, ordered, with
#                      median-keyed gradient fill bounds + midpoint.
#   fd_fig5_ts       — panel b: 5-yr-smoothed area over time per scenario x RCP,
#                      with precomputed gradient line colour per scenario.
#   fd_fig5_ts_ends  — panel b: end labels (RCP 4.5 endpoints, short forms).
# =============================================================================
local({
  # ─── Scenario label map (engine scenario id -> display label) ───
  scn_labels <- c(
    increasing_only  = "Increasing only",
    mixed_increasing = "Mixed: increasing",
    neutral_only     = "Neutral only",
    mixed_neutral    = "Mixed: neutral",
    mixed_reducing   = "Mixed: reducing",
    reducing_only    = "Reducing only"
  )
  pure_labels <- c("Reducing only", "Neutral only", "Increasing only")

  # ─── Panel a: today's NCV-adjusted area distribution per scenario ───
  # area_mean is the MC median (engine convention); area_mean_arith the dot.
  today <- eng("scenario_area_today.csv")
  area_stats <- today %>%
    transmute(
      scenario,
      scn_label = unname(scn_labels[scenario]),
      med_area  = area_mean,
      mean_area = area_mean_arith,
      p5_area   = area_p5,
      p25_area  = area_p25,
      p75_area  = area_p75,
      p95_area  = area_p95
    )

  # Order by MC median (highest median -> y_num 1). FIX vs the legacy figure: the
  # old axis used breaks = y_num but labels = levels() (a different ordering), so
  # the printed row labels were offset from the boxes — a mislabelling. Here each
  # box is labelled with its OWN scenario, so y-axis names match the boxes.
  area_order <- area_stats$scn_label[order(area_stats$med_area, decreasing = TRUE)]
  area_stats <- area_stats %>%
    mutate(
      y_num      = match(scn_label, area_order),  # ggplot y position of each box
      ord        = y_num,                         # integer order for the factor
      axis_label = scn_label,                     # label each box with its own scenario
      mid_area   = median(med_area),              # gradient-fill midpoint (panel a)
      ref_faws   = 135,                           # EU FAWS reference line (Mha)
      ref_tot    = 160                            # EU total forest reference (Mha)
    )

  # Per-scenario gradient line colour for panel b: interpolate the same
  # blue->beige->red palette across the median-area range (was scales::col_numeric
  # in the figure script). Done here so the plot only does setNames lookups.
  area_colour_fn <- scales::col_numeric(
    palette = c("#BDD7EE", "#E8D5C4", "#C0392B"),
    domain  = range(area_stats$med_area))
  area_stats$line_colour <- area_colour_fn(area_stats$med_area)

  wfd(area_stats, "fd_fig5_area")

  # ─── Panel b: NCV-adjusted area over time per scenario x RCP ───
  fwd <- eng("scenario_area_forward.csv") %>%
    group_by(scenario, rcp) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      area_mean = zoo::rollmean(area_mean, k = 5, fill = NA, align = "center"),
      area_p25  = zoo::rollmean(area_p25,  k = 5, fill = NA, align = "center"),
      area_p75  = zoo::rollmean(area_p75,  k = 5, fill = NA, align = "center")
    ) %>%
    ungroup() %>%
    filter(!is.na(area_mean)) %>%
    mutate(
      scn_label  = unname(scn_labels[scenario]),
      rcp_label  = ifelse(rcp == "RCP45", "RCP 4.5", "RCP 8.5"),
      is_pure    = scn_label %in% pure_labels,
      ref_faws   = 135,
      ref_tot    = 160,
      label_year = max(year)
    )

  # Attach the panel-a ordering + precomputed gradient colour so the plot can
  # factor() by 'ord' and scale_*_manual() off 'line_colour' without arithmetic.
  scn_meta <- area_stats %>%
    transmute(scn_label, scn_ord = ord, line_colour)
  fwd <- fwd %>% left_join(scn_meta, by = "scn_label")

  wfd(fwd, "fd_fig5_ts")

  # End labels at the right (RCP 4.5 endpoints, short forms).
  area_end_labels <- fwd %>%
    filter(year == max(year), rcp == "RCP45") %>%
    mutate(short_label = gsub("Mixed: ", "M:", scn_label))

  wfd(area_end_labels, "fd_fig5_ts_ends")
})
