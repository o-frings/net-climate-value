# =============================================================================
# figdata/ed_country_disturbance.R — plot-ready table for ed_fig_country_disturbance
# =============================================================================
# Country-level EFDA disturbance maps (ED). Two choropleth panels:
#   a: forest-area-weighted mean natural-disturbance rate 1986-2023 (% yr-1)
#   b: post-2018 elevation ratio (2018-2023 vs 1986-2017), capped at 6x.
# ALL numeric data prep lives here; the figure script only joins polygons and
# draws. One table is emitted (one country per row, both panels share it).
# Runs inside 15_figure_data.R (eng/wfd/dplyr in scope).
# =============================================================================

local({
  country_data <- eng("country_disturbance.csv")

  fd_country <- country_data |>
    mutate(
      # panel a fill: disturbance rate scaled to % yr-1
      lambda_full_pct       = lambda_full * 100,
      # panel b fill: elevation ratio capped at 6x for the colour scale
      elevation_ratio_capped = pmin(elevation_ratio, 6)
    ) |>
    arrange(country) |>
    select(country, lambda_full, lambda_pre2018, lambda_post2018,
           forest_kha, elevation_ratio,
           lambda_full_pct, elevation_ratio_capped)

  wfd(fd_country, "fd_ed_country_disturbance")
})
