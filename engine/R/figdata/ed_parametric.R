# =============================================================================
# engine/R/figdata/ed_parametric.R — plot-ready data for ed_parametric figure
# =============================================================================
# Parametric vs empirical buffer rates (faithful port of legacy 27). Cleveland
# dot plot: per country (y) and forest type (facet), three points per row
# distinguished by SHAPE = method and COLOUR = biome. ALL numeric preparation
# lives here; the figure script only reads the table and draws.
# Emits a single plot-ready table (one ggplot/facet panel):
#   fd_ed_parametric — one row per (country, forest_type, method) with the
#   x value already scaled to percent (b_pct), the method label + method_ord,
#   the biome, and an integer country_ord for the shared y ordering.
# =============================================================================
local({
  # 1) Parametric structural rates. The engine CSV already carries forest_type
  #    and b_parametric per zone; France & Italy have a Mediterranean and a
  #    Temperate row, so collapse to one row per (country, forest_type) with a
  #    forest-area weighted mean. biome[1] keeps the first-listed zone's biome
  #    (Mediterranean for France/Italy), matching the original colours.
  param <- eng("country_parametric_buffer.csv") %>%
    group_by(country = country_root, forest_type) %>%
    summarise(b_parametric = weighted.mean(b_parametric, forest_kha, na.rm = TRUE),
              biome = biome[1], .groups = "drop")

  # 2) Empirical VaR99 / TVaR99 (headline) at H = 40 yr.
  emp <- eng("clean_buffer_rates.csv") %>%
    select(country, forest_type,
           b_emp_VaR_99 = b_VaR99_H40, b_emp_TVaR_99 = b_TVaR99_H40)

  results <- emp %>%
    left_join(param, by = c("country", "forest_type")) %>%
    filter(!is.na(biome), !is.na(b_parametric))

  # 3) One point per (country, forest_type, method).
  method_levels <- c("b_parametric", "b_emp_VaR_99", "b_emp_TVaR_99")
  method_labels <- c("Parametric structural", "Empirical VaR99",
                     "Empirical TVaR99 (headline)")

  fig_data <- results %>%
    pivot_longer(cols = c(b_parametric, b_emp_VaR_99, b_emp_TVaR_99),
                 names_to = "method", values_to = "b") %>%
    filter(!is.na(b)) %>%
    mutate(method_ord = match(method, method_levels),
           method     = method_labels[method_ord],
           b_pct      = b * 100)

  # 4) Shared y ordering: ascending broadleaf headline (TVaR99) rate.
  rank_order <- fig_data %>%
    filter(forest_type == "broadleaf",
           method == "Empirical TVaR99 (headline)") %>%
    arrange(b) %>% pull(country)
  fig_data$country_ord <- match(fig_data$country, rank_order)

  wfd(fig_data, "fd_ed_parametric")
})
