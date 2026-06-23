# =============================================================================
# engine/R/figdata/ed_jrc.R — plot-ready table for ed_jrc (JRC country forest plot)
# =============================================================================
# Builds the single plot-ready table for figures/ed_jrc.R: per-country buffer
# rates (our empirical TVaR99 H40 rate vs the JRC BP_contribution band/mean),
# faceted by forest type. ALL data prep lives here — biome join, percent
# scaling, forest-type labelling, and the per-facet y-axis ordering encoded as
# an integer 'ord' column. The figure script only reads this and draws.
#
# Faithful port of the prior in-figure logic. Biome (not carried in the JRC CSV)
# is recovered from the engine's country_parametric_buffer.csv — the same
# (country_root -> biome) map as COUNTRY_PARAMS, first row per country_root.
#
# Ordering note: the prior code ordered country rows within each facet by
# (biome, jrc_mean) via arrange(forest_type, biome, jrc_mean) + group_by(
# forest_type) + factor(levels = unique(country_root)). A column holds one set
# of levels, so the levels come from the FIRST group (Broadleaf). Every country
# appears in both facets, so facet_wrap(free_y) drops nothing and BOTH panels
# render in that same Broadleaf-derived order. 'ord' reproduces this exactly:
# it is the position of each country in that single global level sequence.
# =============================================================================

local({
  cmp <- eng("jrc_country_comparison.csv")

  # Biome lookup: first row per country_root (France/Italy -> Mediterranean),
  # identical to the COUNTRY_PARAMS-derived map the figure used before.
  biome_lookup <- eng("country_parametric_buffer.csv") %>%
    select(country_root, biome) %>%
    distinct(country_root, .keep_all = TRUE)
  cmp <- cmp %>% left_join(biome_lookup, by = "country_root")

  # Global y-order = the factor levels the prior code produced: arrange by
  # (forest_type, biome, jrc_mean) and take unique(country_root). With
  # forest_type ordered Broadleaf-before-Conifer, the Broadleaf group fixes the
  # level sequence for the shared (single-column) factor.
  level_order <- cmp %>%
    mutate(forest_type = factor(forest_type, levels = c("broadleaf", "conifer"))) %>%
    arrange(forest_type, biome, jrc_mean) %>%
    pull(country_root) %>%
    unique()

  fd_ed_jrc_forest <- cmp %>%
    transmute(
      country_root,
      biome,
      forest_type = factor(forest_type, levels = c("broadleaf", "conifer"),
                           labels = c("Broadleaf", "Conifer")),
      # percent-scaled positions for every geom (no arithmetic left in plot)
      jrc_p10_pct  = jrc_p10  * 100,
      jrc_p90_pct  = jrc_p90  * 100,
      jrc_mean_pct = jrc_mean * 100,
      b_ours_pct   = b_ours   * 100,
      # integer ordering for factor(country_root): low = bottom of y-axis
      ord = match(country_root, level_order)
    ) %>%
    mutate(forest_type = as.character(forest_type)) %>%
    arrange(forest_type, ord)

  wfd(fd_ed_jrc_forest, "fd_ed_jrc_forest")
})
