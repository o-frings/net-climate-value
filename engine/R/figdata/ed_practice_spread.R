# =============================================================================
# figdata/ed_practice_spread.R — plot-ready tables for "ed_fig_buffer_practice_spread"
# =============================================================================
# ALL numeric preparation for the ED buffer/practice spread figure lives here.
# The figure script only reads these tables and draws. Emits:
#   fd_ed_practice_spread_a       — panel a per-biome buffer spread (median + ribbons, %)
#   fd_ed_practice_spread_b       — panel b per-biome disturbance trajectories (rate %)
#   fd_ed_practice_spread_meta    — scalar plot constants (axis limits, reference line,
#                                   annotation positions/labels)
# =============================================================================
local({
  BASELINE_BUFFER_RATE <- 0.20            # the "Flat 20%" reference line

  # ─── Panel a: per-biome buffer spread across forest-type variants (x100, %) ──
  practice_spread <- eng("buffer_biome_trajectory.csv") %>%
    group_by(year, rcp, biome) %>%
    summarise(
      br_med = median(buffer_rate) * 100,
      br_min = min(buffer_rate) * 100,
      br_max = max(buffer_rate) * 100,
      br_p25 = quantile(buffer_rate, 0.25) * 100,
      br_p75 = quantile(buffer_rate, 0.75) * 100,
      .groups = "drop"
    )

  pract_br_ylim_lo <- 5
  pract_br_ylim_hi <- max(practice_spread$br_max, na.rm = TRUE) + 2

  # ─── Panel b: per-biome disturbance trajectories (Historical + Future), % ────
  biome_dist <- eng("buffer_biome_disturbance.csv")
  biome_dist$rate_pct     <- biome_dist$rate * 100
  biome_dist$rate_p10_pct <- biome_dist$rate_p10 * 100   # projected prediction band
  biome_dist$rate_p90_pct <- biome_dist$rate_p90 * 100   # (NA for observed rows)

  # Individual projected realizations (each jagged) for the spaghetti overlay.
  dist_paths <- eng("buffer_biome_disturbance_paths.csv")
  dist_paths$rate_pct <- dist_paths$rate * 100

  dist_ylim_lo <- 0
  # Clip at the 97.5th percentile of the realizations so the bulk of the spaghetti
  # is visible; a few extreme draws run off-panel rather than flattening the rest.
  dist_ylim_hi <- max(quantile(dist_paths$rate_pct, 0.975, na.rm = TRUE),
                      biome_dist$rate_pct, na.rm = TRUE) * 1.05

  # ─── Scalar plot constants (axis limits, reference line, annotations) ────────
  meta <- data.frame(
    # axis limits
    pract_br_ylim_lo  = pract_br_ylim_lo,
    pract_br_ylim_hi  = pract_br_ylim_hi,
    dist_ylim_lo      = dist_ylim_lo,
    dist_ylim_hi      = dist_ylim_hi,
    # panel a "Flat 20%" reference line + annotation
    baseline_y        = BASELINE_BUFFER_RATE * 100,
    baseline_anno_x   = 2027,
    baseline_anno_y   = BASELINE_BUFFER_RATE * 100 + 0.8,
    baseline_anno_lab = paste0("Flat ", BASELINE_BUFFER_RATE * 100, "%"),
    # panel b dotted 2024 split + Observed/Projected annotation y
    split_x           = 2024,
    dist_anno_y       = dist_ylim_hi * 0.93,
    obs_anno_x        = 2010,
    proj_anno_x       = 2065,
    stringsAsFactors  = FALSE
  )

  wfd(practice_spread, "fd_ed_practice_spread_a")
  wfd(biome_dist,      "fd_ed_practice_spread_b")
  wfd(dist_paths,      "fd_ed_practice_spread_paths")
  wfd(meta,            "fd_ed_practice_spread_meta")
})
