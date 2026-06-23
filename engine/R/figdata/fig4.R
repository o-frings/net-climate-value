# =============================================================================
# engine/R/figdata/fig4.R — plot-ready tables for fig4 (buffer solvency, 2 panels)
# =============================================================================
# All numeric prep for fig4 (figures/fig4.R only plots). Emits:
#   fd_fig4_a       panel a: per-scenario country-level buffer (%), 5-yr smoothed
#                   central median (buf_smooth) + full-MC-range bounds (min/max_smooth)
#   fd_fig4_env_a   panel b: across-pure-scenario median + spread, country level
#   fd_fig4_env_b   panel b: same at the EU-pool level (country rate x div_ratio)
#   fd_fig4_colours scenario -> diverging-ramp hex (+ scn_ord for factor levels)
#   fd_fig4_annot   baseline (flat 20%) annotation scalars
# In scope (15_figure_data.R): dplyr, eng(), wfd().
# =============================================================================
local({
  has_zoo    <- requireNamespace("zoo", quietly = TRUE)
  has_scales <- requireNamespace("scales", quietly = TRUE)

  scn_labels <- c(
    reducing_only    = "Reducing\nonly",
    neutral_only     = "Neutral\nonly",
    increasing_only  = "Increasing\nonly",
    mixed_reducing   = "Mixed\n(reducing)",
    mixed_neutral    = "Mixed\n(neutral)",
    mixed_increasing = "Mixed\n(increasing)")
  pure_scn <- c("Reducing\nonly", "Neutral\nonly", "Increasing\nonly")
  scn_ord  <- setNames(seq_along(scn_labels), unname(scn_labels))

  traj <- eng("scenario_buffer_trajectory.csv")
  summ <- eng("scenario_buffer_summary_clean.csv")

  roll5 <- function(x) {
    if (has_zoo) return(zoo::rollmean(x, k = 5, fill = NA, align = "center"))
    n <- length(x); out <- rep(NA_real_, n)
    for (i in 3:(n - 2)) out[i] <- mean(x[(i - 2):(i + 2)])
    out
  }

  # ─── Panel a: per-scenario central (MC median) + full-MC-range bounds, smoothed ─
  scn_ts <- traj %>%
    mutate(rcp_label = ifelse(rcp == "RCP45", "RCP 4.5", "RCP 8.5"),
           buf_pct   = buffer_rate * 100,
           min_pct   = buffer_rate_min * 100,
           max_pct   = buffer_rate_max * 100,
           scn_label = scn_labels[scenario]) %>%
    group_by(scenario, rcp) %>%
    arrange(year) %>%
    mutate(buf_smooth = roll5(buf_pct),
           min_smooth = roll5(min_pct),
           max_smooth = roll5(max_pct)) %>%
    ungroup() %>%
    filter(!is.na(buf_smooth)) %>%
    mutate(scn_ord = scn_ord[scn_label], is_pure = scn_label %in% pure_scn)

  # ─── Scenario colours: diverging ramp keyed to scenario mean buffer rate ──────
  scn_mean_buf <- summ %>%
    group_by(scenario) %>%
    summarise(mean_buf = mean(mean_buffer_rate), .groups = "drop")
  # Diverging ramp centred on the flat 20% benchmark (beige at 0.20): scenarios
  # above the benchmark shade red, below would shade blue -- colour shows each
  # scenario's distance from the flat-20% reference line.
  bench <- 0.20
  half  <- max(abs(scn_mean_buf$mean_buf - bench))
  if (has_scales) {
    scn_col_fn <- scales::col_numeric(palette = c("#BDD7EE", "#E8D5C4", "#C0392B"),
                                      domain = c(bench - half, bench + half))
    scn_ts_colours <- setNames(scn_col_fn(scn_mean_buf$mean_buf),
                               scn_labels[scn_mean_buf$scenario])
  } else {
    ramp <- grDevices::colorRamp(c("#BDD7EE", "#E8D5C4", "#C0392B"))
    t01  <- (scn_mean_buf$mean_buf - (bench - half)) / (2 * half)
    scn_ts_colours <- setNames(grDevices::rgb(ramp(t01), maxColorValue = 255),
                               scn_labels[scn_mean_buf$scenario])
  }
  colours_tbl <- data.frame(scn_label = names(scn_ts_colours),
                            colour = unname(scn_ts_colours), stringsAsFactors = FALSE)
  colours_tbl$scn_ord <- scn_ord[colours_tbl$scn_label]
  colours_tbl <- colours_tbl[order(colours_tbl$scn_ord), ]

  # ─── EU-pool scaling: full-pool pool/per-project ratio (random enrolment, K=all) ─
  pool <- eng("pool_buildup.csv")
  pool_rand <- pool[pool$ordering == "Random enrolment", ]
  div_ratio <- pool_rand$div_ratio[which.max(pool_rand$K)]

  # ─── Panel b: per-level median (lines + surplus) and one spread band per level ─
  # Per (year, RCP): median of the 3 pure scenarios -> median LINES (solid 4.5 /
  # dashed 8.5) and the diversification-surplus wedge between country and EU.
  med_of <- function(d) d %>% filter(is_pure) %>%
    group_by(year, rcp_label) %>%
    summarise(med = median(buf_smooth), .groups = "drop")
  env_a <- med_of(scn_ts)                                                   # country level
  env_b <- med_of(scn_ts %>% mutate(buf_smooth = buf_smooth * div_ratio))   # EU pool level
  # Per YEAR (across BOTH RCPs and the 3 pure scenarios): ONE spread band per level,
  # drawn as a single ribbon so it widens toward 2100 with RCP divergence instead of
  # the misleading darker overlap of two per-RCP ribbons that thins over time.
  band_of <- function(d) d %>% filter(is_pure) %>%
    group_by(year) %>%
    summarise(lo = min(buf_smooth), hi = max(buf_smooth), .groups = "drop")
  band_a <- band_of(scn_ts)                                                 # country level
  band_b <- band_of(scn_ts %>% mutate(buf_smooth = buf_smooth * div_ratio)) # EU pool level

  # ─── Baseline (flat 20%) annotation scalars ───────────────────────────────────
  consts <- read.csv("engine/params/model_constants.csv", stringsAsFactors = FALSE)
  base_r <- consts$value[consts$name == "BASELINE_BUFFER_RATE"]
  annot <- data.frame(baseline_y = base_r * 100, baseline_label_x = 2027,
                      baseline_label_y = base_r * 100 - 1.5,
                      baseline_label = paste0("Flat ", base_r * 100, "%"),
                      stringsAsFactors = FALSE)

  wfd(scn_ts,      "fd_fig4_a")
  wfd(env_a,       "fd_fig4_env_a")
  wfd(env_b,       "fd_fig4_env_b")
  wfd(band_a,      "fd_fig4_band_a")
  wfd(band_b,      "fd_fig4_band_b")
  wfd(colours_tbl, "fd_fig4_colours")
  wfd(annot,       "fd_fig4_annot")
})
