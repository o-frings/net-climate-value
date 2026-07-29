# =============================================================================
# engine/R/figdata/fig3.R — plot-ready tables for the headline 4-panel figure
# =============================================================================
# Computes ALL numeric data for fig3 (faithful port of the calculations that
# used to live in figures/fig3.R). The figure script now only reads the fd_*
# tables and draws. In scope (from 15_figure_data.R): mc_results, eng(), wfd(),
# dplyr/tidyr, and the label helpers from _labels.R.
#   a: butterfly decomposition (buffer·time·leakage + net issuance) for
#      scheme-covered practices, split into the deduction-long table and the
#      net-bar table.
#   b: net-issuance MC summary by supply class.
#   c: integrity-gap MC summary by scheme.
#   d: parameter tornado (rank correlations with NCV).
# =============================================================================

local({
  # Harvest class is DERIVED from the sign of the harvest displacement fraction x, which is
  # what defines the three groups (Table 1: x > 0 harvest-reducing, x = 0 harvest-neutral,
  # x < 0 harvest-increasing / supply-positive). The previous hardcoded 15-entry map
  # restated this and had drifted from it for Structural diversification (x = +0.30), which
  # it called harvest-neutral while Table 1 and the Methods both class it harvest-reducing
  # -- so panel b counted it in the wrong group. Deriving it means the figure cannot
  # disagree with Table 1 again. x is verified single-valued per practice and never NA.
  practice_type_map_v2 <- local({
    .a <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
    .a <- .a[as.logical(.a$is_anchor), ]
    .x <- tapply(.a$harvest_displacement, .a$practice, function(v) {
      if (length(unique(v)) != 1) stop("harvest_displacement not single-valued for ", v[1])
      v[1]
    })
    if (anyNA(.x)) stop("NA harvest_displacement for: ",
                        paste(names(.x)[is.na(.x)], collapse = ", "))
    setNames(ifelse(.x > 0, "Harvest-reducing",
             ifelse(.x == 0, "Harvest-neutral", "Supply-positive")), names(.x))
  })

  covered_practices <- union(unique(eng("../params/scheme_coverage.csv")$practice), "Agroforestry")

  # ─── Panel a: butterfly decomposition (medians, non-secondary variants) ───
  build_decomp <- function(mc_data, practices_filter = NULL) {
    # Restrict to the practices this panel covers FIRST, then require every survivor to
    # be classified. The old order put filter(!is.na(ptype)) first, which conflated a
    # deliberate exclusion (Short-rotation plantation, not scheme-covered) with an
    # unmapped practice, so a newly added practice would vanish from the panel silently.
    d <- mc_data %>% filter(as.logical(is_anchor))
    if (!is.null(practices_filter)) d <- d %>% filter(practice %in% practices_filter)
    d <- d %>% mutate(ptype = practice_type_map_v2[practice])
    if (anyNA(d$ptype))
      stop("fig3: unclassified practice(s): ",
           paste(unique(d$practice[is.na(d$ptype)]), collapse = ", "))
    d %>%
      median_draw(c("ptype", "practice", "biome", "species")) %>%
      filter(!is_secondary_variant(practice, species)) %>%
      # net_share is the median draw's own value, so it equals the MC median quoted in
      # the text and the three deltas already sum to 1 - net_share exactly. Do NOT
      # recompute it additively from the deltas.
      # delta_leak is SIGNED: >0 for harvest-reducing (a deduction), <0 for
      # supply-positive practices that add timber supply (negative leakage, a
      # value gain). Split for display only, into a left-side deduction (leak_ded)
      # and a right-side value gain (leak_gain); the split never touches net_share.
      mutate(leak_ded  = pmax(delta_leak, 0),
             leak_gain = pmax(-delta_leak, 0),
             bar_label = practice_full_label(practice, species, biome)) %>%
      arrange(desc(net_share)) %>%
      mutate(bar_label = factor(bar_label, levels = rev(bar_label)))
  }

  pbm <- build_decomp(mc_results, covered_practices)
  assert_adds_up(pbm, "fig3 panel a")
  # 'ord' is the integer position used by the plot to set the bar_label factor
  # levels (rev(bar_label) — descending net_share top-to-bottom after coord_flip).
  pbm <- pbm %>% mutate(ord = rev(seq_len(n())))

  ded_long_v2 <- pbm %>%
    select(ptype, bar_label, ord, Leakage = leak_ded, Time = delta_temp, Buffer = delta_buf) %>%
    pivot_longer(c(Leakage, Time, Buffer), names_to = "component", values_to = "share") %>%
    mutate(component_ord = match(component, c("Buffer", "Time", "Leakage")))

  # net_share already includes any negative-leakage gain; leak_gain marks the
  # right-hand slice of the net bar attributable to negative leakage (gain_start
  # = its left edge), so the drawer can highlight it. leak_gain = 0 elsewhere.
  net_bar_v2 <- pbm %>% transmute(ptype, bar_label, ord, net_share,
                                  leak_gain, gain_start = net_share - leak_gain,
                                  label = sprintf("%.0f%%", net_share * 100),
                                  label_y = net_share / 2)

  wfd(ded_long_v2, "fd_fig3_a_ded")
  wfd(net_bar_v2,  "fd_fig3_a_net")

  # ─── Panel b: net-issuance MC summary by supply class ───
  mc_typed <- mc_results %>% mutate(ptype = practice_type_map_v2[practice]) %>%
    filter(!is.na(ptype), practice %in% covered_practices)
  net_mc <- mc_typed %>%
    group_by(ptype, biome, iteration) %>% summarise(net_share = mean(net_share), .groups = "drop") %>%
    group_by(ptype, iteration) %>% summarise(net_share = mean(net_share), .groups = "drop")
  net_summ <- net_mc %>% group_by(ptype) %>%
    summarise(mean_net = mean(net_share), median_net = median(net_share),
      p5_net = quantile(net_share, .05), p25_net = quantile(net_share, .25),
      p75_net = quantile(net_share, .75), p95_net = quantile(net_share, .95), .groups = "drop") %>%
    mutate(ptype = factor(ptype, levels = c("Harvest-reducing", "Harvest-neutral", "Supply-positive"),
                          labels = c("Supply-reducing", "Supply-neutral", "Supply-positive")),
           across(c(mean_net, median_net, p5_net, p25_net, p75_net, p95_net), ~ . * 100),
           y_num = as.numeric(ptype))
  # emit the factor ordering and the plot's text/label-placement columns
  net_summ <- net_summ %>%
    mutate(ptype = as.character(ptype),
           ord = y_num,
           label = sprintf("%.0f%%  [%.0f, %.0f]", round(median_net), p5_net, p95_net),
           label_x = pmax(p95_net, mean_net) + 2)
  wfd(net_summ, "fd_fig3_b")

  # ─── Panel c: integrity-gap MC summary by scheme ───
  gap_mc <- eng("scheme_gap_mc.csv")
  forest_data <- gap_mc %>% group_by(scheme, scheme_name) %>%
    summarise(mean_gap = mean(gap), median_gap = median(gap),
      p5_gap = quantile(gap, .05), p25_gap = quantile(gap, .25),
      p75_gap = quantile(gap, .75), p95_gap = quantile(gap, .95), .groups = "drop") %>%
    mutate(scheme_name = factor(scheme_name, levels = scheme_name[order(-median_gap)]),
      across(c(mean_gap, median_gap, p5_gap, p25_gap, p75_gap, p95_gap), ~ . * 100),
      y_num = as.numeric(scheme_name))
  forest_data <- forest_data %>%
    mutate(scheme_name = as.character(scheme_name),
           ord = y_num,
           label = sprintf("%.0f%%  [%.0f, %.0f]", round(median_gap), p5_gap, p95_gap),
           label_x = pmax(p95_gap, mean_gap) + 2)
  wfd(forest_data, "fd_fig3_c")

  # ─── Panel d: parameter tornado (one driver per deduction dimension) ───
  param_labels_d <- c(k0 = "Net discount (k₀)", kappa = "Leakage intensity (κ)",
                      lambda_mult = "Disturbance (λ)")
  params_show <- c("k0", "kappa", "lambda_mult")
  tornado <- tibble(parameter = params_show,
    rho = sapply(params_show, function(p) cor(mc_results[[p]], mc_results$net_share,
                 method = "spearman", use = "complete.obs"))) %>%
    mutate(label = param_labels_d[parameter], abs_rho = abs(rho)) %>%
    arrange(abs_rho) %>% mutate(label = factor(label, levels = label))
  # ord = factor position (abs_rho ascending). rho is the only value the bar/text
  # needs; text_hjust is its sign-based label placement. Display formatting of rho
  # (sprintf "%.2f") is done in the plot because a "-0.10" string would be re-read
  # as numeric -0.1 on CSV round-trip, dropping the trailing zero.
  tornado <- tornado %>%
    mutate(ord = as.integer(label),
           label = as.character(label),
           text_hjust = ifelse(rho > 0, -0.15, 1.15))
  wfd(tornado, "fd_fig3_d")
})
