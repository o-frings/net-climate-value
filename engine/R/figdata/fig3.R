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
  practice_type_map_v2 <- c(
    "Set-aside" = "Harvest-reducing", "Extended rotation" = "Harvest-reducing",
    "Reduced harvest intensity" = "Harvest-reducing",
    "Forested peatland rewetting" = "Harvest-reducing",
    "Coppice conversion" = "Harvest-reducing",
    "Continuous stock management" = "Harvest-reducing",
    "Structural diversification" = "Harvest-neutral",
    "Species diversification" = "Harvest-neutral",
    "Fuel management" = "Harvest-neutral", "Peatland rewetting" = "Harvest-neutral",
    "Protected afforestation" = "Harvest-neutral",
    "Productive afforestation" = "Supply-positive", "Reforestation" = "Supply-positive",
    "Site fertilisation" = "Supply-positive", "Agroforestry" = "Supply-positive")

  covered_practices <- union(unique(eng("../params/scheme_coverage.csv")$practice), "Agroforestry")

  # ─── Panel a: butterfly decomposition (medians, non-secondary variants) ───
  build_decomp <- function(mc_data, practices_filter = NULL) {
    d <- mc_data %>% mutate(ptype = practice_type_map_v2[practice]) %>% filter(!is.na(ptype))
    d <- d %>% filter(as.logical(is_anchor))   # one representative variant per practice
    if (!is.null(practices_filter)) d <- d %>% filter(practice %in% practices_filter)
    d %>%
      group_by(ptype, practice, biome, species) %>%
      summarise(net_share = median(net_share), delta_leak = median(delta_leak),
                delta_temp = median(delta_temp), delta_buf = median(delta_buf),
                .groups = "drop") %>%
      filter(!is_secondary_variant(practice, species)) %>%
      mutate(delta_leak = pmax(delta_leak, 0),
             net_share = 1 - delta_temp - delta_leak - delta_buf,
             bar_label = practice_full_label(practice, species, biome)) %>%
      arrange(desc(net_share)) %>%
      mutate(bar_label = factor(bar_label, levels = rev(bar_label)))
  }

  pbm <- build_decomp(mc_results, covered_practices)
  # 'ord' is the integer position used by the plot to set the bar_label factor
  # levels (rev(bar_label) — descending net_share top-to-bottom after coord_flip).
  pbm <- pbm %>% mutate(ord = rev(seq_len(n())))

  ded_long_v2 <- pbm %>%
    select(ptype, bar_label, ord, Leakage = delta_leak, Time = delta_temp, Buffer = delta_buf) %>%
    pivot_longer(c(Leakage, Time, Buffer), names_to = "component", values_to = "share") %>%
    mutate(component_ord = match(component, c("Buffer", "Time", "Leakage")))

  net_bar_v2 <- pbm %>% transmute(ptype, bar_label, ord, net_share,
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
