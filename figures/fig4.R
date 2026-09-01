# =============================================================================
# figures/fig4.R — buffer solvency, two-panel. PURE PLOT (engine computes data).
# =============================================================================
# a: COUNTRY-LEVEL (per-project) required buffer rate 2025-2100 for the three
#    PURE CRCF scenarios (reducing/neutral/increasing only) x two RCPs — each
#    scenario shown as its FULL MC range (band) + median line.
# b: COUNTRY- vs EU-LEVEL: pure-scenario median buffer at the country level
#    (warm) and the cross-country EU pool (cool); the shaded gap = the
#    diversification surplus.
# Data from engine/R/figdata/fig4.R: fd_fig4_a (per-scenario %, smoothed central
# + min/max MC bounds), fd_fig4_env_a/_b (across-pure median + scenario spread),
# fd_fig4_colours, fd_fig4_annot. Run via run_figures.R (sources _setup_legacy).
# =============================================================================
local({
  has_repel <- requireNamespace("ggrepel", quietly = TRUE)

  a     <- fd("fd_fig4_a")
  cc    <- fd("fd_fig4_colours"); cols <- setNames(cc$colour, cc$scn_label)
  lv    <- cc$scn_label[order(cc$scn_ord)]; pure <- head(lv, 3)
  ann   <- fd("fd_fig4_annot"); flat <- ann$baseline_y
  env_a  <- fd("fd_fig4_env_a")   # country-level: med per (year, RCP) -> lines + surplus
  env_b  <- fd("fd_fig4_env_b")   # EU-level (per-project x pool ratio)
  band_a <- fd("fd_fig4_band_a")  # country-level spread band: per-year lo/hi (both RCPs)
  band_b <- fd("fd_fig4_band_b")  # EU-level spread band
  a$scn_label <- factor(a$scn_label, levels = lv)
  lt   <- c("RCP 4.5" = "solid", "RCP 8.5" = "dashed")
  WARM <- "#C0392B"; COOL <- "#2C7FB8"

  # ─── Panel a: country-level, pure scenarios, full-MC-range band + median ─────
  ap <- a[a$scn_label %in% pure, ]
  lab_a <- ap[ap$rcp_label == "RCP 8.5" & ap$year == max(ap$year), ]
  lab_a$short <- gsub("\n", " ", lab_a$scn_label)
  pa <- ggplot(ap, aes(year, buf_smooth, colour = scn_label,
                       fill = scn_label, linetype = rcp_label)) +
    geom_hline(yintercept = flat, linetype = "dashed", colour = NATURE_GREY, linewidth = 0.4) +
    geom_ribbon(aes(ymin = min_smooth, ymax = max_smooth,
                    group = interaction(scn_label, rcp_label)), alpha = 0.10, colour = NA) +
    geom_line(aes(group = interaction(scn_label, rcp_label)), linewidth = 0.6) +
    annotate("text", x = ann$baseline_label_x, y = flat - 1.6, label = ann$baseline_label,
             size = 2.3, colour = NATURE_GREY, hjust = 0, fontface = "italic") +
    annotate("text", x = min(ap$year), y = max(ap$max_smooth), hjust = 0, vjust = 1,
             label = "band = full MC range", size = 2.3, colour = NATURE_GREY,
             fontface = "italic") +
    scale_colour_manual(values = cols, guide = "none") +
    scale_fill_manual(values = cols, guide = "none") +
    scale_linetype_manual(values = lt, guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_continuous(breaks = seq(2030, 2100, 20), expand = expansion(mult = c(0.02, 0.18))) +
    coord_cartesian(clip = "off") +
    labs(x = "Year", y = "Required buffer rate (%)") +
    theme_nature(base_size = 9) + theme(legend.position = "none")
  if (has_repel) pa <- pa + ggrepel::geom_text_repel(
    data = lab_a, aes(label = short), hjust = 0, size = 2.4, direction = "y",
    nudge_x = 3, segment.size = 0.15, segment.colour = "#CCCCCC", seed = 42,
    min.segment.length = 0, max.overlaps = 20, show.legend = FALSE)

  # ─── Panel b: country- vs EU-level medians + diversification-surplus wedge ───
  gap <- merge(
    data.frame(year = env_a$year, rcp_label = env_a$rcp_label, top = env_a$med),
    data.frame(year = env_b$year, rcp_label = env_b$rcp_label, bot = env_b$med),
    by = c("year", "rcp_label"))
  x0   <- min(env_a$year) + 1
  # place "Country-level" clear above the rising RCP 8.5 (dashed) line over the
  # label's x-extent; "EU-level" just below the lowest EU line.
  y_cl <- max(env_a$med[env_a$rcp_label == "RCP 8.5" & env_a$year <= min(env_a$year) + 22]) + 1.8
  y_eu <- min(env_b$med[env_b$year == min(env_b$year)]) - 2.2
  pb <- ggplot() +
    geom_hline(yintercept = flat, linetype = "dashed", colour = NATURE_GREY, linewidth = 0.4) +
    geom_ribbon(data = gap, aes(year, ymin = bot, ymax = top, group = rcp_label),
                fill = "grey70", alpha = 0.16) +
    geom_ribbon(data = band_a, aes(year, ymin = lo, ymax = hi),
                fill = WARM, alpha = 0.13) +
    geom_ribbon(data = band_b, aes(year, ymin = lo, ymax = hi),
                fill = COOL, alpha = 0.13) +
    geom_line(data = env_a, aes(year, med, linetype = rcp_label), colour = WARM, linewidth = 0.7) +
    geom_line(data = env_b, aes(year, med, linetype = rcp_label), colour = COOL, linewidth = 0.7) +
    annotate("text", x = ann$baseline_label_x, y = flat + 0.9, label = ann$baseline_label,
             size = 2.3, colour = NATURE_GREY, hjust = 0, fontface = "italic") +
    annotate("text", x = x0, y = y_cl, label = "Country-level", size = 2.9,
             colour = WARM, hjust = 0, fontface = "bold") +
    annotate("text", x = x0, y = y_eu, label = "EU-level", size = 2.9,
             colour = COOL, hjust = 0, fontface = "bold") +
    annotate("text", x = 2060, y = (mean(env_a$med) + mean(env_b$med)) / 2,
             label = "Diversification\nsurplus", size = 2.5, colour = NATURE_GREY,
             fontface = "italic", lineheight = 0.9) +
    scale_linetype_manual(values = lt, guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_continuous(breaks = seq(2030, 2100, 20)) +
    labs(x = "Year", y = NULL) +
    theme_nature(base_size = 9) + theme(legend.position = "none")

  fig <- pa + pb + plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(size = 11, face = "bold"), plot.tag.position = c(0, 1))
  save_figure(fig, "fig4_buffer_solvency", width = 240, height = 108)
})
