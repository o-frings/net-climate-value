# =============================================================================
# figures/ed_country_disturbance.R — country-level EFDA disturbance maps (ED),
# faithful port of legacy 22_efda_country_figures.R, engine data.
# =============================================================================
# a: forest-area-weighted mean natural-disturbance rate 1986-2023 (% yr-1)
#    choropleth of Europe; b: post-2018 elevation ratio (2018-2023 vs 1986-2017)
#    choropleth. Aesthetic is the validated legacy design; values are the
#    engine's country_disturbance.csv. Run via run_figures.R (which sources
#    _setup_legacy.R first). Skips cleanly if sf/rnaturalearth are unavailable.
# =============================================================================

local({
  ok <- requireNamespace("sf", quietly = TRUE) &&
        requireNamespace("rnaturalearth", quietly = TRUE)
  if (!ok) {
    cat("  SKIP ed_fig_country_disturbance: sf/rnaturalearth unavailable\n")
    return(invisible(NULL))
  }
  suppressPackageStartupMessages({ library(sf); library(patchwork) })

  # ─── Plot-ready country table from the engine (all derived columns precomputed) ───
  country_data <- fd("fd_ed_country_disturbance")

  # rnaturalearth uses abbreviated short names for some countries — remap to
  # the canonical project names before the join.
  ne_aliases <- c("Bosnia and Herz." = "Bosnia and Herzegovina",
                  "Czech Rep."       = "Czechia")

  eu_countries <- rnaturalearth::ne_countries(scale = 50, continent = "Europe",
                                              returnclass = "sf") |>
    st_transform(3035)
  eu_countries <- eu_countries["name"]
  eu_countries$name <- ifelse(eu_countries$name %in% names(ne_aliases),
                              ne_aliases[eu_countries$name], eu_countries$name)
  eu_countries <- eu_countries[eu_countries$name %in% country_data$country, ]

  map_data <- merge(eu_countries, country_data,
                    by.x = "name", by.y = "country", all.x = TRUE)

  bbox <- st_bbox(c(xmin = 2.5e6, xmax = 6.5e6, ymin = 1.4e6, ymax = 5.5e6),
                  crs = st_crs(3035))

  # ─── Shared narrow vertical colourbar guide ───
  cbar_guide <- guide_colourbar(
    barwidth        = unit(2.5, "mm"),
    barheight       = unit(35, "mm"),
    title.position  = "top",
    title.hjust     = 0,
    ticks.colour    = NATURE_GREY,
    ticks.linewidth = 0.4,
    frame.colour    = NA,
    draw.ulim       = FALSE,
    draw.llim       = FALSE
  )

  # ─── Clean map theme (blanked axes/grid, bold strip/title, grey legend) ───
  theme_efda_map <- function() {
    theme_nature(base_size = 8) +
      theme(
        axis.text          = element_blank(),
        axis.ticks         = element_blank(),
        axis.title         = element_blank(),
        axis.line          = element_blank(),
        panel.grid         = element_blank(),
        strip.background   = element_blank(),
        strip.text         = element_text(face = "bold", size = rel(1),
                                          margin = margin(b = 4)),
        plot.title         = element_text(face = "bold", size = rel(1),
                                          hjust = 0, margin = margin(b = 4)),
        legend.position    = "right",
        legend.title       = element_text(face = "plain", size = rel(0.85),
                                          colour = NATURE_GREY, lineheight = 1.0,
                                          margin = margin(b = 4)),
        legend.text        = element_text(size = rel(0.8), colour = NATURE_GREY),
        legend.margin      = margin(0, 0, 0, 0),
        legend.box.spacing = unit(4, "pt"),
        plot.margin        = margin(2, 2, 2, 2)
      )
  }

  # ─── Helper: a single choropleth panel (diverging blue-beige-red) ───
  efda_map <- function(fill_expr, name, midpoint, limits, breaks, labels, title) {
    ggplot(map_data) +
      geom_sf(aes(fill = {{ fill_expr }}), colour = "white", linewidth = 0.2) +
      scale_fill_gradient2(
        name     = name,
        low      = "#BDD7EE", mid = "#E8D5C4", high = "#C0392B",
        midpoint = midpoint, limits = limits, breaks = breaks, labels = labels,
        guide    = cbar_guide, na.value = "#EEEEEE"
      ) +
      coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
               ylim = c(bbox["ymin"], bbox["ymax"])) +
      theme_efda_map() +
      labs(title = title)
  }

  p_full <- efda_map(
    fill_expr = lambda_full_pct,
    name      = "Natural\ndisturbance\n(% yr⁻¹)",
    midpoint  = 0.5, limits = c(0, 1.5), breaks = seq(0, 1.5, 0.25),
    labels    = function(x) sprintf("%.2f", x),
    title     = "1986-2023 mean")

  p_elev <- efda_map(
    fill_expr = elevation_ratio_capped,
    name      = "2018-2023\nvs 1986-2017\n(× ratio)",
    midpoint  = 1.0, limits = c(0, 6), breaks = c(0, 1, 2, 3, 4, 6),
    labels    = function(x) sprintf("%.0fx", x),
    title     = "Post-2018 elevation")

  fig <- (p_full | p_elev) +
    plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(face = "bold", size = 10,
                                  margin = margin(r = 4, b = 4)))

  save_figure(fig, "ed_fig_country_disturbance", width = 220, height = 130)
})
