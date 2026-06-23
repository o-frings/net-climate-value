# =============================================================================
# 13_jrc.R  —  per-country comparison vs JRC/Marinelli (ed_fig_jrc_country_comparison)
# =============================================================================
# Cross-validates our country-level empirical TVaR99 buffer against the JRC CRCF
# risk model (Marinelli 2026): aggregate the JRC per-hexagon buffer-pool
# contribution (crcf_risk_bp_maps.gpkg) to country x forest_type (P10/mean/P90,
# n_hex>=5 via spatial join to country polygons), then compare to our b_TVaR99_H40.
# Reports the share of cells within the JRC P10-P90 band + median abs diff to the
# JRC mean (manuscript: 92% within band, 6.2pp). Figure rendering is the P4 layer.
# Reads only data/JRC-risk-model/ + engine/output/clean_buffer_rates.csv.
# =============================================================================
cat("[13_jrc] per-country comparison vs JRC/Marinelli...\n")

suppressPackageStartupMessages({ library(sf); library(rnaturalearth) })

# raw JRC data lives at project-root data/ (external source, not a processed extract)
jrc_gpkg <- "../data/JRC-risk-model/crcf_risk_bp_maps.gpkg"
if (!file.exists(jrc_gpkg)) stop("JRC gpkg not found: ", jrc_gpkg)

hex <- st_read(jrc_gpkg, layer = "forest_type_data", quiet = TRUE)
hex <- st_transform(hex, 3035)
hex <- hex[hex$forest_type %in% c("broadleaf", "conifer") & !is.na(hex$BP_contribution), ]

ne_aliases <- c("Bosnia and Herz." = "Bosnia and Herzegovina")
countries <- ne_countries(scale = 50, continent = "Europe", returnclass = "sf")
countries <- st_transform(countries["name"], 3035)
countries$country_root <- ifelse(countries$name %in% names(ne_aliases),
                                 ne_aliases[countries$name], countries$name)

hex_c <- st_join(hex, countries, join = st_intersects, left = FALSE)
hex_c <- st_drop_geometry(hex_c)

# our headline buffer = empirical TVaR99 H40 per country x forest_type
ours <- read.csv("engine/output/clean_buffer_rates.csv", stringsAsFactors = FALSE)
ours <- ours[, c("country", "forest_type", "b_TVaR99_H40")]
names(ours) <- c("country_root", "forest_type", "b_ours")
hex_c <- hex_c[hex_c$country_root %in% ours$country_root, ]

# JRC P10/mean/P90 per country x forest_type (drop tiny samples)
jrc <- do.call(rbind, lapply(split(hex_c, list(hex_c$country_root, hex_c$forest_type), drop = TRUE),
  function(d) {
    if (nrow(d) < 5) return(NULL)
    data.frame(country_root = d$country_root[1], forest_type = d$forest_type[1],
               n_hex = nrow(d),
               jrc_p10 = as.numeric(quantile(d$BP_contribution, 0.10)),
               jrc_mean = mean(d$BP_contribution),
               jrc_p50 = as.numeric(quantile(d$BP_contribution, 0.50)),
               jrc_p90 = as.numeric(quantile(d$BP_contribution, 0.90)),
               stringsAsFactors = FALSE)
  }))

cmp <- merge(ours, jrc, by = c("country_root", "forest_type"))
cmp$in_range <- cmp$b_ours >= cmp$jrc_p10 & cmp$b_ours <= cmp$jrc_p90
cmp$abs_diff <- abs(cmp$b_ours - cmp$jrc_mean)
cmp <- cmp[order(cmp$forest_type, cmp$jrc_mean), ]
write.csv(cmp, "engine/output/jrc_country_comparison.csv", row.names = FALSE)

cat(sprintf("[13_jrc] OK — %d countries x 2 ft = %d cells; %.0f%% within JRC P10-P90; median |diff to JRC mean| = %.1fpp\n",
            length(unique(cmp$country_root)), nrow(cmp),
            100 * mean(cmp$in_range), 100 * median(cmp$abs_diff)))
