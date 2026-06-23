# =============================================================================
# 02_data_extraction.R — Re-extract external data from source files
# =============================================================================
# Extracts empirical parameters from source data:
#   Part A: Grünig et al. (2026) biome disturbance uplift factors (U_50, U_50_rcp85)
#   Part B: UNECE/FAO reference carbon stocks (S_ref)
#   Part C: Senf & Seidl (2021) base disturbance rates (lambda_obs, severity)
#
# Saves results to data/processed/ as .rds files. 03_parameters.R then loads
# these files and uses extracted values instead of hardcoded defaults.
#
# Toggle: controlled by REEXTRACT_DATA in 00_master.R (default TRUE).
# Requires: sf, rnaturalearth, rnaturalearthdata + source CSVs in data/.
# =============================================================================

cat("\n======================================================================\n")
cat("DATA RE-EXTRACTION\n")
cat("======================================================================\n")

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# #############################################################################
# PART A: GRÜNIG BIOME DISTURBANCE UPLIFT EXTRACTION
# #############################################################################
# Extract biome-level disturbance rates from Grünig et al. (2026) Dryad data.
#
# APPROACH:
# 1. Load hexagon-level disturbance rates (mean 2021-2100) for RCP2.6/4.5/8.5
# 2. Assign each hexagon to a biome via country boundaries
# 3. Compute forest-area-weighted mean disturbance rate per biome per RCP
# 4. Load continental temporal trajectory (annual, from Fig 1a)
# 5. Combine: scale biome means by the continental temporal shape
# 6. Compute agent-weighted uplift factors per biome
#
# INPUT FILES (from Dryad: doi.org/10.5061/dryad.tb2rbp0dv):
#   11_figures/figure_data/Fig2b.csv          - hexagon rates, RCP8.5
#   11_figures/figure_data/FigS5_data_map.csv - hexagon rates, RCP4.5
#   11_figures/figure_data/FigS6_data_map.csv - hexagon rates, RCP2.6
#   11_figures/figure_data/Fig1a_FigS1_fut.csv  - continental trajectory (future)
#   11_figures/figure_data/Fig1a_FigS1_hist.csv - continental trajectory (hist)
#   11_figures/figure_data/Fig3a.csv            - agent-specific temporal trajectories
#   11_figures/figure_data/FigS13-15_sd_map_*.csv - agent hexagon maps

cat("\n--- Part A: Grünig biome disturbance uplifts ---\n")

# =============================================================================
# A1. LOAD HEXAGON-LEVEL DISTURBANCE RATES
# =============================================================================
data_dir <- file.path("..", "data", "11_figures", "figure_data")

read_hex <- function(file, rcp) {
  read_csv(file, show_col_types = FALSE) %>%
    select(gridid, mean_dist_rate, forest_area, forestShare, x, y) %>%
    filter(!is.na(mean_dist_rate), forest_area > 0) %>%
    mutate(scen = rcp)
}

hex_rcp85 <- read_hex(file.path(data_dir, "Fig2b.csv"), "RCP8.5")
hex_rcp45 <- read_hex(file.path(data_dir, "FigS5_data_map.csv"), "RCP4.5")
hex_rcp26 <- read_hex(file.path(data_dir, "FigS6_data_map.csv"), "RCP2.6")

hex_all <- bind_rows(hex_rcp85, hex_rcp45, hex_rcp26)

cat("Hexagons loaded:",
    nrow(hex_rcp85), "(RCP8.5),",
    nrow(hex_rcp45), "(RCP4.5),",
    nrow(hex_rcp26), "(RCP2.6)\n")

# =============================================================================
# A2. ASSIGN HEXAGONS TO BIOMES
# =============================================================================
# Single shared country-to-biome map (used by Parts A, B, D).
# biome_3 = 3-biome classification (Boreal/Temperate/Mediterranean)
# biome_4 = 4-biome (adds Temperate_UK)
BIOME_MAP <- read.csv("data/country_biome_map.csv", stringsAsFactors = FALSE)
COUNTRY_BIOME <- setNames(BIOME_MAP$biome_3, BIOME_MAP$country)

# Get country polygons and reproject to LAEA
countries <- ne_countries(scale = 50, continent = "Europe",
                          returnclass = "sf") %>%
  select(name, iso_a2) %>%
  st_transform(3035)

countries$biome_3 <- COUNTRY_BIOME[countries$name]

# Spatial join: assign each hexagon to a country, then to a biome
hex_pts <- hex_rcp85 %>%
  select(gridid, x, y) %>%
  st_as_sf(coords = c("x", "y"), crs = 3035)

# rnaturalearth uses abbreviated names for some countries — remap to canonical
# project names so downstream country-level joins match across data sources.
ne_aliases <- c("Bosnia and Herz." = "Bosnia and Herzegovina")
hex_country <- st_join(hex_pts, countries, join = st_intersects, left = TRUE) %>%
  st_drop_geometry() %>%
  mutate(country = coalesce(ne_aliases[name], name)) %>%
  select(gridid, country, biome_3)

# Report unmatched hexagons
n_unmatched <- sum(is.na(hex_country$biome_3))
if (n_unmatched > 0) {
  cat("Warning:", n_unmatched, "hexagons not matched to a biome (ocean/border).\n")
  unmatched_countries <- hex_country %>%
    filter(is.na(biome_3), !is.na(country)) %>%
    distinct(country)
  if (nrow(unmatched_countries) > 0) {
    cat("  Unmatched countries:", paste(unmatched_countries$country, collapse = ", "), "\n")
  }
}

# Join biome and country labels back to all hexagons
hex_biome <- hex_all %>%
  left_join(hex_country %>% select(gridid, country, biome_3), by = "gridid")

cat("Biome assignment complete.\n")
cat("  Boreal:", sum(hex_biome$biome_3 == "Boreal", na.rm = TRUE) / 3, "hexagons\n")
cat("  Temperate:", sum(hex_biome$biome_3 == "Temperate", na.rm = TRUE) / 3, "hexagons\n")
cat("  Mediterranean:", sum(hex_biome$biome_3 == "Mediterranean", na.rm = TRUE) / 3, "hexagons\n")

# =============================================================================
# A3. AGGREGATE: FOREST-AREA-WEIGHTED MEAN DISTURBANCE RATE PER BIOME
# =============================================================================

biome_rates <- hex_biome %>%
  filter(!is.na(biome_3)) %>%
  group_by(scen, biome_3) %>%
  summarise(
    mean_dist_rate = weighted.mean(mean_dist_rate, forest_area, na.rm = TRUE),
    total_forest_ha = sum(forest_area, na.rm = TRUE),
    n_hex = n(),
    .groups = "drop"
  ) %>%
  arrange(scen, biome_3)

cat("\n=== BIOME-LEVEL MEAN DISTURBANCE RATES (2021-2100 average) ===\n")
print(biome_rates, n = 20)

# =============================================================================
# A4. LOAD CONTINENTAL TEMPORAL TRAJECTORY
# =============================================================================

traj_fut <- read_csv(file.path(data_dir, "Fig1a_FigS1_fut.csv"),
                     show_col_types = FALSE)
traj_hist <- read_csv(file.path(data_dir, "Fig1a_FigS1_hist.csv"),
                      show_col_types = FALSE)

hist_mean <- traj_hist %>%
  summarise(rate = mean(dist_rate, na.rm = TRUE)) %>%
  pull(rate)

cat("\nHistorical mean disturbance rate (1986-2020):", round(hist_mean, 5), "% yr-1\n")

cont_mean <- traj_fut %>%
  group_by(scen) %>%
  summarise(cont_rate = mean(mean_dist_rate, na.rm = TRUE), .groups = "drop")

# =============================================================================
# A5. COMBINE: BIOME TRAJECTORIES
# =============================================================================

biome_scaling <- biome_rates %>%
  left_join(cont_mean, by = "scen") %>%
  mutate(scale_factor = mean_dist_rate / cont_rate)

biome_trajectories <- traj_fut %>%
  select(Year, scen, mean_dist_rate, ci_low, ci_high) %>%
  rename(cont_rate = mean_dist_rate,
         cont_ci_low = ci_low,
         cont_ci_high = ci_high) %>%
  left_join(
    biome_scaling %>% select(scen, biome_3, scale_factor),
    by = "scen",
    relationship = "many-to-many"
  ) %>%
  mutate(
    biome_rate = cont_rate * scale_factor,
    biome_ci_low = cont_ci_low * scale_factor,
    biome_ci_high = cont_ci_high * scale_factor
  )

# =============================================================================
# A6. COMPUTE UPLIFT FACTORS (AGENT-WEIGHTED)
# =============================================================================
# Agent-specific continental trajectories (Fig3a: fire, beetle, wind) weighted
# by biome-level agent shares (FigS13-15 hexagon maps joined to biome via A2).
# Assumes agent shares are stationary at 2001-2020 baseline values.
# Continental trajectories apply uniformly — a known limitation, pending
# biome-specific disturbance rate data from Grünig.

cat("\n--- A6: Agent-weighted uplift factors ---\n")

# --- Agent-specific continental uplifts from Fig3a ---
fig3a <- read_csv(file.path(data_dir, "Fig3a.csv"), show_col_types = FALSE)

# Baseline period: 2001-2020 mean area per agent
baseline_areas <- fig3a %>%
  filter(period == "2001-2020") %>%
  group_by(agent) %>%
  summarise(baseline_area = mean(mean_area), .groups = "drop")

# Agent uplift at each future period, relative to baseline
agent_uplifts <- fig3a %>%
  filter(!period %in% c("1986-2000", "2001-2020")) %>%
  left_join(baseline_areas, by = "agent") %>%
  group_by(scen, agent, period) %>%
  summarise(uplift = mean(mean_area) / mean(baseline_area) - 1,
            .groups = "drop")

cat("\n=== AGENT-SPECIFIC CONTINENTAL UPLIFTS (from Fig3a) ===\n")
print(agent_uplifts %>%
        filter(scen %in% c("RCP4.5", "RCP8.5")) %>%
        mutate(uplift = round(uplift, 3)) %>%
        arrange(scen, period, agent), n = 30)

# --- Biome-level agent shares from hexagon maps (FigS13-15) ---
read_agent_hex <- function(file, agent_name) {
  read_csv(file, show_col_types = FALSE) %>%
    select(gridid, mean_dist_rate, forest_area) %>%
    filter(!is.na(mean_dist_rate), forest_area > 0) %>%
    mutate(agent = agent_name)
}

agent_hex <- bind_rows(
  read_agent_hex(file.path(data_dir, "FigS13_sd_map_fire.csv"), "Fire"),
  read_agent_hex(file.path(data_dir, "FigS14_sd_map_bbtl.csv"), "Bark beetle"),
  read_agent_hex(file.path(data_dir, "FigS15_sd_map_wind.csv"), "Wind")
)

# Join with biome assignment from A2 (hex_country)
agent_biome <- agent_hex %>%
  left_join(hex_country %>% select(gridid, biome_3), by = "gridid") %>%
  filter(!is.na(biome_3))

# Forest-area-weighted agent shares per biome
biome_agent_shares <- agent_biome %>%
  group_by(biome_3, agent) %>%
  summarise(wtd_rate = weighted.mean(mean_dist_rate, forest_area, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(biome_3) %>%
  mutate(share = wtd_rate / sum(wtd_rate)) %>%
  ungroup()

cat("\n=== BIOME AGENT SHARES (forest-area-weighted, from FigS13-15) ===\n")
print(biome_agent_shares %>%
        select(biome_3, agent, share) %>%
        mutate(share = round(share, 3)) %>%
        arrange(biome_3, agent))

# --- Compute agent-weighted biome uplift factors ---
# Map Fig3a periods to U_50 (midcentury avg) and U_100 (end-century avg)
period_to_metric <- c(
  "2021-2040" = "U_50", "2041-2060" = "U_50",
  "2061-2080" = "U_100", "2081-2100" = "U_100"
)

biome_uplift_results <- agent_uplifts %>%
  filter(scen %in% c("RCP4.5", "RCP8.5"), period %in% names(period_to_metric)) %>%
  mutate(metric = period_to_metric[period]) %>%
  # Average across periods within each metric
  group_by(scen, agent, metric) %>%
  summarise(uplift = mean(uplift), .groups = "drop") %>%
  # Weight by biome agent shares
  left_join(biome_agent_shares %>% select(biome_3, agent, share),
            by = "agent", relationship = "many-to-many") %>%
  group_by(biome_3, scen, metric) %>%
  summarise(U = sum(share * uplift), .groups = "drop") %>%
  pivot_wider(names_from = metric, values_from = U)

# U_peak: max across all future periods
biome_peak <- agent_uplifts %>%
  filter(scen %in% c("RCP4.5", "RCP8.5")) %>%
  left_join(biome_agent_shares %>% select(biome_3, agent, share),
            by = "agent", relationship = "many-to-many") %>%
  group_by(biome_3, scen, period) %>%
  summarise(U_period = sum(share * uplift), .groups = "drop") %>%
  group_by(biome_3, scen) %>%
  summarise(U_peak = max(U_period), .groups = "drop")

biome_uplift_results <- biome_uplift_results %>%
  left_join(biome_peak, by = c("biome_3", "scen"))

cat("\n=== BIOME-LEVEL UPLIFT FACTORS (agent-weighted, vs 2001-2020) ===\n")
print(biome_uplift_results %>%
        mutate(across(c(U_50, U_100, U_peak), ~round(., 3))))

# Validation: compare with continental headline from Fig1a
hist_full <- mean(traj_hist$dist_rate, na.rm = TRUE)
cat("\n=== VERIFICATION vs Grünig continental headlines ===\n")
for (s in c("RCP4.5", "RCP8.5")) {
  cont_U100 <- mean((traj_fut %>% filter(scen == s, Year > 2060))$mean_dist_rate) / hist_full - 1
  cat(sprintf("  %s continental U_100 from Fig1a: +%.0f%% (paper: +%s%%)\n",
              s, cont_U100 * 100, ifelse(s == "RCP4.5", "61", "122")))
}
biome_forest_ha <- biome_rates %>%
  filter(scen == "RCP8.5") %>% select(biome_3, total_forest_ha)

# Plot biome trajectories
p <- ggplot(biome_trajectories, aes(x = Year, y = biome_rate, colour = biome_3)) +
  geom_ribbon(aes(ymin = biome_ci_low, ymax = biome_ci_high, fill = biome_3),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~scen, ncol = 3) +
  scale_colour_manual(values = c(
    "Boreal" = "#2E86AB", "Temperate" = "#28A745", "Mediterranean" = "#FFC107"
  )) +
  scale_fill_manual(values = c(
    "Boreal" = "#2E86AB", "Temperate" = "#28A745", "Mediterranean" = "#FFC107"
  )) +
  geom_hline(yintercept = hist_mean, linetype = "dashed", colour = "grey50") +
  annotate("text", x = 2025, y = hist_mean + 0.005, label = "Historical mean",
           hjust = 0, size = 3, colour = "grey50") +
  labs(
    title = "Projected biome-level disturbance rates (Grünig et al. 2026)",
    subtitle = "Biome means from hexagon data × continental temporal shape",
    x = "Year", y = "Disturbance rate (% yr-1)",
    colour = "Biome", fill = "Biome"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/gruenig_biome_trajectories.pdf", p, width = 12, height = 5)
cat("\nSaved: output/figures/gruenig_biome_trajectories.pdf\n")

# Save uplift factors
gruenig_uplifts <- biome_uplift_results %>%
  select(biome_3, scen, U_50, U_100, U_peak)
saveRDS(gruenig_uplifts, "data/processed/gruenig_uplift_factors.rds")
cat("Saved: data/processed/gruenig_uplift_factors.rds\n")

# =============================================================================
# A7. COUNTRY-LEVEL UPLIFTS (mirrors A3-A6 logic at country granularity)
# =============================================================================
# Hybrid country-level model: each country uses its own λ_obs (EFDA) and own
# U_50/U_100/U_peak (Grünig hexagons aggregated by country). Within-biome
# variation in projected uplift is comparable in magnitude to between-biome
# variation, so biome-mean fallback would erase ~half the spatial signal.
# Countries with <3 hexagons fall back to their biome mean.

cat("\n--- A7: Country-level uplift factors ---\n")

country_rates <- hex_biome %>%
  filter(!is.na(biome_3), !is.na(country)) %>%
  group_by(scen, country, biome_3) %>%
  summarise(
    mean_dist_rate = weighted.mean(mean_dist_rate, forest_area, na.rm = TRUE),
    n_hex = n(),
    .groups = "drop"
  )

# Country-level agent shares
country_agent_shares <- agent_hex %>%
  left_join(hex_country, by = "gridid") %>%
  filter(!is.na(country)) %>%
  group_by(country, agent) %>%
  summarise(wtd_rate = weighted.mean(mean_dist_rate, forest_area, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(country) %>%
  mutate(share = wtd_rate / sum(wtd_rate)) %>%
  ungroup()

# Agent-weighted country uplifts
country_uplift_results <- agent_uplifts %>%
  filter(scen %in% c("RCP4.5", "RCP8.5"), period %in% names(period_to_metric)) %>%
  mutate(metric = period_to_metric[period]) %>%
  group_by(scen, agent, metric) %>%
  summarise(uplift = mean(uplift), .groups = "drop") %>%
  left_join(country_agent_shares %>% select(country, agent, share),
            by = "agent", relationship = "many-to-many") %>%
  group_by(country, scen, metric) %>%
  summarise(U = sum(share * uplift), .groups = "drop") %>%
  pivot_wider(names_from = metric, values_from = U)

# U_peak per country
country_peak <- agent_uplifts %>%
  filter(scen %in% c("RCP4.5", "RCP8.5")) %>%
  left_join(country_agent_shares %>% select(country, agent, share),
            by = "agent", relationship = "many-to-many") %>%
  group_by(country, scen, period) %>%
  summarise(U_period = sum(share * uplift), .groups = "drop") %>%
  group_by(country, scen) %>%
  summarise(U_peak = max(U_period), .groups = "drop")

country_uplift_results <- country_uplift_results %>%
  left_join(country_peak, by = c("country", "scen")) %>%
  left_join(country_rates %>% distinct(country, biome_3, scen, n_hex),
            by = c("country", "scen"))

# Apply biome-mean fallback for countries with too few hexagons
sparse_threshold <- 3
sparse_countries <- country_uplift_results %>%
  filter(n_hex < sparse_threshold) %>%
  distinct(country) %>%
  pull(country)

if (length(sparse_countries) > 0) {
  cat(sprintf("  Biome-fallback for sparse countries (<%d hex): %s\n",
              sparse_threshold, paste(sparse_countries, collapse = ", ")))
  fallback <- biome_uplift_results %>%
    select(biome_3, scen, b_U_50 = U_50, b_U_100 = U_100, b_U_peak = U_peak)
  country_uplift_results <- country_uplift_results %>%
    left_join(fallback, by = c("biome_3", "scen")) %>%
    mutate(
      U_50   = ifelse(n_hex < sparse_threshold, b_U_50,   U_50),
      U_100  = ifelse(n_hex < sparse_threshold, b_U_100,  U_100),
      U_peak = ifelse(n_hex < sparse_threshold, b_U_peak, U_peak)
    ) %>%
    select(-b_U_50, -b_U_100, -b_U_peak)
}

cat("\n=== COUNTRY-LEVEL UPLIFT FACTORS (top 10 by forest area, RCP4.5) ===\n")
print(country_uplift_results %>%
        filter(scen == "RCP4.5") %>%
        left_join(country_rates %>% filter(scen == "RCP4.5") %>%
                    select(country, n_hex_check = n_hex), by = "country") %>%
        arrange(desc(n_hex)) %>%
        head(15) %>%
        mutate(across(c(U_50, U_100, U_peak), ~round(., 2))) %>%
        select(country, biome_3, n_hex, U_50, U_100, U_peak))

gruenig_country_uplifts <- country_uplift_results %>%
  select(country, biome_3, scen, U_50, U_100, U_peak, n_hex)
saveRDS(gruenig_country_uplifts,
        "data/processed/gruenig_country_uplift_factors.rds")
cat("Saved: data/processed/gruenig_country_uplift_factors.rds (",
    nrow(gruenig_country_uplifts), "rows )\n")


# #############################################################################
# PART B: UNECE/FAO S_REF DERIVATION
# #############################################################################
# AGB source: UNECE SDG Indicator 15.2.1(a) (t DM / ha)
#   URL: https://w3.unece.org/SDG/en/Indicator?id=177
# Forest area source: FAO FRA 2020 (1000 ha)
#   URL: https://data.apps.fao.org/catalog/dataset/forest-area-1990-2020-1000-ha

cat("\n--- Part B: UNECE/FAO S_ref derivation ---\n")

# B1. Load UNECE AGB data
agb <- read_csv("../data/UNECE_forest_biomass_data.csv", skip = 1,
                col_types = cols(
                  Country_ID = col_character(),
                  Country_E = col_character(),
                  Indicator_Id = col_character(),
                  Indicator_E = col_character(),
                  Period = col_integer(),
                  Value = col_double()
                ))

agb_latest <- agb %>%
  group_by(Country_E) %>%
  filter(Period == max(Period)) %>%
  ungroup() %>%
  select(country_agb = Country_E, agb_t_ha = Value)

# B2. Load FAO FRA 2020 forest area
fra_raw <- read_csv("../data/FAO_FRA2020_forest_area.csv",
                    col_types = cols(.default = col_character()))

fra <- fra_raw %>%
  select(country_fra = 1, area_2020 = last_col()) %>%
  slice(-1) %>%
  filter(!is.na(area_2020), area_2020 != "") %>%
  mutate(forest_area_kha = as.numeric(area_2020)) %>%
  select(country_fra, forest_area_kha)

# B3. Biome-country mapping (from shared CSV; biome_4 for UK separation)
biome_map_raw <- BIOME_MAP[!is.na(BIOME_MAP$unece_name) & BIOME_MAP$unece_name != "", ]
biome_map <- data.frame(
  country_agb = biome_map_raw$country,
  country_fra = biome_map_raw$unece_name,
  biome = biome_map_raw$biome_4,
  stringsAsFactors = FALSE
)
rm(biome_map_raw)

# B4. Join AGB + forest area
joined <- biome_map %>%
  left_join(agb_latest, by = "country_agb") %>%
  left_join(fra, by = "country_fra")

cat("\n-- Missing data (NAs) --\n")
joined %>%
  filter(is.na(agb_t_ha) | is.na(forest_area_kha)) %>%
  select(country_agb, biome, agb_t_ha, forest_area_kha) %>%
  print()

# B5. Compute area-weighted AGB per biome
biome_agb <- joined %>%
  filter(!is.na(agb_t_ha), !is.na(forest_area_kha)) %>%
  group_by(biome) %>%
  summarise(
    n = n(),
    forest_kha = sum(forest_area_kha),
    agb_wt = sum(agb_t_ha * forest_area_kha) / sum(forest_area_kha),
    agb_min = min(agb_t_ha),
    agb_max = max(agb_t_ha),
    .groups = "drop"
  )

cat("\n-- Biome AGB, area-weighted (t DM / ha) --\n")
print(biome_agb)

# B6. Convert to S_ref in tCO2/ha
cf <- 0.47
co2_c <- 44 / 12

sref_result <- biome_agb %>%
  mutate(S_ref = round(agb_wt * cf * co2_c))

cat("\n== S_ref results (AGB x 0.47 x 3.67 = tCO2/ha) ==\n")
sref_result %>%
  select(biome, n, forest_kha, agb_wt, S_ref) %>%
  print()

# B7. Country detail
cat("\n-- Country detail --\n")
joined %>%
  filter(!is.na(agb_t_ha), !is.na(forest_area_kha)) %>%
  mutate(tco2 = round(agb_t_ha * cf * co2_c, 1)) %>%
  group_by(biome) %>%
  mutate(wt_pct = round(100 * forest_area_kha / sum(forest_area_kha), 1)) %>%
  ungroup() %>%
  arrange(biome, desc(forest_area_kha)) %>%
  select(biome, country = country_agb, forest_area_kha, wt_pct, agb_t_ha, tco2) %>%
  print(n = 40)

# B8. Save S_ref values
sref_biome <- sref_result %>%
  select(biome, S_ref, agb_wt)
saveRDS(sref_biome, "data/processed/sref_biome.rds")
cat("Saved: data/processed/sref_biome.rds\n")


# #############################################################################
# PART C: SENF & SEIDL (2021) BASE DISTURBANCE RATES
# #############################################################################
# Derives biome-level base disturbance rates (lambda_obs) from
# Supplementary Table 3 (country-level frequency, size, severity).
#
# Formula: lambda = frequency × mean_patch_size / 100
#   - frequency: disturbance patches per km² forest area per year
#   - mean_patch_size: mean disturbed patch size (ha)
#   - /100: converts ha/km² to fraction of forest area
#
# Aggregated per biome using national forest area weights
# (FAO FRA 2020, loaded in Part B).
#
# INPUT: ../references/Senf & Seidl (2021)*.csv
# #############################################################################

cat("\n--- Part C: Senf & Seidl (2021) base disturbance rates ---\n")

# C1. Load Supplementary Table 3 CSV
senf_csv <- list.files("../references", pattern = "Senf.*csv$",
                        full.names = TRUE)
if (length(senf_csv) == 0) stop("Senf & Seidl CSV not found in ../references/")

senf_raw <- read_csv(senf_csv[1], show_col_types = FALSE)

# Pivot to wide: one row per country with Frequency, Size, Severity columns
senf_wide <- senf_raw %>%
  select(Indicator, Country, Mean) %>%
  pivot_wider(names_from = Indicator, values_from = Mean) %>%
  rename(country = Country, freq = Frequency, size = Size, severity = Severity)

cat("  Loaded", nrow(senf_wide), "countries from Senf & Seidl Suppl. Table 3\n")

# C2. Country-to-biome mapping (from shared CSV; biome_4 for UK separation)
senf_biome_map <- BIOME_MAP[, c("country", "biome_4")]
names(senf_biome_map) <- c("country", "biome")

# C3. Join with forest area from Part B (FAO FRA 2020)
# Re-use biome_map from Part B for forest area, or load FRA directly
senf_joined <- senf_wide %>%
  inner_join(senf_biome_map, by = "country") %>%
  left_join(
    joined %>% select(country_agb, forest_area_kha),
    by = c("country" = "country_agb")
  ) %>%
  filter(!is.na(forest_area_kha))

# C4. Compute lambda per country and area-weighted mean per biome
senf_joined <- senf_joined %>%
  mutate(lambda = freq * size / 100)

senf_biome_rates <- senf_joined %>%
  group_by(biome) %>%
  summarise(
    n_countries   = n(),
    forest_kha    = sum(forest_area_kha),
    lambda_obs    = weighted.mean(lambda, forest_area_kha),
    severity      = weighted.mean(severity, forest_area_kha),
    .groups = "drop"
  )

cat("\n=== BIOME-LEVEL BASE DISTURBANCE RATES (Senf & Seidl 2021) ===\n")
senf_biome_rates %>%
  mutate(lambda_obs = round(lambda_obs, 5),
         severity = round(severity, 2)) %>%
  print()

# C5. Country detail
cat("\n-- Country detail --\n")
senf_joined %>%
  arrange(biome, desc(forest_area_kha)) %>%
  mutate(
    weight_pct = round(100 * forest_area_kha /
                          ave(forest_area_kha, biome, FUN = sum), 1),
    lambda = round(lambda, 5)
  ) %>%
  select(biome, country, forest_area_kha, weight_pct, freq, size, severity, lambda) %>%
  print(n = 40)

# C6. Save
saveRDS(senf_biome_rates, "data/processed/senf_biome_rates.rds")
cat("Saved: data/processed/senf_biome_rates.rds\n")


# NB: Chiti et al. (2026) rates are sourced from data/Chiti_et_al_2026_Table1.csv
# (per-study values transcribed from paper Table 1) and loaded directly in
# 03_parameters.R. No extraction needed here.


# #############################################################################
# PART D: BERKELEY VROD — CALIFORNIA ARB FOREST OFFSET STATISTICS
# #############################################################################
# Extracts CA ARB forest project statistics from the Berkeley Carbon Trading
# Project Voluntary Registry Offsets Database (VROD).
#
# INPUT: data/VROD-v2025-12.xlsx (PROJECTS sheet)
# SOURCE: Haya, So & Elias (2025), doi.org/10.5281/zenodo.15421078
#
# Extracts:
#   - Project counts by type (IFM, Avoided Conversion, Reforestation)
#   - Credit volumes and shares
#   - Buffer pool health (deposits, reversals covered/uncovered)
#   - Reversal frequency
#   - Effective buffer rates by project type
#   - Yearly issuance trajectory
# #############################################################################

cat("\n--- Part D: Berkeley VROD — CA ARB forest statistics ---\n")

library(readxl)

vrod_path <- "../data/VROD-v2025-12.xlsx"
if (!file.exists(vrod_path)) {
  cat("  VROD file not found at", vrod_path, "— skipping Part D.\n")
} else {

  # D1. Load PROJECTS sheet (headers in row 4, data from row 5)
  vrod <- read_xlsx(vrod_path, sheet = "PROJECTS", skip = 3,
                    col_names = TRUE, .name_repair = "unique_quiet")

  # Standardise column names (readxl may append ...N for duplicates)
  names(vrod)[1:22] <- c(
    "project_id", "project_name", "registry", "arb_wa", "status",
    "scope", "type", "red_rem", "methodology", "meth_version",
    "region", "country", "state", "location", "developer",
    "credits_issued", "credits_retired", "credits_remaining",
    "buffer_deposits", "reversals_covered", "reversals_not_covered",
    "buffer_released"
  )

  cat("  VROD loaded:", nrow(vrod), "projects total\n")

  # D2. Filter to ARB forest projects
  arb_forest <- vrod %>%
    filter(grepl("ARB", arb_wa, ignore.case = TRUE),
           grepl("Forest", scope, ignore.case = TRUE)) %>%
    mutate(across(c(credits_issued, credits_retired, credits_remaining,
                    buffer_deposits, reversals_covered,
                    reversals_not_covered, buffer_released),
                  ~replace_na(as.numeric(.), 0)))

  cat("  ARB forest projects:", nrow(arb_forest), "\n")

  # D3. Project counts by type
  type_summary <- arb_forest %>%
    group_by(type) %>%
    summarise(
      n_projects      = n(),
      credits_issued  = sum(credits_issued),
      buffer_deposits = sum(buffer_deposits),
      gross_credits   = sum(credits_issued + buffer_deposits),
      .groups = "drop"
    ) %>%
    mutate(
      credit_share = credits_issued / sum(credits_issued),
      eff_buffer_rate = buffer_deposits / gross_credits
    ) %>%
    arrange(desc(credits_issued))

  cat("\n=== CA ARB FOREST PROJECTS BY TYPE ===\n")
  print(type_summary %>%
          mutate(credits_M = round(credits_issued / 1e6, 1),
                 credit_share = round(credit_share, 3),
                 eff_buffer_rate = round(eff_buffer_rate, 3)) %>%
          select(type, n_projects, credits_M, credit_share, eff_buffer_rate))

  # D4. Buffer pool health
  total_issued       <- sum(arb_forest$credits_issued)
  total_buffer       <- sum(arb_forest$buffer_deposits)
  total_rev_covered  <- sum(arb_forest$reversals_covered)
  total_rev_uncov    <- sum(arb_forest$reversals_not_covered)
  total_reversals    <- total_rev_covered + total_rev_uncov
  gross_credits      <- total_issued + total_buffer

  buffer_stats <- tibble(
    metric = c("Gross credits generated", "Credits issued (tradable)",
               "Buffer deposits", "Effective buffer rate",
               "Total reversals", "Reversals covered by buffer",
               "Reversals NOT covered", "Buffer shortfall (% of reversals)",
               "Reversal rate (% of gross)", "Buffer surplus"),
    value = c(
      sprintf("%.1fM", gross_credits / 1e6),
      sprintf("%.1fM", total_issued / 1e6),
      sprintf("%.1fM", total_buffer / 1e6),
      sprintf("%.1f%%", total_buffer / gross_credits * 100),
      sprintf("%.1fM", total_reversals / 1e6),
      sprintf("%.1fM", total_rev_covered / 1e6),
      sprintf("%.1fM", total_rev_uncov / 1e6),
      sprintf("%.0f%%", total_rev_uncov / total_reversals * 100),
      sprintf("%.1f%%", total_reversals / gross_credits * 100),
      sprintf("%+.1fM", (total_buffer - total_reversals) / 1e6)
    )
  )

  cat("\n=== BUFFER POOL HEALTH ===\n")
  print(buffer_stats, n = Inf)

  # D5. Reversal frequency
  rev_projects <- arb_forest %>%
    filter(reversals_covered > 0 | reversals_not_covered > 0)
  n_ifm <- sum(arb_forest$type == "Improved Forest Management")
  n_ifm_rev <- sum(rev_projects$type == "Improved Forest Management")

  cat(sprintf("\n=== REVERSAL FREQUENCY ===\n"))
  cat(sprintf("  Projects with reversals: %d/%d (%.0f%%)\n",
              nrow(rev_projects), nrow(arb_forest),
              nrow(rev_projects) / nrow(arb_forest) * 100))
  cat(sprintf("  IFM with reversals:      %d/%d (%.0f%%)\n",
              n_ifm_rev, n_ifm, n_ifm_rev / n_ifm * 100))

  # D6. Top reversed projects
  rev_detail <- rev_projects %>%
    mutate(total_rev = reversals_covered + reversals_not_covered) %>%
    arrange(desc(total_rev)) %>%
    select(project_id, type, state, total_rev,
           reversals_covered, reversals_not_covered)

  cat("\n=== TOP 10 REVERSED PROJECTS ===\n")
  print(rev_detail %>%
          head(10) %>%
          mutate(across(c(total_rev, reversals_covered, reversals_not_covered),
                        ~round(. / 1e6, 2))), n = 10)

  # D7. Yearly issuance (columns 24-53 = vintages 1996-2025)
  # Column names after unique_quiet repair are "1996...24" through "2025...53"
  vintage_cols <- names(vrod)[24:53]
  vintage_years <- 1996:2025
  yearly_issuance <- arb_forest %>%
    select(all_of(vintage_cols)) %>%
    mutate(across(everything(), ~replace_na(as.numeric(.), 0))) %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(everything(), names_to = "col", values_to = "credits") %>%
    mutate(year = vintage_years) %>%
    select(year, credits) %>%
    filter(credits > 0)

  cat("\n=== YEARLY ISSUANCE (forest credits) ===\n")
  print(yearly_issuance %>%
          mutate(credits_M = round(credits / 1e6, 1)) %>%
          select(year, credits_M), n = 30)

  # D8. Geographic concentration
  state_summary <- arb_forest %>%
    group_by(state) %>%
    summarise(n = n(), credits = sum(credits_issued), .groups = "drop") %>%
    arrange(desc(credits)) %>%
    head(10)

  cat("\n=== TOP 10 STATES ===\n")
  print(state_summary %>%
          mutate(credits_M = round(credits / 1e6, 1)) %>%
          select(state, n, credits_M))

  # D9. Save extracted statistics
  vrod_stats <- list(
    n_projects = nrow(arb_forest),
    type_summary = type_summary,
    buffer = list(
      gross_credits    = gross_credits,
      credits_issued   = total_issued,
      buffer_deposits  = total_buffer,
      eff_buffer_rate  = total_buffer / gross_credits,
      total_reversals  = total_reversals,
      rev_covered      = total_rev_covered,
      rev_uncovered    = total_rev_uncov,
      shortfall_pct    = total_rev_uncov / total_reversals,
      reversal_rate    = total_reversals / gross_credits
    ),
    reversals = list(
      n_projects_with_rev = nrow(rev_projects),
      rev_frequency = nrow(rev_projects) / nrow(arb_forest),
      ifm_rev_frequency = n_ifm_rev / n_ifm
    ),
    yearly_issuance = yearly_issuance,
    state_summary = state_summary
  )

  saveRDS(vrod_stats, "data/processed/vrod_ca_arb_stats.rds")
  cat("\nSaved: data/processed/vrod_ca_arb_stats.rds\n")

  # D10. Verify against hardcoded parameters in 03_parameters.R / scheme_parameters.csv
  cat("\n=== VERIFICATION vs SCHEME_PARAMETERS.CSV ===\n")
  scheme_csv <- read.csv("data/external/scheme_parameters.csv",
                         stringsAsFactors = FALSE)
  ca_rows <- scheme_csv %>% filter(scheme_id == "CA_USFP")

  check <- function(label, extracted, expected, tol = 0.01) {
    match <- abs(extracted - expected) < tol
    cat(sprintf("  %-30s extracted=%-10s expected=%-10s %s\n",
                label,
                format(round(extracted, 4), nsmall = 4),
                format(round(expected, 4), nsmall = 4),
                ifelse(match, "OK", "*** MISMATCH ***")))
  }

  csv_n <- as.numeric(ca_rows$value[ca_rows$param == "n_projects"])
  csv_ifm <- as.numeric(ca_rows$value[ca_rows$param == "practice_share_ifm"])

  ifm_row <- type_summary %>% filter(type == "Improved Forest Management")

  check("n_projects (forest)",
        nrow(arb_forest), csv_n)
  check("IFM credit share",
        ifm_row$credit_share, csv_ifm)
  check("Effective buffer rate (overall)",
        total_buffer / gross_credits,
        as.numeric(ca_rows$value[ca_rows$param == "buffer_rate"]),
        tol = 0.05)

  cat("\n  Note: CARB protocol buffer rate = 13.4% (Badgley avg);\n")
  cat("        VROD effective rate = ", round(total_buffer / gross_credits * 100, 1),
      "% (deposits / gross)\n")
}


# #############################################################################
# PART E: EFDA per-country summary (hybrid country-level model input)
# #############################################################################
# Builds data/processed/efda_country_summary.rds from per-country annual
# disturbance series in data/processed/efda_country_rates/*.rds. Those .rds
# files are produced offline from the European Forest Disturbance Atlas
# (Viana-Soto & Senf 2025, Zenodo doi.org/10.5281/zenodo.13333034) using
# zonal aggregation per country and per (country × bioregion) for France
# and Italy. See docs/efda_extraction.md for the workflow.
#
# Output schema mirrors senf_biome_rates.rds plus zone_label/sub_zone for
# bioregion splits — drop-in for COUNTRY_PARAMS in 03_parameters.R.
# #############################################################################

cat("\n--- Part E: EFDA per-country summary ---\n")

efda_dir <- file.path("data", "processed", "efda_country_rates")
senf_csv <- list.files("../references", pattern = "Senf.*csv$", full.names = TRUE)

if (!dir.exists(efda_dir) ||
    length(list.files(efda_dir, "\\.rds$")) == 0) {
  cat("  SKIP: no EFDA per-country files in", efda_dir, "\n")
} else if (length(senf_csv) == 0) {
  cat("  SKIP: Senf CSV not found in ../references/\n")
} else {

  # 1. Senf severity, mapping renamed countries via senf_alias column
  senf_aliases <- BIOME_MAP |>
    select(country, senf_alias) |>
    filter(!is.na(senf_alias))
  senf_sev <- read_csv(senf_csv[1], show_col_types = FALSE) |>
    filter(Indicator == "Severity") |>
    select(senf_country = Country, severity = Mean) |>
    left_join(senf_aliases, by = c("senf_country" = "senf_alias")) |>
    mutate(country = coalesce(country, senf_country)) |>
    select(country, severity)

  # 2. Per-zone EFDA aggregates, parsing filename (canonical name from BIOME_MAP)
  per_zone <- list.files(efda_dir, "\\.rds$", full.names = TRUE) |>
    lapply(function(f) {
      ann   <- readRDS(f)
      key   <- sub("\\.rds$", "", basename(f))
      parts <- strsplit(key, "_", fixed = TRUE)[[1]]
      file_country <- parts[1]
      sub_zone     <- if (length(parts) > 1) parts[2] else NA_character_
      cn <- BIOME_MAP$country[BIOME_MAP$efda_filename == file_country][1]
      if (is.na(cn)) {warning("Unmapped EFDA file: ", file_country); return(NULL)}
      tibble(
        zone_label   = if (!is.na(sub_zone)) paste(cn, sub_zone, sep = "_") else cn,
        country_root = cn, sub_zone = sub_zone,
        biome = if (!is.na(sub_zone)) sub_zone else
                  BIOME_MAP$biome_4[BIOME_MAP$country == cn][1],
        lambda_full     = mean(ann$lambda_natural[ann$year >= 1986]),
        lambda_pre2018  = mean(ann$lambda_natural[ann$year >= 1986 &
                                                  ann$year <= 2017]),
        lambda_post2018 = mean(ann$lambda_natural[ann$year >= 2018]),
        forest_kha      = head(ann$forest_ha, 1) / 1000  # hectares -> kha
      )
    }) |>
    bind_rows() |>
    filter(!is.na(biome))

  # 3. Drop whole-country rows where sub-zone splits exist
  split_roots <- unique(per_zone$country_root[!is.na(per_zone$sub_zone)])
  per_zone <- per_zone |>
    filter(!(country_root %in% split_roots & is.na(sub_zone)))

  # 4. Join Senf severity (senf_alias mapping covers all EFDA countries; if a
  # country is added later without a Senf match, this will produce NA and the
  # missing entry will be visible downstream — preferable to a silent biome
  # fallback that hides the gap)
  per_zone <- per_zone |>
    left_join(senf_sev, by = c("country_root" = "country"))
  if (any(is.na(per_zone$severity)))
    warning("Senf severity missing for: ",
            paste(per_zone$country_root[is.na(per_zone$severity)],
                  collapse = ", "))

  saveRDS(per_zone, "data/processed/efda_country_summary.rds")
  write_csv(per_zone, "data/processed/efda_country_summary.csv")
  cat(sprintf("  Saved efda_country_summary.rds (%d zones, %d countries + %d sub-national)\n",
              nrow(per_zone),
              sum(is.na(per_zone$sub_zone)),
              sum(!is.na(per_zone$sub_zone))))
}


# #############################################################################
# VERIFY OUTPUTS
# #############################################################################

if (!file.exists("data/processed/gruenig_uplift_factors.rds"))
  stop("Extraction failed: gruenig_uplift_factors.rds not created")
if (!file.exists("data/processed/sref_biome.rds"))
  stop("Extraction failed: sref_biome.rds not created")
if (!file.exists("data/processed/senf_biome_rates.rds"))
  stop("Extraction failed: senf_biome_rates.rds not created")

cat("\n======================================================================\n")
cat("DATA RE-EXTRACTION COMPLETE\n")
cat("  - data/processed/gruenig_uplift_factors.rds (U_50, U_50_rcp85 per biome)\n")
cat("  - data/processed/gruenig_country_uplift_factors.rds (per country)\n")
cat("  - data/processed/sref_biome.rds (S_ref per biome)\n")
cat("  - data/processed/senf_biome_rates.rds (lambda_obs, severity per biome)\n")
if (file.exists("data/processed/efda_country_summary.rds"))
  cat("  - data/processed/efda_country_summary.rds (per country, EFDA-derived)\n")
if (file.exists("data/processed/vrod_ca_arb_stats.rds"))
  cat("  - data/processed/vrod_ca_arb_stats.rds (CA ARB forest offset stats)\n")
cat("03_parameters.R will load these values next.\n")
cat("======================================================================\n")
