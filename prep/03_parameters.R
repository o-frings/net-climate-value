# =============================================================================
# 03_parameters.R - Model Parameters (Empirically Calibrated)
# =============================================================================
# All parameters calibrated from peer-reviewed literature.
# Full references in BIBLIOGRAPHY section at end of file.
# =============================================================================

cat("Loading model parameters...\n")

# PATHS
# =============================================================================
PATHS <- list(
  data_processed = "data/processed",
  output_tables = "output/tables",
  output_figures = "output/figures",
  output_results = "output/results"
)
# Legacy extraction/param scripts write scratch outputs here; recreate on run so
# the directory can be archived without breaking reproducibility.
for (.d in c(PATHS$output_tables, PATHS$output_figures, PATHS$output_results))
  dir.create(.d, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# GLOBAL PARAMETERS
# =============================================================================
GLOBAL_PARAMS <- list(
  # --- Discount rate (r) ---
  # Arrow et al. (2013) recommend ~3% near-term declining; Drupp et al. (2018)
  # expert survey median 2% (IQR 1-3%); Groom & Venmans (2023) use 3.2%.
  # Sensitivity range: Stern (2007) 1.4% to Nordhaus (2017) 4.25%.
  r = 0.03,           # Central social discount rate (see DISCOUNT_SCENARIOS for low/high)
  # --- SCC growth rate (g) ---
  # Rate at which the social cost of carbon rises over time.
  # Groom & Venmans (2023): g ≈ GDP growth ≈ 2% in quadratic-damage model.
  # Nordhaus (2017) DICE: SCC grows ~2-3%/yr; Rennert et al. (2022) ~1-2%.
  # g < r required for convergent temporality weight. MC range: 0.5-3%.
  g = 0.02,           # SCC growth rate (Groom & Venmans 2023, aligned with GDP growth)
  tau_1 = 0,          # Project start
  H_ref = Inf,        # Temporality reference horizon (true permanence in calc_T)
  # --- Legal-permanence horizon for afforestation ---
  # Afforestation on cropland/grassland transitions the land use to "forest"
  # under national law; once classified, the land must remain forested and
  # is subject to binding replant/regeneration obligations after clearfell
  # (e.g. Germany BWaldG §11 with 2-3 yr deadlines by Land; France Code
  # forestier art. L124-6, 5 yr after coupe definitive; Sweden Skogsvardslagen
  # 3 yr; Finland Metsalaki; Poland, Romania, Austria national codes;
  # reinforced at EU level by LULUCF Reg. Art. 4 and the Nature Restoration
  # Law 2024). For practices flagged legally_protected_landuse in
  # PRACTICE_DEFS, calc_T integrates to H_perm rather than tau_2 and the
  # empirical buffer is sized to H_perm. IPCC convention; matches WCC's
  # 100-yr contract length.
  H_perm = 100,
  kappa = 0.6,        # Leakage intensity factor (Eq 4); carbon/harvest ratio ~0.4-0.7 from Daigneault (2025)
  ell_max = 0.20,     # Max negative leakage cap (Murray 2004: -4.4% in one scenario)
  # Hazard function (Eq 7). NB: gamma ≠ theta_base (scheme-level loading in ACTUARIAL_PARAMS)
  gamma = 1.10,       # Project-level hazard scaler (+10% precautionary margin)
  alpha = 1.0,        # Vulnerability exponent (tested 0.5-1.5, Table S5)
  beta = 1.20,        # Stock-density hazard exponent: superlinear (Stadelmann 2013,
                      #   Bottero 2016, Seidl & Schelhaas 2011). Tested 0.5-2.0.
  tCO2_per_tC = 3.67  # Conversion factor
)

# =============================================================================
# ACTUARIAL BUFFER POOL — premium principle: b = (1+θ)λ_eff(1+c)/τ
# =============================================================================
ACTUARIAL_PARAMS <- list(
  theta_base = 0.05,        # SD-loading on the actuarial premium
                            # (Klugman/Panjer/Willmot 2012 §5.3).
                            # θ_eff = θ_base × √(H/H_ref) × √(c+(1-c)/N_pool)
                            # Calibrated by 25_finite_N_mc.R against a 99%
                            # solvency target via cross-project sampling.
                            # On the (N × target × RCP) grid every cell
                            # saturates at the bisection floor: the analytic
                            # premium b = λ E[Z] / τ already meets 99%
                            # solvency for N ∈ {100, 200, 500, 1000}.
                            # The 0.05 value carries a precautionary
                            # margin for model risk not captured by the
                            # AR(1)+Beta-mixture loss process (e.g. tail
                            # dependence across biomes, finite-MC
                            # undercounting of 1-in-1000 events). See
                            # output/tables/theta_base_finite_N.csv.
  H_ref      = 30,          # Reference horizon for √-scaling (years)
  # Compound severity: Z ~ p*1.0 + (1-p)*Beta(a,b); E[Z] ≈ 0.82-0.87
  partial_severity_a = 2.0,  # Beta shape1 (partial loss mean ≈ 0.35)
  partial_severity_b = 3.7,  # Beta shape2
  rho_ar1 = 0.30,            # AR(1) disturbance autocorrelation (Senf & Seidl; portfolio ≈ 0.29)
  sigma_AR1 = 0.34           # AR(1) noise σ/μ — empirical from EFDA pool aggregate
                             # 1986-2023 (38 years), forest-area-weighted across
                             # countries. Detrended ≈ raw (no significant trend
                             # at pool scale). Was 0.15 (under-calibrated by ~2×).
                             # Worst observed year: 1990 (Lothar/Wiebke storms)
                             # at 1.9× pool mean; 2022 (drought + Ips) tied.
)

# HORIZON PARAMETERS (summary)
#   LIABILITY_HORIZON       buffer solvency window; θ scales as √(H/H_ref)
#   ACTUARIAL_PARAMS$H_ref  buffer-scaling anchor (where θ = θ_base unscaled)
#   GLOBAL_PARAMS$H_ref     temporality permanence reference (Inf in calc_T)
#   PRACTICE_DEFS$tau_2     per-practice crediting period (10-100 yr)
# Change LIABILITY_HORIZON only → θ auto-rescales via √(H/H_ref); buffer follows.
# θ = θ_base·√(H/H_ref)·√(c+(1-c)/N_pool). With θ_base=0.05, c≈0.2, N_pool=Inf
# (pool factor √c≈0.447): θ(H=30)≈0.022, θ(H=100)≈0.041 — small, because the
# cross-project MC bisection floors at θ≈0 (premium already 99%-solvent); 0.05
# is a precautionary margin (see ACTUARIAL_PARAMS$theta_base note).
LIABILITY_HORIZON <- 30

# Baseline buffer rate for counterfactual comparison (current schemes: 15-20%)
BASELINE_BUFFER_RATE <- 0.20

# =============================================================================
# FOREST-TYPE VULNERABILITY MULTIPLIERS R, BY BIOME (Methods; Marinelli 2026)
# =============================================================================
# conifer/mixed and broadleaf/mixed loss ratios (mixed reference R = 1.0) from
# the within-hexagon fixed-effects regression on Marinelli et al. (2026) JRC
# loss ratios. These are biome-specific: in particular Mediterranean conifer is
# 1.06 (fire is largely species-indiscriminate), NOT 1.37. This REPLACES the
# former flat c(broadleaf = 0.85, conifer = 1.37) that 26_empirical_buffer.R and
# 23_efda_country_buffer_rates.R applied UNIFORMLY across biomes — which
# overstated the Mediterranean conifer headline buffer and biased the JRC
# comparison. Manuscript Methods state conifer/mixed = 1.37 (Boreal, Temperate),
# 1.06 (Mediterranean), with R bounded in [0.60, 1.20] for broadleaf-side stands.
# CONFIRM these against the committed Marinelli FE regression output before the
# regeneration run.
R_BY_BIOME_FT <- list(
  Boreal        = c(broadleaf = 0.85, conifer = 1.37),
  Temperate     = c(broadleaf = 0.77, conifer = 1.37),
  Temperate_UK  = c(broadleaf = 0.77, conifer = 1.37),
  Mediterranean = c(broadleaf = 0.80, conifer = 1.06)
)

# =============================================================================
# AFFORESTATION ESTABLISHMENT-RISK FLOOR, BY BIOME
# =============================================================================
# Newly established afforestation stands carry an establishment-phase failure
# risk (first ~5-10 yr) — planting/seedling mortality, frost & drought failure,
# browsing, total plantation failure — that is INDEPENDENT of the mature-forest
# wind/beetle/fire disturbance rate the EFDA bootstrap captures, so it is absent
# from the headline buffer's main term. It is added as an independent compound
# term in empirical_buffer():  b -> 1 - (1 - b)(1 - floor).
#
# Re-derived from the establishment-failure literature ALONE (NOT tuned to the
# JRC/Marinelli benchmark). The "failure" probability is net of routine
# beating-up: operational standards treat ~10-15% seedling loss as recoverable
# (UK EWCO beating-up; Ireland DAFM 90%-stocking-at-yr-4), so the floor is the
# residual probability that a stand falls below restock threshold / is abandoned.
# Biome gradient is drought-driven (Mediterranean >> Boreal), independent of the
# mature-disturbance term. Central temperate ~7% = union of ~independent drivers
# (drought ~3-5%, browsing ~2-4%, planting/technical ~2-3%); good-practice
# temperate residual ~2-4%; Mediterranean dryland establishment failure is far
# higher (per-seedling 40->94%). Sensitivity sweep: 0.04 / 0.07 / 0.12.
#
# SUPPORTING EVIDENCE (VERIFY exact figures against full PDFs before manuscript
# submission — gathered via abstracts/operational reports):
#   - Boreal: Finnish oak/Norway-spruce afforestation ~12%/5yr per-seedling
#     mortality (Forest Ecol. Manage. 2008, S0378112707007980); Swedish Scots-
#     pine regeneration/browsing failure (Scand. J. For. Res. 2021,
#     doi:10.1080/02827581.2021.2005133).
#   - Temperate: UK 2022 drought ~22% establishment loss (Forestry Commission /
#     Forest Research); deer-browsing establishment failure (Forest Ecol.
#     Manage. 2020, S0378112720311087); operational beating-up tolerance
#     (UK EWCO; Ireland DAFM Afforestation Scheme 2023-27).
#   - Mediterranean: SE-Spain dryland reforestation failure 53% short-term,
#     40->94% by species (Sci. Total Environ. 2021, S0048969721040249).
#   - Global cross-check: planted-tree mortality 18%/yr1 -> 44%/yr5
#     (Banin et al. 2023, Phil. Trans. R. Soc. B, doi:10.1098/rstb.2021.0090).
EST_FLOOR_AFFOREST_BY_BIOME <- c(
  Boreal        = 0.05,
  Temperate     = 0.07,
  Temperate_UK  = 0.07,
  Mediterranean = 0.13
)
# Sensitivity bounds (used by sensitivity scripts): low 0.04, high 0.12.
EST_FLOOR_AFFOREST_RANGE <- c(low = 0.04, central = 0.07, high = 0.12)

# =============================================================================
# BACKTEST PARAMETERS (10_buffer_backtest.R)
# =============================================================================
INITIAL_STOCK_MT   <- 5     # Initial stock for counterfactual backtest (MtCO2)
MARKET_GROWTH_RATE <- 0.10  # Logistic growth rate for backtest

# =============================================================================
# BIOME-SPECIFIC PARAMETERS — Senf & Seidl (2021) Suppl. Table 3
# =============================================================================
# lambda/severity: area-weighted by national forest area (Forest Europe 2020).
# U_*: climate uplift over project lifetime vs 2001-2020 baseline (Grünig et al.
#   2026 Science); agent-weighted (fire/beetle/wind shares from FigS13-15).
#   Used in calc_phi() as λ_H = λ_obs × (1 + U_50). See 02_data_extraction.R.
# S_ref: area-weighted AGB from UNECE SDG 15.2.1(a), → tCO2/ha (×0.47×3.67).
# c: spatial correlation (Anderegg et al. 2020 Science).

BIOME_PARAMS <- list(
  # BOREAL (FI, SE, NO, EE, LV, LT, BY — 79.3 Mha)
  Boreal = list(
    lambda_obs = 0.006, lambda_p90 = 0.008, trend_pct_decade = 22.4, severity = 0.80,
    # Uplift: 60% fire, 18% beetle, 22% wind
    U_50 = 0.34, U_50_rcp85 = 0.45, U_100 = 0.49,
    U_100_rcp85 = 1.06, U_peak = 0.50, U_peak_rcp85 = 0.45,
    c = 0.15,     # Less fire-driven contagion
    S_ref = 142,  # 82.1 t/ha area-weighted → 142 tCO2/ha
    C_stock_AGB = 180, C_seq_rate = 4.0,   # Forest Europe (2020); Pilli et al. (2022)
    # Species R from JRC CRCF forest-type loss ratios (Marinelli et al.
    # 2026): conifer/mixed MC_mean ratio = 1.37, broadleaf/mixed = 0.85.
    # Anchored to JRC frequency-weighted ratios across forest types rather
    # than single-study vulnerability estimates.
    R_mixed = 1.0, R_Scots_pine = 1.10, R_Norway_spruce = 1.35
  ),

  # TEMPERATE (AT, BE, BA, BG, CZ, DK, FR, DE, HU, MD, NL, PL, RO, RS, SK, SI, CH, UA — 78.2 Mha)
  Temperate = list(
    lambda_obs = 0.003, lambda_p90 = 0.004, trend_pct_decade = 12.6, severity = 0.73,
    # Uplift: 35% fire, 31% beetle, 35% wind (lowest uplift — wind barely changes)
    U_50 = 0.25, U_50_rcp85 = 0.33, U_100 = 0.31,
    U_100_rcp85 = 0.72, U_peak = 0.31, U_peak_rcp85 = 0.33,
    c = 0.20,     # ~20km scale
    S_ref = 279,  # 162 t/ha area-weighted → 279 tCO2/ha
    C_stock_AGB = 350, C_seq_rate = 8.5,
    # Species R from JRC forest-type ratios: conifer/mixed = 1.38, broadleaf/mixed
    # = 0.77. Norway spruce anchored at 1.35 (dominant conifer); native broadleaves
    # at 0.80. Climate-adapted mix at 0.75 (diversity bonus, Jactel & Brockerhoff).
    R_mixed = 1.0, R_Norway_spruce = 1.35, R_Native_broadleaves = 0.80,
    R_Mixed_conifers = 1.35, R_Climate_adapted_mix = 0.75, R_Old_growth = 0.70,
    R_Mixed_broadleaves = 0.80, R_Beech_oak = 0.80,
    R_Native_pinewood = 1.10, R_Productive_oak_beech = 0.80
  ),

  # TEMPERATE_UK — comprehensive disturbance (wind + pest + fire)
  # Lower than continental (oceanic climate, active management) but higher than fire-only.
  # Wind dominant: Storm Arwen 2021 ~0.4% loss; pest: ash dieback ~0.1%; fire: ~0.025%.
  Temperate_UK = list(
    lambda_obs = 0.005, lambda_p90 = 0.004, trend_pct_decade = 8.0, severity = 0.89,
    # Uplift: Temperate × 0.8 oceanic buffer (wind-dominated, less climate-sensitive)
    U_50 = 0.20, U_50_rcp85 = 0.26, U_100 = 0.25,
    U_100_rcp85 = 0.57, U_peak = 0.25, U_peak_rcp85 = 0.26,
    c = 0.12,     # Moist climate limits fire spread
    S_ref = 197,  # 114 t/ha area-weighted → 197 tCO2/ha
    C_stock_AGB = 300, C_seq_rate = 10.0,
    # UK: Sitka spruce dominant conifer; JRC Temperate conifer ratio applied.
    R_mixed = 1.0, R_Native_broadleaves = 0.80, R_Sitka_spruce = 1.35,
    R_Mixed_conifers = 1.35, R_Norway_spruce = 1.35
  ),

  # MEDITERRANEAN (PT, ES, GR, IT, AL, ME, MK, HR — 40.1 Mha)
  Mediterranean = list(
    lambda_obs = 0.008, lambda_p90 = 0.011, trend_pct_decade = 12.3, severity = 0.76,
    # Uplift: 82% fire, 3% beetle, 14% wind (highest — fire increases fastest)
    U_50 = 0.42, U_50_rcp85 = 0.55, U_100 = 0.64,
    U_100_rcp85 = 1.34, U_peak = 0.67, U_peak_rcp85 = 0.55,
    c = 0.25,     # High fire contagion
    S_ref = 126,  # 73.0 t/ha area-weighted → 126 tCO2/ha
    C_stock_AGB = 150, C_seq_rate = 5.5,
    # JRC Mediterranean: conifer/mixed = 1.06 (small gap — fire is species-
    # indiscriminate). Maritime pine kept at 1.10 (slightly above mixed due to
    # resinous flammability); Cork oak at 0.80 (fire-resistant bark).
    R_mixed = 1.0, R_Maritime_pine = 1.10, R_Cork_oak = 0.80, R_Aleppo_pine = 1.05
  )
)

# =============================================================================
# OVERRIDE FROM EXTRACTED DATA (02_data_extraction.R, if available)
# =============================================================================
.uplift_file <- file.path("data", "processed", "gruenig_uplift_factors.rds")
.sref_file   <- file.path("data", "processed", "sref_biome.rds")
.senf_file   <- file.path("data", "processed", "senf_biome_rates.rds")

if (file.exists(.uplift_file) && file.exists(.sref_file)) {
  cat("  Patching BIOME_PARAMS from extracted data...\n")
  .uplifts <- readRDS(.uplift_file)
  .srefs   <- readRDS(.sref_file)

  for (.bname in c("Boreal", "Temperate", "Mediterranean")) {
    if (!.bname %in% names(BIOME_PARAMS)) next

    .bu <- .uplifts[.uplifts$biome_3 == .bname, ]
    .r45 <- .bu[.bu$scen == "RCP4.5", ]
    .r85 <- .bu[.bu$scen == "RCP8.5", ]

    if (nrow(.r45) == 1) {
      BIOME_PARAMS[[.bname]]$U_50  <- round(.r45$U_50, 3)
      BIOME_PARAMS[[.bname]]$U_100 <- round(.r45$U_100, 3)
      BIOME_PARAMS[[.bname]]$U_peak <- round(.r45$U_peak, 3)
    }
    if (nrow(.r85) == 1) {
      BIOME_PARAMS[[.bname]]$U_50_rcp85  <- round(.r85$U_50, 3)
      BIOME_PARAMS[[.bname]]$U_100_rcp85 <- round(.r85$U_100, 3)
      BIOME_PARAMS[[.bname]]$U_peak_rcp85 <- round(.r85$U_peak, 3)
    }

    .sref <- .srefs$S_ref[.srefs$biome == .bname]
    if (length(.sref) == 1) BIOME_PARAMS[[.bname]]$S_ref <- .sref
  }

  # Temperate_UK: Temperate uplift × 0.8 oceanic buffer
  if ("Temperate_UK" %in% names(BIOME_PARAMS)) {
    .r45 <- .uplifts[.uplifts$biome_3 == "Temperate" & .uplifts$scen == "RCP4.5", ]
    .r85 <- .uplifts[.uplifts$biome_3 == "Temperate" & .uplifts$scen == "RCP8.5", ]
    if (nrow(.r45) == 1) {
      BIOME_PARAMS$Temperate_UK$U_50   <- round(.r45$U_50 * 0.8, 3)
      BIOME_PARAMS$Temperate_UK$U_100  <- round(.r45$U_100 * 0.8, 3)
      BIOME_PARAMS$Temperate_UK$U_peak <- round(.r45$U_peak * 0.8, 3)
    }
    if (nrow(.r85) == 1) {
      BIOME_PARAMS$Temperate_UK$U_50_rcp85   <- round(.r85$U_50 * 0.8, 3)
      BIOME_PARAMS$Temperate_UK$U_100_rcp85  <- round(.r85$U_100 * 0.8, 3)
      BIOME_PARAMS$Temperate_UK$U_peak_rcp85 <- round(.r85$U_peak * 0.8, 3)
    }

    .sref_uk <- .srefs$S_ref[.srefs$biome == "Temperate_UK"]
    if (length(.sref_uk) == 1) BIOME_PARAMS$Temperate_UK$S_ref <- .sref_uk
  }

  cat("  Patched uplift + S_ref.\n")
  rm(.uplifts, .srefs, .bu, .r45, .r85, .sref, .bname)
  if (exists(".sref_uk")) rm(.sref_uk)
} else {
  warning("Grünig uplift / S_ref extracts missing (", .uplift_file, ", ",
          .sref_file, ") - BIOME_PARAMS$U_* and S_ref retain hardcoded ",
          "literature values (NOT extracted). Run 02_data_extraction.R.")
}

# Patch SEVERITY from Senf & Seidl (2021) extraction.
# NB: lambda_obs is deliberately NOT taken from Senf. Senf maps TOTAL canopy
# mortality (harvest-inclusive); the model's disturbance hazard is NATURAL
# only (harvest is contractually managed within CRCF projects and excluded).
# In harvest-dominated biomes (esp. Boreal) the Senf total far exceeds the
# natural rate. lambda_obs is therefore patched below from the EFDA natural
# aggregates — the same basis as the country-level COUNTRY_PARAMS$lambda_obs —
# so the biome- and country-level layers share one calibration. Severity has
# no EFDA equivalent and is retained from Senf.
if (file.exists(.senf_file)) {
  .senf <- readRDS(.senf_file)
  for (.bname in c("Boreal", "Temperate", "Mediterranean", "Temperate_UK")) {
    if (!.bname %in% names(BIOME_PARAMS)) next
    .row <- .senf[.senf$biome == .bname, ]
    if (nrow(.row) == 1)
      BIOME_PARAMS[[.bname]]$severity <- round(.row$severity, 2)
  }
  cat("  Patched severity (Senf & Seidl 2021).\n")
  rm(.senf, .row, .bname)
}

# Patch lambda_obs from EFDA natural-disturbance biome aggregates (European
# Forest Disturbance Atlas v2.1.1, Viana-Soto & Senf 2025; codes 1+2 only,
# harvest excluded). Biome value = forest-area-weighted mean of per-country
# lambda_full over 1986-2023 — identical basis to COUNTRY_PARAMS$lambda_obs,
# so a biome-fallback project and a country-level project share the same
# natural-disturbance calibration. Validated in 11_disturbance_analysis.R.
.efda_summary_file <- file.path("data", "processed", "efda_country_summary.rds")
if (file.exists(.efda_summary_file)) {
  .efda_sum <- as.data.frame(readRDS(.efda_summary_file))
  for (.bname in c("Boreal", "Temperate", "Mediterranean", "Temperate_UK")) {
    if (!.bname %in% names(BIOME_PARAMS)) next
    .z <- .efda_sum[.efda_sum$biome == .bname &
                    !is.na(.efda_sum$lambda_full) &
                    !is.na(.efda_sum$forest_kha), ]
    if (nrow(.z) >= 1)
      BIOME_PARAMS[[.bname]]$lambda_obs <-
        round(sum(.z$forest_kha * .z$lambda_full) / sum(.z$forest_kha), 5)
  }
  cat("  Patched lambda_obs (EFDA natural, area-weighted biome means).\n")
  rm(.efda_sum, .z, .bname)
} else {
  warning("efda_country_summary.rds missing - BIOME_PARAMS$lambda_obs retains ",
          "hardcoded values (NOT EFDA natural); biome/country layers diverge.")
}
rm(.efda_summary_file)
rm(.uplift_file, .sref_file, .senf_file)


# =============================================================================
# COUNTRY-LEVEL PARAMETERS (hybrid: country-specific λ, severity, U; biome-
# inherited c, S_ref, R_*). COUNTRY_PARAMS is a tibble with country-specific
# overrides; params_for() merges with BIOME_PARAMS lazily on lookup.
# =============================================================================
.efda_file    <- file.path("data", "processed", "efda_country_summary.rds")
.gru_country  <- file.path("data", "processed", "gruenig_country_uplift_factors.rds")
COUNTRY_PARAMS <- tibble::tibble()

if (file.exists(.efda_file) && file.exists(.gru_country)) {
  cat("  Building COUNTRY_PARAMS from EFDA + Grünig country aggregates...\n")

  .gru_wide <- readRDS(.gru_country) %>%
    tidyr::pivot_wider(id_cols = country,
                       names_from = scen,
                       values_from = c(U_50, U_100, U_peak)) %>%
    rename(country_root = country,
           U_50         = `U_50_RCP4.5`, U_50_rcp85   = `U_50_RCP8.5`,
           U_100        = `U_100_RCP4.5`, U_100_rcp85  = `U_100_RCP8.5`,
           U_peak       = `U_peak_RCP4.5`, U_peak_rcp85 = `U_peak_RCP8.5`)

  COUNTRY_PARAMS <- readRDS(.efda_file) %>%
    select(zone_label, country_root, sub_zone, biome,
           lambda_obs = lambda_full, severity, forest_kha) %>%
    left_join(.gru_wide, by = "country_root") %>%
    mutate(lambda_obs = round(lambda_obs, 5),
           across(c(severity, U_50, U_100, U_peak,
                    U_50_rcp85, U_100_rcp85, U_peak_rcp85),
                  ~round(., 3)))

  # Surface country-name join misses: U_50 NA after the Grünig join means either
  # a genuine low-hexagon-coverage country (biome-mean fallback applies in
  # params_for) OR a country-name mismatch. Warn with the list so a name-join
  # bug cannot pass silently (A2.3).
  .na_u <- unique(COUNTRY_PARAMS$country_root[is.na(COUNTRY_PARAMS$U_50)])
  if (length(.na_u) > 0)
    warning("COUNTRY_PARAMS: U_50 is NA for ", length(.na_u), " country(ies) [",
            paste(.na_u, collapse = ", "), "] after the Grünig join - biome-mean ",
            "fallback will apply. Verify these are genuine low-coverage countries, ",
            "not name-join mismatches.")
  rm(.na_u)

  cat(sprintf("  COUNTRY_PARAMS built for %d zones (%d countries + %d sub-national)\n",
              nrow(COUNTRY_PARAMS),
              sum(is.na(COUNTRY_PARAMS$sub_zone)),
              sum(!is.na(COUNTRY_PARAMS$sub_zone))))
  rm(.gru_wide)
}
rm(.efda_file, .gru_country)

# Helper: return merged parameter list for a zone, country, or biome.
# Lookup order: zone_label → country_root → biome name. Country-level
# overrides (λ, severity, U) replace biome defaults; c, S_ref, R_* are
# inherited from BIOME_PARAMS unchanged.
params_for <- function(name) {
  row <- NULL
  if (nrow(COUNTRY_PARAMS) > 0) {
    hit <- which(COUNTRY_PARAMS$zone_label == name)
    if (length(hit) == 0) {
      hit <- which(COUNTRY_PARAMS$country_root == name)
      if (length(hit) > 1)
        stop("'", name, "' is split into sub-zones [",
             paste(COUNTRY_PARAMS$zone_label[hit], collapse = ", "),
             "]; specify a sub-zone explicitly")
    }
    if (length(hit) > 0) row <- COUNTRY_PARAMS[hit[1], ]
  }
  if (is.null(row)) {
    if (!name %in% names(BIOME_PARAMS)) stop("No params for: ", name)
    bp <- BIOME_PARAMS[[name]]
    bp$country <- NA_character_; bp$zone_label <- name; bp$biome <- name
    return(bp)
  }
  bp <- BIOME_PARAMS[[row$biome]]
  if (is.null(bp)) stop("No BIOME_PARAMS for biome '", row$biome,
                         "' (zone: ", name, ")")
  overrides <- c("country_root" = "country", "zone_label" = "zone_label",
                 "biome" = "biome", "lambda_obs" = "lambda_obs",
                 "severity" = "severity", "forest_kha" = "forest_kha",
                 "U_50" = "U_50", "U_100" = "U_100", "U_peak" = "U_peak",
                 "U_50_rcp85" = "U_50_rcp85", "U_100_rcp85" = "U_100_rcp85",
                 "U_peak_rcp85" = "U_peak_rcp85")
  for (src in names(overrides)) {
    val <- row[[src]]
    if (!is.null(val) && !is.na(val)) bp[[overrides[src]]] <- val
  }
  bp
}


# =============================================================================
# LEAKAGE PARAMETERS
# Sources (demand elasticities):
#   Kangas & Baudin (2003) ETTS V, ECE/TIM/DP/30 — country-level demand & trade
#     elasticities for 18 European countries, 1964-1991 (Tables 3-25)
#   Buongiorno (2015) Silva Fennica 49(5):1395 — global panel, 180 countries
#   Couture, Garcia & Reynaud (2012) Energy Econ 34:1972-1981 — French fuelwood
# Sources (supply elasticities):
#   Kallio & Solberg (2018) — Nordic roundwood supply; total leakage 73-84%
#   Rorstad, Solberg et al. (2022) Silva Fennica 56(1):10326 — Norwegian regional
#     sawlog (0.74-1.77) and pulpwood (0.13-1.00) supply by region
#   Borzykowski (2019) Forest Pol Econ 102:100-113 — Swiss roundwood supply ~0.5
#   Tian et al. (2017) Forest Prod J 67:152-163 — meta-analysis of timber supply
#   Forest Research UK (2024) — UK import-weighted elasticities
# Sources (trade & model calibration):
#   Morland et al. (2018) Forest Pol Econ 92:92-105 — TiMBA global model
#   Sauquet et al. (2011) Resource Energy Econ 33:771-781 — French Armington (FFSM)
#   Murray (2004) — leakage framework
# =============================================================================

LEAKAGE_PARAMS <- list(
  # Generic product-class elasticities (fallback when biome-specific unavailable)
  # Approximate biome-weighted average; see biome-specific blocks for sources
  elasticities_by_product = list(
    SL = list(eps_d = -0.30, eps_s_dom = 0.55, eps_s_imp = 1.10, gamma = 0.85),
    PW = list(eps_d = -0.25, eps_s_dom = 0.45, eps_s_imp = 0.80, gamma = 0.95),
    WF = list(eps_d = -0.20, eps_s_dom = 0.30, eps_s_imp = 0.50, gamma = 0.80)
  ),

  # Practice → product-class weights (sum to 1.0 within each practice)
  practice_product_weights = list(
    # Harvest-reducing
    `Set-aside`                   = list(SL = 0.60, PW = 0.30, WF = 0.10),
    `Extended rotation`           = list(SL = 0.70, PW = 0.20, WF = 0.10),
    `Reduced harvest intensity`   = list(SL = 0.20, PW = 0.60, WF = 0.20),
    `Coppice conversion`          = list(SL = 0.60, PW = 0.30, WF = 0.10),
    `Reforestation`               = list(SL = 0.50, PW = 0.30, WF = 0.20),
    `Continuous stock management`  = list(SL = 0.55, PW = 0.35, WF = 0.10),
    `Forested peatland rewetting` = list(SL = 0.20, PW = 0.70, WF = 0.10),
    # Harvest-neutral
    `Structural diversification`  = list(SL = 0.40, PW = 0.40, WF = 0.20),
    `Species diversification`     = list(SL = 0.50, PW = 0.30, WF = 0.20),
    `Fuel management`             = list(SL = 0.00, PW = 0.00, WF = 0.00),
    `Peatland rewetting`          = list(SL = 0.00, PW = 0.00, WF = 0.00),
    # Harvest-increasing
    `Protected afforestation`     = list(SL = 0.50, PW = 0.30, WF = 0.20),
    `Productive afforestation`    = list(SL = 0.50, PW = 0.30, WF = 0.20),
    `Agroforestry`                = list(SL = 0.30, PW = 0.30, WF = 0.40),
    `Site fertilisation`          = list(SL = 0.60, PW = 0.30, WF = 0.10),
    `Short-rotation plantation`   = list(SL = 0.00, PW = 0.90, WF = 0.10)
  ),

  # Trade shares by biome
  trade_shares = list(
    Boreal = list(alpha_dom = 0.70, alpha_imp = 0.30),
    Temperate = list(alpha_dom = 0.60, alpha_imp = 0.40),
    Mediterranean = list(alpha_dom = 0.80, alpha_imp = 0.20)
  ),

  # Biome × product-class parameters (Table C in paper)
  #
  # BOREAL (Finland, Sweden, Norway, Baltics)
  # eps_d SL: -0.40 — Scandinavian markets well-integrated, price-responsive;
  #   above Morland (2018) global pooled -0.30 and high-income -0.16,
  #   consistent with Kallio & Solberg (2018); ETTS V lacks Nordic data
  # eps_s_dom SL: 0.90 — Norwegian regional sawlog supply 0.74-1.77
  #   (Rorstad et al. 2022, first-difference static model; common-slope
  #   1.24, median 1.21); conservative relative to Norwegian estimates;
  #   well above Swiss LR 0.37-0.53 (Borzykowski 2019), reflecting
  #   large managed forest estate and active timber markets
  # eps_s_dom PW: 0.60 — Norwegian pulpwood 0.13-1.00, common-slope 0.50
  #   (Rorstad et al. 2022); Finnish pulpwood ~1.0 (EFI-GTM)
  Boreal = list(
    SL = list(u = 0.70, s = 0.90, eps_d = -0.40, eps_s_dom = 0.90, eps_s_imp = 1.20),
    PW = list(u = 0.70, s = 0.90, eps_d = -0.30, eps_s_dom = 0.60, eps_s_imp = 0.80),
    WF = list(u = 0.80, s = 0.85, eps_d = -0.15, eps_s_dom = 0.40, eps_s_imp = 0.50)
  ),
  # TEMPERATE (France, Germany, Austria, Switzerland, Benelux, Poland)
  # eps_d SL: -0.45 — weighted from ETTS V: France -0.52, Germany -0.44,
  #   Austria -0.12; production-weighted toward FR/DE. Above Morland (2018)
  #   high-income pooled -0.16 and Buongiorno (2015) pooled -0.17, but
  #   ETTS V is geographically specific to these markets
  # eps_s_dom SL: 0.50 — Swiss roundwood LR 0.37 preferred, 0.53 with
  #   structural breaks (Borzykowski 2019, 3SLS, 1949-2013); 0.50 is
  #   central estimate. Well below Nordic (0.90) reflecting smaller
  #   forest share of GDP, fragmented private ownership. Morland (2018)
  #   global domestic supply 0.04-0.08 (too low for individual-country
  #   estimates of managed European forests)
  # eps_s_dom PW: 0.45 — pulpwood supply less responsive than sawlog
  #   (Rorstad et al. 2022); scaled from SL proportionally
  # eps_d WF: -0.25 — Couture et al. (2012) French fuelwood -0.40;
  #   GFPM default -0.10; temperate mean reflects mix of dedicated
  #   fuelwood markets (FR) and residual use (DE/AT)
  Temperate = list(
    SL = list(u = 0.60, s = 0.90, eps_d = -0.45, eps_s_dom = 0.50, eps_s_imp = 1.10),
    PW = list(u = 0.60, s = 0.90, eps_d = -0.30, eps_s_dom = 0.45, eps_s_imp = 0.80),
    WF = list(u = 0.50, s = 0.85, eps_d = -0.25, eps_s_dom = 0.35, eps_s_imp = 0.50)
  ),
  # Temperate_UK: heavy importer (~80%); low market leakage (Forest Research UK 2024)
  # eps_d SL: -0.20 — ETTS V UK coniferous sawnwood; small domestic
  #   market dominated by imports, low own-price sensitivity
  Temperate_UK = list(
    SL = list(u = 0.32, s = 0.95, eps_d = -0.20, eps_s_dom = 0.39, eps_s_imp = 1.50),
    PW = list(u = 0.25, s = 0.95, eps_d = -0.25, eps_s_dom = 0.50, eps_s_imp = 1.20),
    WF = list(u = 0.30, s = 0.90, eps_d = -0.15, eps_s_dom = 0.40, eps_s_imp = 0.80)
    # SL rho_rep ~ 0.35 (vs ~0.70 continental); WCC is primarily afforestation
  ),
  # MEDITERRANEAN (Spain, Portugal, Italy, Greece, southern France)
  # eps_d SL: -0.08 — ETTS V: Italy -0.04, Spain -0.09; markets
  #   small, construction-driven, low substitutability with imports.
  #   Consistent with Morland (2018) high-income SL demand -0.16
  #   (Mediterranean likely below high-income average)
  # eps_s_dom SL: 0.30 — no direct Mediterranean supply estimates;
  #   set below Swiss LR 0.37 (Borzykowski 2019) given smaller managed
  #   estate, cork/non-timber orientation, fragmented ownership.
  #   Morland (2018) global domestic supply 0.04-0.08 (panel average
  #   including many non-forestry economies)
  # eps_s_dom PW: 0.20 — pulpwood markets thin (Iberian eucalyptus
  #   pulp largely vertically integrated); lower price responsiveness
  # eps_d WF: -0.10 — fuelwood as subsistence/residual use in
  #   Mediterranean; Morland (2018) fuelwood demand -0.15 (global);
  #   GFPM default -0.10
  Mediterranean = list(
    SL = list(u = 0.80, s = 0.85, eps_d = -0.08, eps_s_dom = 0.30, eps_s_imp = 0.90),
    PW = list(u = 0.80, s = 0.85, eps_d = -0.15, eps_s_dom = 0.20, eps_s_imp = 0.70),
    WF = list(u = 0.85, s = 0.80, eps_d = -0.10, eps_s_dom = 0.20, eps_s_imp = 0.40)
  )
)

# =============================================================================
# CHITI ET AL. (2026) RATE LOOKUP — from paper Table 1 (authoritative)
# =============================================================================
# Reads Chiti's Table 1 per-study values (data/Chiti_et_al_2026_Table1.csv,
# 113 rows transcribed from the paper). For each practice × biome we filter
# Table 1 by management category (± optional type or native/plantation) and
# take the mean of AGB (or AGB+SOC for peatland practices).
#
# Practices outside Chiti's Table 1 scope stay hardcoded — see rate_source
# column on ALL_PROJECTS after override.

.chiti_t1 <- if (file.exists("data/Chiti_et_al_2026_Table1.csv"))
  read.csv("data/Chiti_et_al_2026_Table1.csv", stringsAsFactors = FALSE, na.strings = "") else NULL
if (!is.null(.chiti_t1))
  cat(sprintf("  Loaded Chiti Table 1 (%d rows)\n", nrow(.chiti_t1)))

# Filter Table 1 subset and return mean rate (MgCO2/ha/yr)
.t1_mean <- function(t1_biome, management, field = "AGB",
                     type = NULL, native = NULL) {
  if (is.null(.chiti_t1)) return(list(val = NA_real_, n = 0L))
  r <- .chiti_t1[.chiti_t1$biome == t1_biome &
                 .chiti_t1$management %in% management, ]
  if (!is.null(type))   r <- r[!is.na(r$type)   & r$type   %in% type, ]
  if (!is.null(native)) r <- r[!is.na(r$native) & r$native %in% native, ]
  if (nrow(r) == 0) return(list(val = NA_real_, n = 0L))
  # n counts rows that actually contribute a value to the mean (non-NA in the
  # averaged field), not all filtered rows — blank AGB/SOC cells must not
  # inflate the reported sample size in rate_provenance.csv.
  val <- switch(field,
    "AGB"     = mean(r$AGB_MgCO2, na.rm = TRUE),
    "SOC"     = mean(r$SOC_MgCO2, na.rm = TRUE),
    "AGB+SOC" = {
      a <- mean(r$AGB_MgCO2, na.rm = TRUE)
      s <- mean(r$SOC_MgCO2, na.rm = TRUE)
      sum(if (is.nan(a)) 0 else a, if (is.nan(s)) 0 else s)
    })
  n <- switch(field,
    "AGB"     = sum(!is.na(r$AGB_MgCO2)),
    "SOC"     = sum(!is.na(r$SOC_MgCO2)),
    "AGB+SOC" = sum(!is.na(r$AGB_MgCO2) | !is.na(r$SOC_MgCO2)))
  if (is.nan(val) || is.na(val)) list(val = NA_real_, n = 0L)
  else list(val = round(val, 2), n = n)
}

# Compute within-study Control-vs-treated differential from Harvest Intensity data
.t1_differential <- function(t1_biome, management = "Harvest Intensity",
                              baseline = "Control", treatments = "Moderate") {
  if (is.null(.chiti_t1)) return(list(val = NA_real_, n = 0L))
  r <- .chiti_t1[.chiti_t1$biome == t1_biome &
                 .chiti_t1$management == management, ]
  if (nrow(r) == 0) return(list(val = NA_real_, n = 0L))
  diffs <- c()
  for (ref in unique(r$reference)) {
    study <- r[r$reference == ref, ]
    ctrls <- study$AGB_MgCO2[!is.na(study$type) & study$type == baseline]
    treats <- study$AGB_MgCO2[!is.na(study$type) & study$type %in% treatments]
    ctrls <- ctrls[!is.na(ctrls)]; treats <- treats[!is.na(treats)]
    if (length(ctrls) == 0 || length(treats) == 0) next
    diffs <- c(diffs, mean(ctrls) - mean(treats))
  }
  if (length(diffs) == 0) return(list(val = NA_real_, n = 0L))
  list(val = round(mean(diffs), 2), n = length(diffs))
}

# Practice × biome → Table 1 filter spec.
# Practices missing from this list stay hardcoded (Chiti Table 1 has no
# matching category: Reforestation, Species/Structural diversification,
# Fuel management, Set-aside, Continuous stock management, Coppice conversion).
CHITI_MAP <- list(
  # Afforestation (Protected = native broadleaves; Productive = plantation conifers/eucalyptus)
  list(practice = "Protected afforestation",  biome = "Temperate",
       t1_biome = "Temperate",     management = "Afforestation cropland",
       type = "B", field = "AGB"),
  list(practice = "Productive afforestation", biome = "Boreal",
       t1_biome = "Boreal",        management = "Afforestation grassland",
       type = "C", field = "AGB"),
  list(practice = "Productive afforestation", biome = "Temperate",
       t1_biome = "Temperate",     management = "Afforestation grassland",
       type = "C", field = "AGB"),
  # Med: all conifers across cropland+grassland (incl. plantation Pinus radiata, Pseudotsuga)
  list(practice = "Productive afforestation", biome = "Mediterranean",
       t1_biome = "Mediterranean",
       management = c("Afforestation cropland", "Afforestation grassland"),
       type = "C", field = "AGB"),

  # Rotation extensions
  list(practice = "Extended rotation", biome = "Boreal",
       t1_biome = "Boreal",        management = "Longer rotation period", field = "AGB"),
  list(practice = "Extended rotation", biome = "Mediterranean",
       t1_biome = "Mediterranean", management = "Longer rotation",
       native = "N", field = "AGB"),

  # Reduced harvest intensity: Chiti Table 1 has "Harvest Intensity" data for Med
  # (Control/Light/Moderate/Heavy). The within-study Control-Moderate differential
  # is extracted via .t1_differential(). No Boreal/Temperate Harvest Intensity
  # data in Chiti; those biomes stay hardcoded (Hynynen 2005).

  # Fertilisation
  list(practice = "Site fertilisation", biome = "Boreal",
       t1_biome = "Boreal",    management = "Ash fertilisation", field = "AGB"),
  list(practice = "Site fertilisation", biome = "Temperate",
       t1_biome = "Temperate", management = "Fertilisation",     field = "AGB"),

  # Agroforestry
  list(practice = "Agroforestry", biome = "Mediterranean",
       t1_biome = "Mediterranean", management = "Agroforestry", field = "AGB"),

  # Peatland — SOC only (the mechanism). Boreal only: Temperate Chiti sample
  # is heterogeneous (n=4) with 3/4 studies reporting continued SOC emissions,
  # so Peatland rewetting Temp stays hardcoded (Mander 2024 global mean).
  list(practice = "Forested peatland rewetting", biome = "Boreal",
       t1_biome = "Boreal", management = "Peatland management", field = "SOC"),
  list(practice = "Peatland rewetting", biome = "Boreal",
       t1_biome = "Boreal", management = "Peatland management", field = "SOC")
)

# --- Pre-compute all Chiti Table 1 rates ---
.chiti_lookup <- NULL
if (!is.null(.chiti_t1)) {
  .chiti_lookup <- do.call(rbind, lapply(CHITI_MAP, function(m) {
    res <- .t1_mean(m$t1_biome, m$management, m$field,
                    type = m$type, native = m$native)
    data.frame(
      practice = m$practice, biome = m$biome,
      chiti_rate = res$val, chiti_n = res$n,
      chiti_detail = sprintf("Chiti T1: %s, %s, %s, n=%d",
                             m$t1_biome, m$management, m$field, res$n),
      stringsAsFactors = FALSE)
  }))
  .chiti_lookup <- .chiti_lookup[!is.na(.chiti_lookup$chiti_rate), ]
  # Deduplicate (vector management fields can produce duplicates)
  .chiti_lookup <- .chiti_lookup[!duplicated(.chiti_lookup[, c("practice", "biome")]), ]

  # Reduced harvest intensity Med: Control-Moderate differential from Harvest Intensity
  .rhi_med <- .t1_differential("Mediterranean")
  if (!is.na(.rhi_med$val)) {
    .chiti_lookup <- rbind(.chiti_lookup, data.frame(
      practice = "Reduced harvest intensity", biome = "Mediterranean",
      chiti_rate = .rhi_med$val, chiti_n = .rhi_med$n,
      chiti_detail = sprintf("Chiti T1: Med Harvest Intensity, Control-Moderate differential, n=%d studies",
                             .rhi_med$n),
      stringsAsFactors = FALSE))
  }

  # Interpolate Extended rotation Temperate from Boreal + Med Chiti means
  .ext_b <- .chiti_lookup$chiti_rate[.chiti_lookup$practice == "Extended rotation" &
                                      .chiti_lookup$biome == "Boreal"]
  .ext_m <- .chiti_lookup$chiti_rate[.chiti_lookup$practice == "Extended rotation" &
                                      .chiti_lookup$biome == "Mediterranean"]
  if (length(.ext_b) == 1 && length(.ext_m) == 1) {
    .chiti_lookup <- rbind(.chiti_lookup, data.frame(
      practice = "Extended rotation", biome = "Temperate",
      chiti_rate = round(mean(c(.ext_b, .ext_m)), 2), chiti_n = NA_integer_,
      chiti_detail = sprintf("Interpolated: mean of Boreal (%.2f) and Med (%.2f)",
                             .ext_b, .ext_m),
      stringsAsFactors = FALSE))
  }

  cat(sprintf("  Pre-computed %d Chiti Table 1 rates\n", nrow(.chiti_lookup)))
}
rm(.chiti_t1, .t1_mean, .t1_differential, CHITI_MAP)

# =============================================================================
# ALL_PROJECTS — SINGLE SOURCE OF TRUTH (Table 2 in paper)
# =============================================================================
# rate: net additional MgCO2/ha/yr.
#   NA  = sourced from Chiti Table 1 (filled below from .chiti_lookup)
#   numeric = hardcoded from literature (source in rate_citation column)
# area_ha: grounded in scheme registry data (WCC ~47ha, LBC ~11ha, PLC ~135ha)
# Q = rate × area_ha × duration_yr. net_share is Q-invariant.
# S_bar: tCO2/ha for buffer calc; must be consistent with biome stocks.
# is_anchor: TRUE = main analysis biome; FALSE = cross-biome sensitivity.
# =============================================================================

ALL_PROJECTS <- tibble::tribble(
  ~practice, ~biome, ~species, ~rate, ~area_ha, ~duration_yr, ~S_bar, ~is_anchor,

  # /////////////////////////////////////////////////////////////////////////
  # SECTION A — DATA-EXTRACTED RATES (rate = NA, filled from Chiti Table 1)
  # /////////////////////////////////////////////////////////////////////////
  # These rows have rate = NA. After the tribble, the Chiti Table 1 lookup
  # (.chiti_lookup) fills them automatically. If the CSV is missing or a
  # lookup fails, the pipeline stops with an error.

  # Extended rotation: "Longer rotation period" (Boreal), "Longer rotation" (Med)
  # Temperate: interpolated from Boreal + Med means
  "Extended rotation", "Boreal",        "Mixed conifers",  NA, 100, 30, 180, FALSE,
  "Extended rotation", "Temperate",     "Mixed conifers",  NA, 100, 30, 300, TRUE,  # above S_ref: stands deliberately older than rotation age
  "Extended rotation", "Mediterranean", "Maritime pine",    NA, 100, 30,  80, FALSE,

  # Reduced harvest intensity Med: "Harvest Intensity" Control-Moderate differential
  "Reduced harvest intensity", "Mediterranean", "Maritime pine", NA, 100, 30, 80, FALSE,

  # Forested peatland rewetting Boreal: "Peatland management", SOC field
  "Forested peatland rewetting", "Boreal", "Drained peatland forest", NA, 135, 30, 120, TRUE,

  # Peatland rewetting Boreal: same Chiti "Peatland management" SOC field
  "Peatland rewetting", "Boreal", "Paludiculture", NA, 135, 50, 50, TRUE,

  # Protected afforestation: "Afforestation cropland", type=B (native broadleaves)
  "Protected afforestation", "Temperate", "Native broadleaves", NA, 47, 100, 200, TRUE,  # sigmoid time-weighted mean over 100 yr (0 → ~350 tCO2/ha)

  # Productive afforestation: "Afforestation grassland", type=C (conifers)
  # Med: all conifers pooled across cropland + grassland
  "Productive afforestation", "Boreal",        "Scots pine",    NA, 47, 40,  85, FALSE,
  "Productive afforestation", "Temperate",     "Sitka spruce",  NA, 47, 40, 180, TRUE,  # fast growth but thinned + clearfelled at 40 yr; lower time-weighted mean
  "Productive afforestation", "Mediterranean", "Maritime pine", NA, 47, 40,  55, FALSE,

  # Site fertilisation: "Ash fertilisation" (Boreal), "Fertilisation" (Temperate)
  "Site fertilisation", "Boreal",    "Scots pine",        NA, 100, 10,  85, TRUE,
  "Site fertilisation", "Temperate", "Mixed broadleaves", NA, 100, 10, 160, FALSE,

  # Agroforestry Med: "Agroforestry"
  "Agroforestry", "Mediterranean", "Cork oak", NA, 15, 30, 40, TRUE,

  # /////////////////////////////////////////////////////////////////////////
  # SECTION B — LITERATURE-SOURCED RATES (rate = numeric, hardcoded)
  # /////////////////////////////////////////////////////////////////////////
  # These rows have explicit numeric rates from primary literature sources
  # (not Chiti Table 1). Each rate has a citation in the comment above it.
  # See output/tables/rate_provenance.csv for full provenance record.

  # Reduced harvest intensity Boreal/Temp: Hynynen et al. 2005 (Forest Ecol
  #   Manage 207, Fig 8) Picea abies unmanaged-vs-thinned differential
  #   0.79 MgC/ha/yr = 2.9 MgCO2/yr; moderate intensity interpretation → 1.5
  "Reduced harvest intensity", "Boreal",    "Mixed conifers", 1.5, 100, 30, 180, FALSE,
  "Reduced harvest intensity", "Temperate", "Mixed conifers", 1.5, 100, 30, 220, TRUE,

  # Set-aside: no Chiti Table 1 category for full harvest cessation
  #   Boreal: Hynynen 2005 Fig 8 unmanaged Picea 0.79 MgC/ha/yr = 2.9 MgCO2/yr
  #   Temperate: Chiti study-level "Management vs no-intervention" ~4.1-4.5
  #   Med: Palma 2018 + Francaviglia 2012 cork oak
  "Set-aside", "Boreal",        "Norway spruce", 2.9, 100, 30, 180, FALSE,
  "Set-aside", "Temperate",     "Old growth",    4.5, 100, 30, 280, TRUE,
  "Set-aside", "Mediterranean", "Cork oak",      2.0, 100, 30,  80, FALSE,

  # Coppice conversion: Chiti paper text ~8 MgCO2/ha/yr coppice-to-high-forest
  #   Campani 2022 (J Environ Manage 312) confirms SOC negligible at 50 yr
  "Coppice conversion", "Temperate",     "Chestnut/oak", 8.0, 11, 30, 140, TRUE,
  "Coppice conversion", "Mediterranean", "Holm oak",     8.0, 11, 30,  70, FALSE,

  # Continuous stock management: Eyvindson 2021 + LBC GFSC methodology
  #   10-20% harvest reduction; Hilmers 2020 Table 4 supports ~0.3-0.5 range
  "Continuous stock management", "Boreal",    "Mixed conifers",    0.30, 11, 30, 120, FALSE,
  "Continuous stock management", "Temperate", "Mixed broadleaves", 0.45, 11, 30, 250, TRUE,  # continuous cover ≈ biome mean; selective harvest keeps stock high

  # Forested peatland rewetting Temp: Mander et al. 2025 (New Phytol 246:94-102)
  #   natural peatlands 0.04-3.67 MgCO2/ha/yr; 2.0 upper-mid range
  #   Chiti Temp Peatland restoration (n=4) inconclusive (3/4 SOC emissions)
  "Forested peatland rewetting", "Temperate", "Drained peatland", 2.0, 135, 30, 150, FALSE,

  # Species diversification: Pretzsch & Schütze 2021 (Ann Bot 128:767-786)
  #   7-53% productivity uplift in 63 long-term German plots; median 30% of
  #   typical ~6 MgCO2/ha/yr baseline = +1.8. Cross-biome pending better data.
  "Species diversification", "Boreal",        "Climate-adapted mix", 1.8, 150, 30,  75, FALSE,
  "Species diversification", "Temperate",     "Climate-adapted mix", 1.8, 150, 30, 140, TRUE,
  "Species diversification", "Mediterranean", "Climate-adapted mix", 1.8, 150, 30,  55, FALSE,

  # Structural diversification (CCF): Hilmers 2020 (EJFR 139, Table 4)
  #   Bavarian transformation scenarios 1.2-2.7 MgCO2/ha/yr in-situ
  "Structural diversification", "Boreal",        "Mixed conifers", 1.0, 100, 30, 120, FALSE,
  "Structural diversification", "Temperate",     "Mixed species",  1.5, 100, 30, 230, TRUE,  # transition phase but high target state; ~80% of S_ref
  "Structural diversification", "Mediterranean", "Cork oak",       1.0, 100, 30,  60, FALSE,

  # Fuel management: Davis 2024 (For Ecol Manage 561) 62-72% severity reduction
  #   + Fernandes 2015 3-4:1 leverage. Probability-weighted avoided loss.
  "Fuel management", "Temperate",     "Mixed broadleaves", 1.5, 500, 15, 170, FALSE,
  "Fuel management", "Mediterranean", "Maritime pine",      4.0, 500, 15,  80, TRUE,

  # Peatland rewetting Temp: Mander 2025 (New Phytol 246:94-102) upper-range
  #   of natural peatland sink (0.04-3.67 MgCO2/ha/yr); PLC-scale 135 ha.
  "Peatland rewetting", "Temperate", "Paludiculture", 2.5, 135, 50, 60, FALSE,

  # Reforestation (HEURISTIC): ~10% of Productive afforestation rate.
  #   Post-disturbance restocking (LBC Reconstitution). No Chiti category.
  #   Treat as sensitivity parameter in manuscript.
  "Reforestation", "Boreal",        "Mixed conifers",    0.5, 11, 30, 120, FALSE,
  "Reforestation", "Temperate",     "Mixed broadleaves", 0.8, 11, 30, 160, TRUE,
  "Reforestation", "Mediterranean", "Maritime pine",      0.5, 11, 30,  55, FALSE,

  # Agroforestry Temp: Cardinael et al. 2017 (Ag Ecosyst Environ 236:243-255)
  #   French silvoarable walnut: tree biomass 0.65 MgC/ha/yr → AGB ≈ 1.9
  "Agroforestry", "Temperate", "Native broadleaves", 1.9, 15, 30, 100, FALSE,

  # Short-rotation plantation Med: Eucalyptus globulus coppice on cropland.
  #   Pérez-Cruzado et al. 2012 (Chiti Table 1): AGB = 28.3-40.0 MgCO2/ha
  #   at 10-20 yr = ~2.0 MgCO2/ha/yr. SOC negative at yr 10, turning
  #   positive by yr 15-20. b4est.eu: T_rot=10-12 yr, 2-3 coppice cycles.
  #   Novara et al. 2012: SOC = 1.79 MgCO2/ha/yr. AGB rate used (net of
  #   coppice regrowth). Conservative: AGB only, SOC excluded (negative early).
  "Short-rotation plantation", "Mediterranean", "Eucalyptus", 2.0, 47, 30, 40, TRUE,

  # /////////////////////////////////////////////////////////////////////////
  # SECTION C — BROADLEAF / CONIFER VARIANTS FOR ANCHOR PRACTICES
  # /////////////////////////////////////////////////////////////////////////
  # The seven practices below admit both broadleaf and conifer realisations
  # in EU operational practice. The existing anchor row carries the dominant
  # forest type for each practice (see Section A/B); the variant added here
  # carries the alternate type, marked is_anchor = TRUE so both forest-type
  # realisations appear in the headline rankings. Rates for variants without
  # a direct Chiti lookup are hardcoded from primary literature.

  # Extended rotation — broadleaf variant (beech/oak extended rotation,
  # e.g. Bavaria Buchenwald conversion). Pretzsch & Schutze 2009 (Ann Bot
  # 100) report 30-50% productivity uplift in extended-rotation beech.
  "Extended rotation", "Temperate", "Beech/oak", 3.5, 100, 30, 320, TRUE,

  # Reduced harvest intensity — broadleaf variant. Holscher et al. 2014
  # (For Ecol Manage 326): beech RHI 50% retention → 1.0-1.5 MgCO2/yr.
  "Reduced harvest intensity", "Temperate", "Mixed broadleaves", 1.2, 100, 30, 250, TRUE,

  # Set-aside — conifer variant (Sitka/spruce no-cut zones, German
  # "Bannwald"). Hilmers 2020 Table 4: spruce set-aside 2.5-3.5 MgCO2/yr.
  "Set-aside", "Temperate", "Norway spruce", 3.0, 100, 30, 200, TRUE,

  # Continuous stock management — conifer variant (CCF in Sitka/Norway
  # spruce). Hilmers 2020: continuous-cover spruce 0.25-0.35 MgCO2/yr.
  "Continuous stock management", "Temperate", "Mixed conifers", 0.30, 11, 30, 180, TRUE,

  # Protected afforestation — conifer variant (Caledonian-style native
  # pinewood; Forestry Commission Scotland Caledonian Forest Reserve
  # studies report 4-6 MgCO2/yr for natural regeneration of Scots pine).
  "Protected afforestation", "Temperate", "Native pinewood", 5.0, 47, 100, 150, TRUE,

  # Productive afforestation — broadleaf variant (oak/beech timber
  # plantations in France/Germany). Chiti afforestation cropland type A
  # (broadleaves) averages ~7 MgCO2/yr in temperate Europe.
  "Productive afforestation", "Temperate", "Productive oak/beech", 7.0, 47, 40, 280, TRUE,

  # Reforestation — conifer variant (post-disturbance replanting of
  # spruce/Douglas in storm/beetle gaps). Heuristic ~0.5 MgCO2/yr,
  # mirroring Boreal Reforestation Mixed conifers entry.
  "Reforestation", "Temperate", "Mixed conifers", 0.5, 11, 30, 150, TRUE
)

# --- Source tracking ---
ALL_PROJECTS$rate_source <- ifelse(is.na(ALL_PROJECTS$rate),
                                   "Chiti_Table1", "hardcoded")
ALL_PROJECTS$rate_n      <- NA_integer_
ALL_PROJECTS$rate_citation <- NA_character_

# --- Fill Chiti Table 1 rates into NA slots ---
if (!is.null(.chiti_lookup)) {
  for (i in which(ALL_PROJECTS$rate_source == "Chiti_Table1")) {
    prac  <- ALL_PROJECTS$practice[i]
    biome <- ALL_PROJECTS$biome[i]
    m <- .chiti_lookup[.chiti_lookup$practice == prac &
                       .chiti_lookup$biome == biome, ]
    if (nrow(m) >= 1) {
      ALL_PROJECTS$rate[i]          <- m$chiti_rate[1]
      ALL_PROJECTS$rate_n[i]        <- m$chiti_n[1]
      ALL_PROJECTS$rate_citation[i] <- m$chiti_detail[1]
    }
  }
  .n_filled <- sum(ALL_PROJECTS$rate_source == "Chiti_Table1" & !is.na(ALL_PROJECTS$rate))
  cat(sprintf("  Filled %d/%d Chiti Table 1 rates; %d literature-sourced\n",
              .n_filled, nrow(ALL_PROJECTS),
              sum(ALL_PROJECTS$rate_source == "hardcoded")))
  rm(.n_filled)
}

# Validate: no rates should remain NA after fill
.na_rates <- ALL_PROJECTS[is.na(ALL_PROJECTS$rate), c("practice", "biome")]
if (nrow(.na_rates) > 0) {
  stop("Missing rates after Chiti fill (is CSV present?):\n",
       paste(sprintf("  %s x %s", .na_rates$practice, .na_rates$biome),
             collapse = "\n"))
}
rm(.na_rates)

# --- Citations for hardcoded rates ---
.hc_citations <- c(
  "Reduced harvest intensity|Boreal"            = "Hynynen 2005 (Forest Ecol Manage 207): moderate-intensity differential",
  "Reduced harvest intensity|Temperate"         = "Hynynen 2005; Chiti Harvest Intensity Control-Moderate differential",
  "Set-aside|Boreal"                            = "Hynynen 2005 (Forest Ecol Manage 207, Fig 8): unmanaged Picea abies",
  "Set-aside|Temperate"                         = "Chiti synthesis: Temperate unmanaged-managed differential",
  "Set-aside|Mediterranean"                     = "Scaled estimate from Temperate/Boreal",
  "Reforestation|Boreal"                        = "Heuristic: ~10% of Productive afforestation rate",
  "Reforestation|Temperate"                     = "Heuristic: ~10% of Productive afforestation rate",
  "Reforestation|Mediterranean"                 = "Heuristic: ~10% of Productive afforestation rate",
  "Coppice conversion|Temperate"                = "Chiti et al. 2026 text: ~8 MgCO2/ha/yr coppice-to-high-forest",
  "Coppice conversion|Mediterranean"            = "Chiti et al. 2026 text: ~8 MgCO2/ha/yr coppice-to-high-forest",
  "Continuous stock management|Boreal"          = "LBC GFSC: 10-20% harvest reduction estimate",
  "Continuous stock management|Temperate"       = "LBC GFSC: 10-20% harvest reduction estimate",
  "Forested peatland rewetting|Temperate"       = "Mander et al. 2025 (New Phytol 246:94-102): natural peatland sink mid-range",
  "Species diversification|Boreal"              = "Pretzsch & Schuetze 2021 (Ann Bot 128:767-786): 30% mixing uplift",
  "Species diversification|Temperate"           = "Pretzsch & Schuetze 2021 (Ann Bot 128:767-786): 30% mixing uplift",
  "Species diversification|Mediterranean"       = "Pretzsch & Schuetze 2021 (Ann Bot 128:767-786): 30% mixing uplift",
  "Structural diversification|Boreal"           = "Chiti Hilmers 2020 net ~1.5; scaled for Boreal",
  "Structural diversification|Temperate"        = "Chiti Hilmers 2020 net ~1.5",
  "Structural diversification|Mediterranean"    = "Chiti Hilmers 2020 net ~1.5; scaled for Mediterranean",
  "Fuel management|Temperate"                   = "Probability-weighted avoided fire loss estimate",
  "Fuel management|Mediterranean"               = "Probability-weighted avoided fire loss ~4.0",
  "Peatland rewetting|Temperate"                = "Mander et al. 2025 (New Phytol 246:94-102): upper-range",
  "Agroforestry|Temperate"                      = "Cardinael et al. 2017 (Ag Ecosyst Environ 236:243-255): walnut AGB",
  "Short-rotation plantation|Mediterranean"     = "Perez-Cruzado et al. 2012 (Chiti Table 1): E. globulus AGB 28-40 MgCO2/ha at 10-20 yr"
)
# Variants that share (practice, biome) with an existing entry but use a
# different forest type carry their own citation; fall back to a generic
# label otherwise.
.hc_citations_variant <- c(
  "Extended rotation|Temperate|Beech/oak"          = "Pretzsch & Schutze 2009 (Ann Bot 100): beech extended-rotation productivity uplift",
  "Reduced harvest intensity|Temperate|Mixed broadleaves" = "Holscher 2014 (For Ecol Manage 326): beech RHI retention",
  "Set-aside|Temperate|Norway spruce"              = "Hilmers 2020 (EJFR 139, Table 4): spruce set-aside differential",
  "Continuous stock management|Temperate|Mixed conifers"  = "Hilmers 2020 (EJFR 139, Table 4): CCF spruce/Norway differential",
  "Protected afforestation|Temperate|Native pinewood"     = "Forestry Commission Scotland Caledonian Forest natural regen 4-6 MgCO2/yr",
  "Productive afforestation|Temperate|Productive oak/beech" = "Chiti afforestation cropland type A (broadleaves) ~7 MgCO2/yr",
  "Reforestation|Temperate|Mixed conifers"         = "Heuristic: post-disturbance CF restocking, mirrors Boreal Reforestation"
)
for (i in which(ALL_PROJECTS$rate_source == "hardcoded")) {
  key_variant <- paste(ALL_PROJECTS$practice[i], ALL_PROJECTS$biome[i],
                        ALL_PROJECTS$species[i], sep = "|")
  key_default <- paste(ALL_PROJECTS$practice[i], ALL_PROJECTS$biome[i], sep = "|")
  ALL_PROJECTS$rate_citation[i] <- if (key_variant %in% names(.hc_citations_variant)) {
    .hc_citations_variant[[key_variant]]
  } else if (key_default %in% names(.hc_citations)) {
    .hc_citations[[key_default]]
  } else {
    NA_character_
  }
}
rm(.hc_citations, .hc_citations_variant)

# Coverage summary
.n_chiti <- sum(ALL_PROJECTS$rate_source == "Chiti_Table1")
.n_hc    <- sum(ALL_PROJECTS$rate_source == "hardcoded")
cat(sprintf("  Rate sources: %d Chiti Table 1 (%.0f%%), %d hardcoded (%.0f%%)\n",
            .n_chiti, 100 * .n_chiti / nrow(ALL_PROJECTS),
            .n_hc,    100 * .n_hc    / nrow(ALL_PROJECTS)))
rm(.n_chiti, .n_hc, .chiti_lookup)

# --- Persist rate provenance record ---
# One row per rate: where it came from, the Chiti Table 1 subset + sample size
# (rate_n is the non-NA count actually averaged), and the citation. rate_n is NA
# for hardcoded literature rates. Used as the supplementary provenance audit.
.rate_provenance <- ALL_PROJECTS[, c("practice", "biome", "species", "rate",
                                     "rate_source", "rate_n", "rate_citation")]
write.csv(.rate_provenance,
          file.path(PATHS$output_tables, "rate_provenance.csv"),
          row.names = FALSE)
cat(sprintf("  Wrote rate provenance (%d rows) -> %s/rate_provenance.csv\n",
            nrow(.rate_provenance), PATHS$output_tables))
rm(.rate_provenance)

# Compute Q from decomposed components
ALL_PROJECTS$Q <- with(ALL_PROJECTS, rate * area_ha * duration_yr)

# =============================================================================
# SILVICULTURAL PARAMETERS FOR DYNAMIC x (zero-baseline practices)
# =============================================================================
# For practices with baseline harvest ≈ 0 (productive afforestation, short-
# rotation plantation), x = -H/Q is computed dynamically via
# calc_x_afforestation() using species- and biome-specific silvicultural params.
# Rows with NA silvicultural params use static x from PRACTICE_DEFS.
#
# T_rot_silv:  rotation length (yr)
# T_thin_silv: age of first thinning (yr); 0 = no thinning (coppice systems)
# f_thin_silv: fraction of total harvest from thinning (0 = clearfell only)
#
# Sources:
#   Scots pine:    Niemistö et al. 2018 (Silva Fennica 52:7816); Skogsstyrelsen
#   Sitka spruce:  Edwards & Christie 1981 (FC Booklet 48); Forest Research UK
#   Maritime pine: Alegria 2011 (Forest Systems 20:361-378)
#   Eucalyptus:    b4est.eu; Pérez-Cruzado et al. 2012
#   f_thin = 0.24: Skogsstyrelsen fellings 2019-2023 (national cross-species avg)
ALL_PROJECTS$T_rot_silv  <- NA_real_
ALL_PROJECTS$T_thin_silv <- NA_real_
ALL_PROJECTS$f_thin_silv <- NA_real_

# Productive afforestation — biome-specific silvicultural regimes
.pa <- ALL_PROJECTS$practice == "Productive afforestation"
ALL_PROJECTS$T_rot_silv[.pa & ALL_PROJECTS$biome == "Boreal"]        <- 80   # Scots pine (Niemistö 2018)
ALL_PROJECTS$T_thin_silv[.pa & ALL_PROJECTS$biome == "Boreal"]       <- 30   # Niemistö 2018: first thin at ~30 yr
ALL_PROJECTS$f_thin_silv[.pa & ALL_PROJECTS$biome == "Boreal"]       <- 0.24 # Skogsstyrelsen

ALL_PROJECTS$T_rot_silv[.pa & ALL_PROJECTS$biome == "Temperate"]     <- 40   # Sitka spruce (Edwards & Christie 1981)
ALL_PROJECTS$T_thin_silv[.pa & ALL_PROJECTS$biome == "Temperate"]    <- 18   # FC Booklet 48: YC 14-16
ALL_PROJECTS$f_thin_silv[.pa & ALL_PROJECTS$biome == "Temperate"]    <- 0.24 # Skogsstyrelsen (cross-species proxy)

ALL_PROJECTS$T_rot_silv[.pa & ALL_PROJECTS$biome == "Mediterranean"] <- 45   # Maritime pine (Alegria 2011)
ALL_PROJECTS$T_thin_silv[.pa & ALL_PROJECTS$biome == "Mediterranean"]<- 15   # Alegria 2011
ALL_PROJECTS$f_thin_silv[.pa & ALL_PROJECTS$biome == "Mediterranean"]<- 0.20 # Estimated (fewer thinnings than boreal)

# Short-rotation plantation — eucalyptus coppice, no thinning
.srp <- ALL_PROJECTS$practice == "Short-rotation plantation"
ALL_PROJECTS$T_rot_silv[.srp]  <- 12  # b4est.eu: 10-12 yr coppice cycle
ALL_PROJECTS$T_thin_silv[.srp] <- 0   # No thinning in eucalyptus coppice
ALL_PROJECTS$f_thin_silv[.srp] <- 0   # All harvest is clearfell

rm(.pa, .srp)

# Anchor-biome subset (main analysis) vs. full cross-biome set = ALL_PROJECTS
STYLISED_PROJECTS <- ALL_PROJECTS[ALL_PROJECTS$is_anchor, ]

# Practice harvest-class assignment (used in scenarios, figures, and summary)
HARVEST_CLASSES <- list(
  `Set-aside`                  = "Harvest-reducing",
  `Extended rotation`          = "Harvest-reducing",
  `Reduced harvest intensity`  = "Harvest-reducing",
  `Coppice conversion`         = "Harvest-reducing",
  `Continuous stock management`= "Harvest-reducing",
  `Forested peatland rewetting`= "Harvest-reducing",
  `Reforestation`              = "Harvest-increasing",
  `Structural diversification` = "Harvest-neutral",
  `Species diversification`    = "Harvest-neutral",
  `Fuel management`            = "Harvest-neutral",
  `Peatland rewetting`         = "Harvest-neutral",
  `Protected afforestation`    = "Harvest-neutral",
  `Productive afforestation`   = "Harvest-increasing",
  `Short-rotation plantation`  = "Harvest-increasing",
  `Site fertilisation`         = "Harvest-increasing",
  `Agroforestry`               = "Harvest-increasing"
)

# =============================================================================
# CRCF EU-WIDE DEPLOYMENT SCENARIOS (Regulation EU 2024/3012)
# =============================================================================
# 6 scenarios targeting CRCF_TARGET_MT MtCO2/yr (LULUCF shortfall).
# 3 pure pathways (one harvest class fills 100 Mt alone) +
# 3 mixed emphasis (60/25/15 split across classes).
# Rates and durations fixed from Chiti et al. (2026); only areas vary.

# --- Base practices: fixed rates, durations, and reference areas ---
CRCF_BASE_PRACTICES <- tibble::tribble(
  ~practice, ~rate, ~eu_area_ha, ~duration_yr, ~source,

  # Harvest-reducing: IFM + peatland (high market leakage risk)
  "Extended rotation",          3.5,  1500000, 20, "1.5% of ~100M ha managed forest",
  "Reduced harvest intensity",  1.0,  1000000, 10, "1% of managed forest",
  "Set-aside",                  4.5, 16000000, 30, "EU Biodiversity Strategy 10% strict protection",
  "Coppice conversion",         8.0,  2000000, 30, "~2M ha EU coppice eligible",
  "Continuous stock management", 0.45,  500000, 20, "LBC GFSC + similar EU schemes",
  "Forested peatland rewetting", 2.5,  2000000, 30, "Subset of 7.5M ha EU rewetting target",

  # Harvest-neutral: diversification, fire, peatland, protected afforestation
  "Species diversification",     3.5,  5000000, 30, "5% of managed forest (Waldumbau-type)",
  "Structural diversification",  1.5,  3000000, 30, "3% of managed forest (CCF conversion)",
  "Fuel management",             4.0,  5000000, 15, "~25% of Mediterranean forest (~20M ha)",
  "Peatland rewetting",          3.4,  5000000, 50, "30% of ~25M ha EU drained peatland",
  "Protected afforestation",     8.0,  1000000, 100, "Native woodland creation (WCC-type)",

  # Harvest-increasing: supply-positive (negative leakage potential)
  "Reforestation",               0.8,  1000000, 30, "[PRELIMINARY] post-disturbance restocking",
  "Productive afforestation",    8.0,  1000000, 40, "Commercial plantation, ~1M ha",
  "Site fertilisation",          5.0,   500000, 10, "Nordic drained peatland forestry",
  "Agroforestry",                2.0,  1000000, 25, "EU agroforestry expansion target"
)
# Sync CRCF rates with anchor-biome rates from ALL_PROJECTS (post-Chiti override)
.n_synced <- 0
for (i in seq_len(nrow(CRCF_BASE_PRACTICES))) {
  prac <- CRCF_BASE_PRACTICES$practice[i]
  anchor <- ALL_PROJECTS[ALL_PROJECTS$practice == prac & ALL_PROJECTS$is_anchor, ]
  if (nrow(anchor) == 1 && anchor$rate != CRCF_BASE_PRACTICES$rate[i]) {
    CRCF_BASE_PRACTICES$rate[i] <- anchor$rate
    .n_synced <- .n_synced + 1
  }
}
if (.n_synced > 0) cat(sprintf("  Synced %d CRCF base rates with ALL_PROJECTS anchors\n", .n_synced))
rm(.n_synced)

CRCF_BASE_PRACTICES$harvest_class <- sapply(
  CRCF_BASE_PRACTICES$practice,
  function(p) HARVEST_CLASSES[[p]]
)

# --- EU forest area per biome (Senf & Seidl 2021, Forest Europe 2020) ---
# Used to split scenario areas across biomes proportional to forest extent
.senf_biome <- readRDS(file.path("data/processed", "senf_biome_rates.rds"))
.senf_biome <- .senf_biome[.senf_biome$biome != "Temperate_UK", ]
BIOME_FOREST_KHA <- setNames(.senf_biome$forest_kha, .senf_biome$biome)
BIOME_FOREST_SHARES <- BIOME_FOREST_KHA / sum(BIOME_FOREST_KHA)
rm(.senf_biome)

# Practice → plausible biomes (from ALL_PROJECTS)
PRACTICE_BIOMES <- tapply(
  ALL_PROJECTS$biome, ALL_PROJECTS$practice, unique, simplify = FALSE
)
# Remove Temperate_UK from biome lists
PRACTICE_BIOMES <- lapply(PRACTICE_BIOMES, function(b) b[b != "Temperate_UK"])

# --- Factory: scale areas per harvest class, split across biomes ---
build_crcf_scenario <- function(base, target_mt, class_weights) {
  class_map <- c(
    "Harvest-reducing"  = "reducing",
    "Harvest-neutral"   = "neutral",
    "Harvest-increasing" = "increasing"
  )
  # Step 1: scale total area per class to hit target
  out <- base
  for (hclass in names(class_map)) {
    key <- class_map[[hclass]]
    idx <- out$harvest_class == hclass
    current_class_mt <- sum(out$rate[idx] * out$eu_area_ha[idx] / 1e6)
    if (current_class_mt > 0 && class_weights[[key]] > 0) {
      target_class_mt <- target_mt * class_weights[[key]]
      out$eu_area_ha[idx] <- out$eu_area_ha[idx] * (target_class_mt / current_class_mt)
    } else if (class_weights[[key]] == 0) {
      out$eu_area_ha[idx] <- 0
    }
  }

  # Step 2: expand each practice across its plausible biomes,
  # splitting area by biome forest shares (re-normalised within plausible set)
  expanded <- do.call(rbind, lapply(seq_len(nrow(out)), function(i) {
    row <- out[i, ]
    biomes <- PRACTICE_BIOMES[[row$practice]]
    if (is.null(biomes)) biomes <- "Temperate"
    shares <- BIOME_FOREST_SHARES[biomes]
    shares <- shares / sum(shares)  # re-normalise to plausible biomes
    do.call(rbind, lapply(seq_along(biomes), function(j) {
      r <- row
      r$biome <- biomes[j]
      r$eu_area_ha <- row$eu_area_ha * shares[j]
      r
    }))
  }))

  expanded$eu_Q_total       <- with(expanded, rate * eu_area_ha * duration_yr)
  expanded$eu_annual_MtCO2  <- with(expanded, rate * eu_area_ha / 1e6)
  expanded
}

# --- Build 6 scenarios ---
CRCF_SCENARIO_LIST <- list(
  # Pure pathways: one class delivers 100% of the target
  reducing_only    = build_crcf_scenario(CRCF_BASE_PRACTICES, CRCF_TARGET_MT,
                       c(reducing = 1.00, neutral = 0.00, increasing = 0.00)),
  neutral_only     = build_crcf_scenario(CRCF_BASE_PRACTICES, CRCF_TARGET_MT,
                       c(reducing = 0.00, neutral = 1.00, increasing = 0.00)),
  increasing_only  = build_crcf_scenario(CRCF_BASE_PRACTICES, CRCF_TARGET_MT,
                       c(reducing = 0.00, neutral = 0.00, increasing = 1.00)),
  # Mixed emphasis: 60/25/15 split
  mixed_reducing   = build_crcf_scenario(CRCF_BASE_PRACTICES, CRCF_TARGET_MT,
                       c(reducing = 0.60, neutral = 0.25, increasing = 0.15)),
  mixed_neutral    = build_crcf_scenario(CRCF_BASE_PRACTICES, CRCF_TARGET_MT,
                       c(reducing = 0.25, neutral = 0.60, increasing = 0.15)),
  mixed_increasing = build_crcf_scenario(CRCF_BASE_PRACTICES, CRCF_TARGET_MT,
                       c(reducing = 0.25, neutral = 0.15, increasing = 0.60))
)

# --- Activate selected scenario ---
stopifnot(ACTIVE_SCENARIO %in% names(CRCF_SCENARIO_LIST))
CRCF_EU_SCENARIOS <- CRCF_SCENARIO_LIST[[ACTIVE_SCENARIO]]

# Report scenario summary
cat(sprintf("  CRCF scenario: %s (%.0f MtCO2/yr, %.1f Mha)\n",
            ACTIVE_SCENARIO,
            sum(CRCF_EU_SCENARIOS$eu_annual_MtCO2),
            sum(CRCF_EU_SCENARIOS$eu_area_ha) / 1e6))

# Feasibility warning
if (ACTIVE_SCENARIO == "increasing_only") {
  cat(sprintf("  WARNING: increasing_only requires %.1f Mha (EU afforestation target: 2 Mha)\n",
              sum(CRCF_EU_SCENARIOS$eu_area_ha) / 1e6))
}

# Drop harvest_class before passing downstream (match expected schema)
CRCF_EU_SCENARIOS$harvest_class <- NULL

# =============================================================================
# PRACTICE_DEFS — practice-specific parameters (Table S1 in paper)
# =============================================================================
# harvest_displacement: +ve = reduces harvest, -ve = increases
# R_mult: vulnerability multiplier (severity scaling, enters as R^alpha)
# lambda_mult: frequency multiplier (probability scaling)
# c_mult: correlation multiplier (spatial/temporal clustering)

PRACTICE_DEFS <- tibble::tribble(
  ~practice_id, ~practice_name,
  ~harvest_displacement, ~product_class,
  ~phi_add, ~R_mult, ~lambda_mult, ~c_mult,

  # --- HARVEST-REDUCING ---
  # x = fraction of Q attributable to harvest reduction (leakage-vulnerable).
  #   For pure harvest-reducing practices, the dominant mechanism is retained
  #   standing stock: carbon is there because wood was not removed. A minority
  #   share (~10%) represents old-growth compounding — large unharvested trees
  #   accumulate at higher absolute rates than the younger regeneration cohorts
  #   that would follow baseline harvests (Stephenson et al. 2014, Nature;
  #   Luyssaert et al. 2008, Nature). This attribution is conservative and
  #   applied uniformly (x=0.90) to all harvest-reducing practices; no
  #   empirical basis exists to differentiate further. Sensitivity: Table S5
  #   varies x by +/-50%.
  #
  # Extended rotation: R=1.20 older stands more vulnerable
  #   (Valinger & Fridman 2011: windthrow >2x age 50→90)
  "ExtendedRotation", "Extended rotation",
  0.90, "SL", 0, 1.20, 1.10, 1.00,

  # Reduced harvest intensity: PW-dominated; R=1.15 higher stand density
  #   (Bradford 2022, Sohn 2016 meta-analysis)
  "ReducedHarvestIntensity", "Reduced harvest intensity",
  0.90, "PW", 0, 1.15, 1.05, 1.00,

  # Set-aside: Krüger et al. (2025, n=314):
  #   22% lower disturbance rate, 32% lower severity → R=1.00 (conservative neutral)
  "SetAside", "Set-aside",
  0.90, "SL", 0, 1.00, 1.00, 1.10,

  # Reforestation [PRELIMINARY]: LBC Reconstitution; adds future supply
  "Reforestation", "Reforestation",
  -0.03, "SL", 0, 0.95, 0.95, 0.90,

  # Coppice conversion: LBC Balivage method — converts coppice to high forest.
  #   x=0.40: coppice clearcut foregone (~40% of Q is harvest-attributable),
  #   but the structural transformation from short multi-stemmed coppice to
  #   tall single-stemmed high forest with understory layers fundamentally
  #   increases carbon carrying capacity (~60% of Q).
  "CoppiceConversion", "Coppice conversion",
  0.40, "SL", 0, 1.05, 1.00, 1.00,

  # Continuous stock management: LBC GFSC; maintains 80-90% of harvestable volume.
  #   x=0.90: conservative — same as other harvest-reducing practices.
  #   Evidence for carrying-capacity gains from vertical layering is
  #   insufficient to justify a lower x (Laiho et al. 2011, Forestry).
  "ContinuousStockManagement", "Continuous stock management",
  0.90, "SL", 0, 0.95, 0.95, 0.95,

  # Forested peatland rewetting: timber harvest ceases when peatland is rewetted.
  #   Q includes both SOC stabilisation (~70-85% of benefit) and AGB effects.
  #   x=0.15: only the harvest-attributable share of Q is leakage-vulnerable;
  #   the dominant SOC channel is independent of timber markets (Ojanen &
  #   Minkkinen 2020; Simola et al. 2021: peat/tree C ratio 4-25x).
  #   R=0.90 rewetting stabilises peat but transition mortality persists.
  "ForestPeatlandRewetting", "Forested peatland rewetting",
  0.15, "PW", 0, 0.90, 0.95, 0.90,

  # --- HARVEST-NEUTRAL ---
  # Species diversification: Jactel & Brockerhoff (2007) d=-0.67; Berthelot (2021)
  #   GLM: bark beetle damage decreases with tree species richness (community-level
  #   p=0.089, non-significant). R=0.80 (20% severity reduction), lambda=0.85
  "SpeciesDiversification", "Species diversification",
  0.00, "SL", 0, 0.80, 0.85, 0.95,

  # Structural diversification (CCF): Mohr et al. (2024, remote sensing, 41,350 ha,
  #   4 Austrian forestry companies): frequency 36.3% lower, severity only 3.8% lower.
  #   R=0.95, lambda=0.80. x=0.30: CCF conversion involves some harvest adjustment
  #   (~30% of Q), but the dominant mechanism is vertical structural layering that
  #   increases carrying capacity via complementary light capture (Pretzsch 2009:
  #   10-59% overyielding; Dong et al. 2025 PNAS: canopy complexity drives
  #   productivity). ~70% of Q from structural enhancement.
  "StructuralDiversification", "Structural diversification",
  0.30, "SL", 0, 0.95, 0.80, 0.90,

  # Fuel management: Davis et al. (2024) Tamm Review: severity -62-72%;
  #   Fernandes (2015) 3-4:1 leverage. R=0.60, lambda=0.55
  "FuelManagement", "Fuel management",
  0.00, NA_character_, 0, 0.60, 0.55, 0.70,

  # Peatland rewetting: Kettridge (2015) drained peat burns 2.7× deeper;
  #   Taufik (2023) ~40% fewer extreme fire events post-rewetting (area only -5%).
  #   R=0.70, lambda=0.85
  "PeatlandRewetting", "Peatland rewetting",
  0.00, NA_character_, 0, 0.70, 0.85, 0.80,

  # --- HARVEST-INCREASING ---
  # Protected afforestation: WCC native woodland, min intervention; no harvest
  "ProtectedAfforestation", "Protected afforestation",
  0.00, NA_character_, 0, 0.90, 0.90, 0.90,

  # Productive afforestation: WCC productive conifer; x = -H/Q computed
  #   dynamically via calc_x_afforestation() (placeholder value not used)
  "ProductiveAfforestation", "Productive afforestation",
  -0.10, "SL", 0, 0.90, 0.90, 0.90,

  # Site fertilisation: Walter (2021) N-fertilisation increases windstorm damage
  #   (+46% BA damage, +52% stems; R=1.10 conservative lower bound)
  #   x = -H/Q ≈ -0.05 (fertilisation increases growth ~10-20%; harvest of
  #   that increment relative to net sequestration retained)
  "SiteFertilisation", "Site fertilisation",
  -0.05, "SL", 0, 1.10, 1.00, 1.00,

  # Agroforestry: open-grown trees, minimal harvest during crediting period;
  #   x = -H/Q ≈ -0.05 (prunings/coppice products relative to net AGB gain)
  "Agroforestry", "Agroforestry",
  -0.05, "WF", 0, 0.75, 0.80, 0.85,

  # Short-rotation plantation: Eucalyptus globulus coppice (Portugal/Spain) or
  #   poplar hybrids (Italy). T_rot=10-12 yr, no thinning, 2-3 coppice cycles
  #   per 30 yr. x = -H/Q computed dynamically via calc_x_afforestation().
  #   High fire risk in Mediterranean (R=1.25); PW product class.
  #   Sources: b4est.eu; Pérez-Cruzado et al. 2012; FAO Unasylva No. 91
  "ShortRotationPlantation", "Short-rotation plantation",
  -0.50, "PW", 0, 1.25, 1.20, 1.00
)

# =============================================================================
# PRACTICE LOOKUP HELPERS
# =============================================================================

# Storage durations (yr). Default 30; deviations require biophysical justification.
.tau_2_map <- c(
  ExtendedRotation = 30, ReducedHarvestIntensity = 30, SetAside = 30,
  Reforestation = 30, CoppiceConversion = 30, ContinuousStockManagement = 30,
  ForestPeatlandRewetting = 30, SpeciesDiversification = 30,
  StructuralDiversification = 30, Agroforestry = 30,
  FuelManagement = 15,           # fuel re-accumulation cycle
  SiteFertilisation = 10,        # nutrient pulse effect
  ProtectedAfforestation = 100,  # WCC native woodland
  ProductiveAfforestation = 40,  # sigmoid stock curve / canopy closure
  PeatlandRewetting = 50,        # irreversible rewetting / peat recovery
  ShortRotationPlantation = 30   # eucalyptus/poplar coppice cycles
)
PRACTICE_DEFS$tau_2 <- unname(.tau_2_map[PRACTICE_DEFS$practice_id])
rm(.tau_2_map)

# Legal-land-use protection flag. TRUE only for practices that ESTABLISH a
# new forest land-use classification on previously non-forest land
# (afforestation on cropland/grassland/scrub). The project itself triggers
# the reclassification; thereafter national forest law prohibits
# deforestation and mandates replanting after harvest. The H_perm
# extension applies because the long-horizon permanence claim is
# project-induced.
#
# Reforestation is FALSE: replanting after disturbance on land that is
# already legally classified as forest does not change the land's legal
# status. The replant obligation (BWaldG §11, Code forestier Art. L124-6,
# Skogsvardslagen, etc.) was binding before the project; the project's
# marginal contribution to permanence is therefore not legally distinct
# from the baseline. Reforestation is credited at its contract-length
# tau_2 like other management practices.
#
# All non-afforestation practices remain bounded by their contract length.
.legally_protected_map <- c(
  ProtectedAfforestation   = TRUE,
  ProductiveAfforestation  = TRUE,
  Reforestation            = FALSE,  # land already forest pre-project; permanence not project-induced
  ShortRotationPlantation  = FALSE,  # crop-like rotation, not classified as forest
  ExtendedRotation         = FALSE,
  ReducedHarvestIntensity  = FALSE,
  SetAside                 = FALSE,
  CoppiceConversion        = FALSE,
  ContinuousStockManagement = FALSE,
  ForestPeatlandRewetting  = FALSE,
  SpeciesDiversification   = FALSE,
  StructuralDiversification = FALSE,
  FuelManagement           = FALSE,
  PeatlandRewetting        = FALSE,
  SiteFertilisation        = FALSE,
  Agroforestry             = FALSE
)
PRACTICE_DEFS$legally_protected_landuse <- unname(
  .legally_protected_map[PRACTICE_DEFS$practice_id]
)
rm(.legally_protected_map)

# Establishment-risk flag. TRUE for practices that ESTABLISH a new stand from
# bare/disturbed ground and therefore carry first-decade establishment-failure
# risk (planting/seedling mortality, frost, drought, browsing) that the mature-
# forest EFDA bootstrap does not observe: afforestation AND reforestation
# (post-disturbance replanting). This is SEPARATE from legally_protected_landuse
# (which governs the permanence HORIZON, H_perm): reforestation is NOT legally
# protected (permanence not project-induced) but IS newly established, so it
# carries the establishment floor on its contract-horizon buffer. The floor is
# applied in empirical_buffer() via EST_FLOOR_AFFOREST_BY_BIOME.
.establishment_risk_map <- c(
  ProtectedAfforestation   = TRUE,
  ProductiveAfforestation  = TRUE,
  Reforestation            = TRUE,   # post-disturbance replanting: real establishment risk
  ShortRotationPlantation  = FALSE,  # crop-like coppice rotation, not stand establishment
  ExtendedRotation         = FALSE,
  ReducedHarvestIntensity  = FALSE,
  SetAside                 = FALSE,
  CoppiceConversion        = FALSE,
  ContinuousStockManagement = FALSE,
  ForestPeatlandRewetting  = FALSE,
  SpeciesDiversification   = FALSE,
  StructuralDiversification = FALSE,
  FuelManagement           = FALSE,
  PeatlandRewetting        = FALSE,
  SiteFertilisation        = FALSE,
  Agroforestry             = FALSE
)
PRACTICE_DEFS$establishment_risk <- unname(
  .establishment_risk_map[PRACTICE_DEFS$practice_id]
)
rm(.establishment_risk_map)

#' Get practice definition row
#' @param practice Practice name (display) or ID (internal)
#' @return Single-row tibble or NULL if not found
get_practice_def <- function(practice, practice_defs = PRACTICE_DEFS) {
  # Try by display name first
  idx <- match(practice, practice_defs$practice_name)
  if (is.na(idx)) {
    # Try by internal ID
    idx <- match(practice, practice_defs$practice_id)
  }
  if (is.na(idx)) {
    warning(paste("Practice not found:", practice))
    return(NULL)
  }
  practice_defs[idx, ]
}

# =============================================================================
# CANONICAL FALLBACK CONSTANTS
# =============================================================================
# Single-source defaults referenced by 04_functions.R, 09_scheme_comparison.R,
# and 15_integrity_gap_sensitivity.R. Matches Temperate SL product class.

DEFAULT_LEAK_PARAMS <- list(
  u = 0.60, s = 0.90, eps_d = -0.45,
  eps_s_dom = 0.65, eps_s_imp = 1.10
)

DEFAULT_TRADE_SHARES <- list(alpha_dom = 0.60, alpha_imp = 0.40)

# =============================================================================
# EXISTING SCHEME PARAMETERS — from official documentation
# =============================================================================
# See data/external/scheme_parameters.csv for full audit trail.

SCHEME_PARAMS <- list(
  # Label Bas-Carbone (France) — 4 forest methods: Boisement, Reconstitution,
  # Balivage, GFSC. ~1200 projects, 3.3 MtCO2 (I4CE 2025).
  # Boisement/Reconstitution/Balivage: 0% leakage, 10-25% risk, 40% BASI penalty
  # GFSC: 5% leakage (fuite), 10% general + 0-15% data + 0-30% fire risk
  LBC = list(
    name = "Label Bas-Carbone",
    country = "FR",
    buffer_rate = 0.15,       # BIOME-BLENDED NATIONAL AVERAGE, not a temperate-only number:
                              # Boisement/Reconstitution carry 10% base in non-fire zones and up to
                              # 10%+15%=25% in fire zones; a national portfolio of ~2/3 non-fire (10%)
                              # + ~1/3 fire-zone (~25%) blends to ~15%. Do NOT "correct" this down to
                              # 10% just because the framework side is evaluated on Temperate anchors
                              # (is_anchor) — that would assume zero fire-zone projects and OVER-state
                              # the gap. Structurally a PERMANENT rabais (deduction), not a reversible
                              # buffer; this 15% is the non-permanence slice only. VALIDATION:
                              # combined scheme deduction (15% buffer x 5% leakage = ~19%) reconciles
                              # with I4CE 2025 empirical ~22% total haircut (10-39% range) — the extra
                              # ~3pp is the 40% BASI additionality penalty, which NCV does not price
                              # via the buffer channel. Do NOT substitute 22% into buffer_rate.
    leakage_rate = 0.05,      # 5% leakage for GFSC only; Boisement/Reconstitution/Balivage have 0%
    liability_years = 30,     # 30 yr for Boisement, Reconstitution, Balivage
    liability_years_min = 20, # 20 yr for GFSC method (Apr 2025)
    notes = "Risk discount approach; 20-30 yr; 40% BASI additionality penalty if not demonstrated",
    covered_practices = c(
      "Productive afforestation",        # Boisement: 60% monoculture plots (WWF 2021), avg 5 species
      "Reforestation",                   # Reconstitution (degraded stand restocking)
      "Coppice conversion",              # Balivage (taillis → futaie sur souches) [4 projects, 2.3 ktCO2e]
      "Continuous stock management"      # GFSC (maintain 80-90% harvestable volume) [newer method]
    ),
    # Cross-practice credit-share weights (I4CE / Observatoire de la forêt
    # & resoilag bilan LBC 2023):
    #   Boisement       352 projects, 604 426 tCO2e (~44% of credits)
    #   Reconstitution  319 projects, 773 394 tCO2e (~56%)
    #   Balivage          4 projects,   2 323 tCO2e (<0.2%, negligible)
    #   GFSC             newer method, share not yet material
    practice_weights = c(
      "Productive afforestation"   = 0.44,
      "Reforestation"              = 0.56,
      "Coppice conversion"         = 0.00,
      "Continuous stock management" = 0.00
    ),
    # Forest-type weights within each covered practice. Boisement projects
    # are CF-dominated (WWF 2021 noted 60% monoculture plots, primarily
    # Douglas/Maritime pine); Reconstitution is post-disturbance replanting,
    # dominated by CF stands that suffered storm/beetle losses; Balivage is
    # BL-only by definition (coppice = broadleaf). GFSC defaults to national
    # French forest mix (~67% BL / 33% CF, IGN inventory).
    forest_type_weights = list(
      "Productive afforestation"  = c(broadleaf = 0.30, conifer = 0.70),
      "Reforestation"             = c(broadleaf = 0.30, conifer = 0.70),
      "Coppice conversion"        = c(broadleaf = 1.00, conifer = 0.00),
      "Continuous stock management" = c(broadleaf = 0.67, conifer = 0.33)
    )
  ),
  
  # Woodland Carbon Code (UK) — woodland creation only, 845 projects (Sep 2025)
  WCC = list(
    name = "Woodland Carbon Code",
    country = "UK",
    buffer_rate = 0.20,       # Flat 20% to shared pool (Version 2.0+)
    leakage_rate = 0.00,      # Activity-shifting disclosure only; no market leakage
    liability_years = 100,    # Max crediting period
    liability_years_min = 40, # Min crediting period (v3.0)
    liability_beta_shape = c(2, 3), # Right-skewed: mode 60, median 63 (Forest Carbon evidence)
    notes = "Pooled buffer; woodland creation only; 40-100 yr crediting period",
    covered_practices = c(
      "Protected afforestation",   # WCC native woodland (minimum intervention)
      "Productive afforestation"   # WCC productive conifer (thinning + clearfell)
    ),
    # Carbon-share-weighted UK portfolio mix.
    # Scotland accounts for 83% of WCC projected sequestration (10.8/13.0 Mt
    # CO2; Forest Research Forestry Statistics 2025, Ch.4); the Scottish
    # Government EIR release FOI/202400398009 (31 Dec 2023) reports the
    # Scotland WCC project breakdown as 30% broadleaved (>80% BL) + 11%
    # mixed mainly broadleaved (50-80% BL) + 23% mixed mainly conifer
    # (50-80% C) + 37% conifer (>80% C). England (~12% of WCC carbon) is
    # ~90% broadleaf / ~10% conifer in 2023-24 woodland-creation statistics
    # (Forestry Commission). Wales+NI (~5%) assigned 50/50 mid (no
    # disaggregated data). Carbon-share weighting yields ~47% broadleaf-
    # dominant ("Protected") and ~53% conifer-dominant ("Productive") for
    # the UK WCC portfolio.
    practice_weights = c(
      "Protected afforestation"  = 0.47,
      "Productive afforestation" = 0.53
    ),
    # Forest-type weights within each covered practice. WCC's protected
    # ("native woodland creation") arm is predominantly broadleaf (oak,
    # birch, mixed deciduous); native pinewood projects are a minority.
    # WCC's productive arm is overwhelmingly Sitka spruce in Scotland
    # (Broadmeadows-type plantings ~86% conifer).
    forest_type_weights = list(
      "Protected afforestation"  = c(broadleaf = 0.90, conifer = 0.10),
      "Productive afforestation" = c(broadleaf = 0.15, conifer = 0.85)
    )
  ),
  
  # Peatland Code (UK) — peatland restoration. Verified against Peatland Code
  # v2.1 (PDF dated 2024-11, reissued 2025-09), 2026-06-18.
  PLC = list(
    name = "Peatland Code",
    country = "UK",
    buffer_rate = 0.20,       # 20% of NET reductions to shared Risk Buffer (v2.1).
                              # NOTE: net = gross − carbon-cost (5% if not calc'd) − 5%
                              # conservative buffer − leakage, so total discount from gross
                              # is ~28%, not 20%. buffer_rate here = the non-permanence
                              # buffer only; the 5%+5% are separate deductions — DECIDE whether
                              # the model should capture them too.
    leakage_rate = 0.00,      # Default zero, but CONDITIONAL: quantified only if displacement
                              # is >=5% of project sequestration over the duration (v2.1).
    liability_years = 100,    # Practical max. NOT a Code-fixed maximum — duration is bounded
                              # by peat-depth depletion under the do-nothing baseline; 100 yr
                              # is a worked example (fen needing >=1.5 m peat).
    liability_years_min = 30, # Min project duration (v2.1) [missing: actual distribution]
    notes = "Peatland restoration (revegetate and/or rewet); 30 yr min, max peat-depth-bounded (~100 yr)",
    covered_practices = c(
      "Peatland rewetting"        # Code wording: "revegetate and/or rewet" degraded bog/fen
    )
  ),
  
  # Wald-Klima-Standard (Germany) — forest restoration/conversion.
  # Operator: eva / Ecosystem Value Alliance (eva.eco). Verified against the
  # WKS standard (standard.eva.eco, v0.4–v1.3, latest 14 May 2026), 2026-06-18.
  # Methods we model: M01 Wald-Wiederaufbau (reforestation of calamity areas)
  # and M02 Waldumbau (species AND structural diversification of managed
  # forests). M03 Klimaoptimiertes Forstbetriebsmanagement (reduced-harvest
  # carbon-stock management) is NOT modelled here.
  WKS = list(
    name = "Wald-Klima-Standard",
    country = "DE",
    buffer_rate = 0.15,       # Flat 15% permanence-buffer base contribution (Basisbeitrag),
                              # all methods (owner credited 85%); WKS std indicator 7.1.1.
                              # M03-only project risk surcharge not modelled. [was 0.20: that
                              # conflated the 15% buffer with eva's separate ~15% fee]
    leakage_rate = 0.00,      # 0% for the methods we model (M01, M02): activity-shifting 0%
                              # all methods; market leakage 0% for M01/M02, 5% only for M03
                              # (not modelled). WKS std §6.7. [was 0.05: applied only to M03]
    liability_years = 30,     # M02 fixed at 30 yr; M01 selectable 20/25/30 (30 recommended)
    liability_years_min = 20, # M01 lower bound (WKS std indicator 1.3.4)
    notes = "15% flat permanence buffer; M01 20-30 yr (30 rec.), M02 fixed 30 yr; leakage 0% for modelled M01/M02",
    covered_practices = c(
      "Protected afforestation",     # M01 Wald-Wiederaufbau: reforestation of calamity areas
      "Species diversification",     # M02 Waldumbau (species diversification)
      "Structural diversification"   # M02 Waldumbau (stand-structure diversification)
    ),
    # German Wald-Klima-Standard targets post-bark-beetle / post-storm
    # reconstruction, dominated by formerly Norway-spruce sites (~70% CF).
    # Species/structural diversification primarily transitions CF
    # monocultures toward mixed forests (Waldumbau).
    forest_type_weights = list(
      "Protected afforestation"     = c(broadleaf = 0.30, conifer = 0.70),
      "Structural diversification"  = c(broadleaf = 0.30, conifer = 0.70)
    )
  ),
  
  # Klimaskovfonden (Denmark) — afforestation + wetlands. Core params verified
  # against KSF methodology VERSION 3 (Jan 2026) + standard pages, 2026-06-19.
  # Portfolio totals (~251 proj / ~2,823 ha / ~1.02 MtCO2, Mar 2026) live in
  # external/scheme_parameters.csv and drift; live Jun 2026 was lower
  # (241 forests / 2,740 ha / 981,490 tCO2) — refresh with explicit access date.
  KSF = list(
    name = "Klimaskovfonden",
    country = "DK",
    buffer_rate = 0.15,       # 15% flat to fælles buffer, all project types (owner 85%)
    leakage_rate = 0.00,      # National boundary only; imports assumed external
    liability_years = 100,    # Forest reserve status = permanent (fredskovspligt)
    liability_years_min = 80,  # Effective lower bound: statute could be amended
    liability_beta_shape = c(5, 1), # Beta(5,1) on [80,100]: skewed toward 100
    notes = "National boundary assumption; afforestation and wetlands",
    covered_practices = c(
      "Protected afforestation",   # Primary activity: permanent native forest reserves (~97%)
      "Peatland rewetting"         # Low-lying areas with natural water levels (~3%)
    ),
    # Danish KSF favours climate-resilient mixed-species afforestation
    # (Bækgaard pilot: BL + CF mix). Danish forest is ~50/50 nationally;
    # KSF projects skew slightly toward broadleaf (climate-adaptation goal).
    forest_type_weights = list(
      "Protected afforestation" = c(broadleaf = 0.55, conifer = 0.45)
    )
  ),
  
  # California USFP — 172 forest projects (150 IFM, 13 AC, 9 AR), IFM 93% of credits (VROD v2025-12); buffer pool ~95% depleted.
  # Buffer excluded from integrity gap (no US biome params; see SCHEME_EXCLUDE_BUFFER).
  CA_USFP = list(
    name = "California USFP",
    country = "US",
    buffer_rate = 0.134,      # Risk-rated pool (observed average Jan 2022)
    leakage_rate = 0.20,      # Analytical estimate of market leakage (IFM).
                              # CARB protocol does not quantify market leakage;
                              # 20% reflects Murray et al. (2004) for federal timber
                              # restrictions, applied here as a correction.
    liability_years = 100,    # 100-year permanence obligation (nominal, protocol-fixed)
    liability_years_min = 40,  # Effective lower bound: contractual, buffer nearly depleted
    notes = "Disaggregated risk-rated buffer (8.7-19.2%, avg 13.4%); IFM dominates issuance",
    covered_practices = c(
      "Protected afforestation",   # Reforestation: 9 projects, 0 credits issued (VROD v2025-12)
      "Extended rotation",   # IFM - increasing rotation ages [93% of forest credits, 150 projects]
      "Reduced harvest intensity",      # IFM - increasing stocking [93% of forest credits, 150 projects]
      "Set-aside"          # Avoided Conversion: 13 projects, 7% of forest credits
    ),
    # US Pacific Northwest IFM projects are conifer-dominated (Douglas-fir,
    # western hemlock, Sitka spruce). Southeast IFM projects include loblolly
    # pine and mixed hardwoods. Aggregate USFP ~85% CF / 15% BL.
    forest_type_weights = list(
      "Protected afforestation"     = c(broadleaf = 0.15, conifer = 0.85),
      "Extended rotation"           = c(broadleaf = 0.15, conifer = 0.85),
      "Reduced harvest intensity"   = c(broadleaf = 0.15, conifer = 0.85),
      "Set-aside"                   = c(broadleaf = 0.15, conifer = 0.85)
    )
  )
)

# Practices NOT covered by any scheme in our comparison:
# - Site fertilisation: No scheme credits this as standalone practice
# - Fuel management: Risk mitigation, not creditable activity

# =============================================================================
# DISCOUNT RATE SCENARIOS — Groom & Venmans (2023)
# =============================================================================
DISCOUNT_SCENARIOS <- tibble::tribble(
  ~scenario, ~r, ~g, ~rationale,
  "Low (Stern)", 0.015, 0.010, "Stern Review approach, intergenerational equity",
  "Central", 0.030, 0.020, "Groom & Venmans (2023): r=3.2%, g=GDP growth≈2%",
  "High (Traditional)", 0.045, 0.020, "Traditional cost-benefit, market rates"
)

# =============================================================================
# SENSITIVITY ANALYSIS RANGES
# =============================================================================
SENSITIVITY_RANGES <- list(
  r          = list(min = 0.015, max = 0.050),
  g          = list(min = 0.010, max = 0.025),  # 1-2.5%: Stern-conservative to above Groom & Venmans
  k0         = list(min = 0.005, max = 0.025),  # 0.5-2.5%: clamp on r-g (Ramsey-consistent)
  eps_mult   = list(min = 0.70,  max = 1.30),
  lambda_mult = list(min = 0.50, max = 1.50),
  U_mult     = list(min = 0.70,  max = 1.30),
  kappa      = list(min = 0.33,  max = 1.00)   # carbon/harvest leakage ratio; 0.33 lower bound, 1.0 = full harvest equivalence
)

KAPPA_SWEEP <- list(
  levels = c(0.33, 0.50, 0.60, 0.75, 1.00),
  labels = c("Low (0.33)", "Moderate (0.50)", "Central (0.60)",
             "High (0.75)", "Full (1.00)")
)
X_SWEEP <- list(
  multipliers = c(0.50, 0.75, 1.00, 1.25, 1.50),
  labels = c("Low (0.50x)", "Moderate (0.75x)", "Central (1.00x)",
             "High (1.25x)", "Very high (1.50x)")
)

# =============================================================================
# COLOUR PALETTE
# =============================================================================

BIOME_COLOURS <- c(
  Boreal = "#2E86AB",        # Blue
  Temperate = "#28A745",     # Green  
  Mediterranean = "#FFC107"  # Amber/Orange
)

# =============================================================================
# CRCF-DERIVED PORTFOLIO PARAMETERS
# =============================================================================

cat("  Deriving portfolio parameters...\n")

# Join CRCF areas with practice biomes. De-duplicate STYLISED_PROJECTS to one
# biome per practice first: 7 practices carry two is_anchor=TRUE rows (the
# BL/CF variants, all the same biome), so a raw merge by "practice" silently
# duplicates them and over-weights those practices in the portfolio aggregates
# (A2.2). Assert the join does not expand the CRCF rows.
.sp_pb <- STYLISED_PROJECTS[!duplicated(STYLISED_PROJECTS$practice),
                            c("practice", "biome")]
.crcf_biome <- merge(
  CRCF_EU_SCENARIOS[, c("practice", "rate", "eu_area_ha", "duration_yr")],
  .sp_pb,
  by = "practice", all.x = TRUE
)
stopifnot(nrow(.crcf_biome) == nrow(CRCF_EU_SCENARIOS))

# Practice weights by enrolled area
.practice_weights <- .crcf_biome$eu_area_ha / sum(.crcf_biome$eu_area_ha)

# Portfolio-weighted disturbance rate
.biome_lambdas <- sapply(.crcf_biome$biome, function(b) BIOME_PARAMS[[b]]$lambda_obs)
PORTFOLIO_LAMBDA <- sum(.practice_weights * .biome_lambdas)

# Portfolio-weighted correlation loading
.biome_c <- sapply(.crcf_biome$biome, function(b) BIOME_PARAMS[[b]]$c)
PORTFOLIO_C <- sum(.practice_weights * .biome_c)

# Biome weights from CRCF area distribution
.biome_areas <- tapply(.crcf_biome$eu_area_ha, .crcf_biome$biome, sum)
PORTFOLIO_BIOME_WEIGHTS <- .biome_areas / sum(.biome_areas)

# Climate trend coefficient from CRCF-derived biome weights
.biome_c_values <- sapply(names(BIOME_PARAMS), function(b) BIOME_PARAMS[[b]]$c)
CLIMATE_TREND_COEF <- sum(
  PORTFOLIO_BIOME_WEIGHTS * .biome_c_values[names(PORTFOLIO_BIOME_WEIGHTS)]
)

# Base disturbance rate = portfolio-weighted lambda
BASE_DISTURBANCE_RATE <- PORTFOLIO_LAMBDA

# CRCF-derived market scale
CRCF_ANNUAL_MT <- sum(CRCF_EU_SCENARIOS$eu_annual_MtCO2)
CRCF_MEAN_DURATION <- weighted.mean(
  .crcf_biome$duration_yr, .crcf_biome$eu_area_ha
)
MARKET_CAPACITY_MT <- CRCF_ANNUAL_MT * CRCF_MEAN_DURATION
PROJECT_TURNOVER <- 1 / CRCF_MEAN_DURATION

cat(sprintf("  Portfolio: lambda=%.4f, c=%.3f, %.0f MtCO2/yr, mean dur=%.0f yr\n",
            PORTFOLIO_LAMBDA, PORTFOLIO_C, CRCF_ANNUAL_MT, CRCF_MEAN_DURATION))

rm(.crcf_biome, .practice_weights, .biome_lambdas, .biome_c,
   .biome_areas, .biome_c_values)

# =============================================================================
# SCENARIO COMPARISON TABLE
# =============================================================================
# Compute portfolio summary for all 6 scenarios (area, annual Mt, biome mix)

cat("  Building scenario comparison table...\n")
SCENARIO_COMPARISON <- do.call(rbind, lapply(names(CRCF_SCENARIO_LIST), function(scn) {
  s <- CRCF_SCENARIO_LIST[[scn]]
  # Filter to non-zero area practices
  s_nz <- s[s$eu_area_ha > 0, ]
  if (nrow(s_nz) == 0) return(NULL)

  # Join with biome via anchor projects
  .sb <- merge(
    s_nz[, c("practice", "rate", "eu_area_ha", "duration_yr", "harvest_class")],
    .sp_pb,  # de-duplicated practice->biome (A2.2; defined above)
    by = "practice", all.x = TRUE
  )
  .w <- .sb$eu_area_ha / sum(.sb$eu_area_ha)
  .lam <- sapply(.sb$biome, function(b) BIOME_PARAMS[[b]]$lambda_obs)
  .c   <- sapply(.sb$biome, function(b) BIOME_PARAMS[[b]]$c)
  .biome_area <- tapply(.sb$eu_area_ha, .sb$biome, sum)
  .bw <- .biome_area / sum(.biome_area)

  tibble::tibble(
    scenario          = scn,
    total_area_Mha    = sum(s$eu_area_ha) / 1e6,
    annual_MtCO2      = sum(s$eu_annual_MtCO2),
    reducing_MtCO2    = sum(s$eu_annual_MtCO2[s$harvest_class == "Harvest-reducing"]),
    neutral_MtCO2     = sum(s$eu_annual_MtCO2[s$harvest_class == "Harvest-neutral"]),
    increasing_MtCO2  = sum(s$eu_annual_MtCO2[s$harvest_class == "Harvest-increasing"]),
    weighted_lambda    = sum(.w * .lam),
    weighted_c         = sum(.w * .c),
    mean_duration_yr   = weighted.mean(.sb$duration_yr, .sb$eu_area_ha),
    boreal_pct         = round(100 * ifelse("Boreal" %in% names(.bw), .bw[["Boreal"]], 0), 1),
    temperate_pct      = round(100 * ifelse("Temperate" %in% names(.bw), .bw[["Temperate"]], 0), 1),
    mediterranean_pct  = round(100 * ifelse("Mediterranean" %in% names(.bw), .bw[["Mediterranean"]], 0), 1),
    feasible           = !(scn == "increasing_only")
  )
}))

if (is.null(SCENARIO_COMPARISON) || nrow(SCENARIO_COMPARISON) == 0)
  stop("SCENARIO_COMPARISON is empty — all scenarios returned NULL")

write.csv(SCENARIO_COMPARISON,
          file.path(PATHS$output_tables, "scenario_comparison.csv"),
          row.names = FALSE)

cat(sprintf("  %d scenarios built (%.0f MtCO2/yr target, active=%s)\n",
            nrow(SCENARIO_COMPARISON), CRCF_ANNUAL_MT, ACTIVE_SCENARIO))

cat("OK: Parameters loaded\n")
# =============================================================================
# BIBLIOGRAPHY
# =============================================================================
# 
# Anderegg WRL et al. (2020) Climate-driven risks to the climate mitigation
#   potential of forests. Science 368:1341-1345.
#
# Badgley G et al. (2022) California's forest carbon offsets buffer pool is
#   severely undercapitalized. Frontiers in Forests and Global Change 5:930426.
#
# Brandl S et al. (2020) The influence of climate and management on survival
#   probability for Germany's most important tree species. Forest Ecology &
#   Management 458:117652.
#
# Chiti T et al. (2026) A review of forest management practices potentially
#   suitable for carbon farming in European forests. Journal of Environmental
#   Management 398:128391.
#
# Davis KT et al. (2024) Tamm review: A meta-analysis of thinning, prescribed
#   fire, and wildfire effects on subsequent wildfire severity in conifer
#   dominated forests of the Western US. Forest Ecology & Management
#   561:121885.
#
# Dobor L, Hlasny T, Zimova S (2020) Contrasting vulnerability of
#   monospecific and species-diverse forests to wind and bark beetle
#   disturbance: The role of management. Ecology and Evolution
#   10(21):12233-12245.
#
# Forest Europe (2020) State of Europe's Forests 2020.
#   https://foresteurope.org/state-europes-forests-2020/
#
# Forest Research (2024) Forestry Statistics 2024. Forest Research, Edinburgh.
#   https://www.forestresearch.gov.uk/tools-and-resources/statistics/
#
# Forzieri G et al. (2021) Emergent vulnerability to climate-driven disturbances
#   in European forests. Nature Communications 12:1216.
#
# Gardiner B et al. (2010) Destructive storms in European forests: past and
#   forthcoming impacts. European Forest Institute.
#
# Couture S, Garcia S, Reynaud A (2012) Household energy choices and fuelwood
#   consumption: An econometric approach using French data. Energy Economics
#   34(6):1972-1981.
#
# Groom B, Venmans F (2023) The social value of offsets. Nature 619:768-773.
#
# Hlasny T et al. (2021) Devastating outbreak of bark beetles in Central Europe:
#   drivers, impacts and outlook. Forest Ecology & Management 490:119075.
#
# Jactel H et al. (2012) Drought effects on damage by forest insects and
#   pathogens. Forest Ecology & Management 270:133-148.
#
# Kangas K, Baudin A (2003) Modelling and projections of forest products
#   demand, supply and trade in Europe. ECE/TIM/DP/30, FAO/UNECE Geneva.
#
# Kallio AMI, Solberg B (2018) Leakage of forest harvest changes in a small
#   open economy: case Norway. Scandinavian Journal of Forest Research 33(5):502-510.
#
# Kruger K et al. (2025) Setting aside areas for conservation does not
#   increase disturbances in temperate forests. Journal of Applied Ecology
#   62(2):329-343.
#
# Morland C et al. (2018) Supply and demand functions for global wood markets:
#   specification and plausibility testing of econometric models within the
#   global forest sector. Forest Policy & Economics 92:92-105.
#
# Murray BC et al. (2004) Estimating leakage from forest carbon sequestration
#   programs. Land Economics 80(1):109-124.
#
# Netherer S, Schopf A (2010) Potential effects of climate change on insect
#   herbivores. Journal of Pest Science 83:171-207.
#
# Borzykowski N (2019) A supply-demand modelling of the Swiss roundwood
#   market: actors responsiveness and CO2 implications. Forest Policy &
#   Economics 102:120-129.
#
# Patacca M et al. (2023) Significant increase in natural disturbance impacts
#   on European forests since 1950. Global Change Biology 29:1505-1520.
#
# Potterf M et al. (2025) Hotter drought increases population levels and
#   accelerates phenology of the European spruce bark beetle Ips typographus.
#   Forest Ecology & Management 577:122439.
#
# Pretzsch H et al. (2020) Growth and mortality of Norway spruce and European
#   beech in monospecific and mixed-species stands under natural episodic and
#   experimentally extended drought. Trees 34:957-970.
#
# Buongiorno J (2015) Income and time dependence of forest product demand
#   elasticities and implications for forecasting. Silva Fennica 49(5):1395.
#
# Rorstad PK, Solberg B et al. (2022) Can we detect regional differences in
#   econometric analyses of the Norwegian timber supply? Silva Fennica
#   56(1):10326.
#
# Quine CP, Gardiner BA (2007) Understanding how the interaction of wind and
#   trees results in windthrow. Forestry 80:61-81.
#
# Ruffault J et al. (2020) Increased fire activity under high atmospheric CO2
#   and warming in Iberian Peninsula. Environmental Research Letters 15:094006.
#
# Seidl R et al. (2014) Increasing forest disturbances in Europe and their
#   impact on carbon storage. Nature Climate Change 4:806-810.
#
# Seidl R et al. (2017) Forest disturbances under climate change.
#   Nature Climate Change 7:395-400.
#
# Senf C, Seidl R (2021) Mapping the forest disturbance regimes of Europe.
#   Nature Sustainability 4:63-70.
#
# Straus H, Boncina A (2025) The vulnerability of four main tree species in
#   European forests to seven natural disturbance agents: lessons from
#   Slovenia. European Journal of Forest Research 144:267-282.
#
# Thom D et al. (2017) The impacts of climate change and disturbance on
#   forest ecosystem service capacity. Journal of Ecology 105:983-994.
#
# Thurner M et al. (2014) Carbon stock and density of northern boreal and
#   temperate forests. Global Ecology & Biogeography 23:297-310.
#
# Tian X et al. (2017) A global analysis of timber harvest supply elasticity.
#   Forest Policy & Economics 77:9-15.
#
# Turco M et al. (2019) Exacerbated fires in Mediterranean Europe due to
#   anthropogenic warming. Nature Communications 10:3821.
#
# Valinger E, Fridman J (2011) Factors affecting the probability of windthrow
#   at stand level as a result of Gudrun winter storm in southern Sweden.
#   Forest Ecology & Management 262(3):398-403.
#
# =============================================================================