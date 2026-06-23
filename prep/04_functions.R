# =============================================================================
# 04_functions.R - Core Model Functions (Methods Section Equations)
# =============================================================================

cat("Loading core model functions...\n")

# =============================================================================
# Time-dependent harvest displacement for zero-baseline practices
# =============================================================================
# For practices where baseline harvest = 0 and baseline AGB ≈ 0 (productive
# afforestation, agroforestry on cropland), x = -H/Q: the ratio of harvest
# volume to net additional sequestration during the crediting period.
#
# f_h = fraction of gross growth harvested during T_project. Then:
#   Q = G(1 - f_h),  H = G·f_h  =>  x = -H/Q = -f_h / (1 - f_h)
#
# f_h is estimated from silvicultural parameters:
#   - Thinnings begin at T_thin, contribute f_thin of gross growth per thinning year
#   - Clearfell at T_rot removes the standing crop (≈ remaining fraction of growth)
#
# No upper bound on |x| — ℓ_max clamps the resulting leakage rate downstream.
# Short-rotation plantations (e.g., eucalyptus, poplar) can have f_h → 1 and
# x → -∞; this is correct (large negative leakage, capped at ℓ_max = 0.20).
#
# WCC (2025) management regimes: productive conifer (thinning + clearfell at yr 40)
# vs. minimum intervention (no thinning or clearfell). Protected afforestation has
# x=0 (static). This function applies only to productive afforestation.
#
# Swedish NFI: thinnings ≈ 24% of total harvest (Skogsstyrelsen 2024).
calc_x_afforestation <- function(T_project, T_thin = 15, T_rot = 40, f_thin = 0.24) {
  if (T_project <= 0) return(0)
  # Fraction of gross growth harvested during crediting period
  thin_end <- min(T_project, T_rot)
  thin_years <- max(thin_end - T_thin, 0)
  f_h <- f_thin * thin_years / T_project
  # Clearfell: only count harvests followed by regrowth within the crediting
  # period. A terminal clearfell at T_project removes stored carbon (a Q/
  # temporality matter), not a market supply addition with subsequent regrowth.
  if (T_project > T_rot) {
    n_clearfell <- floor((T_project - 1) / T_rot)
    f_h <- f_h + (1 - f_thin) * n_clearfell * T_rot / T_project
  }
  f_h <- min(f_h, 0.99)  # prevent division by zero; ℓ_max binds well before this
  # x = -H/Q = -f_h / (1 - f_h)
  -(f_h / (1 - f_h))
}

# Temporality deduction T (Methods Eq. 2)
# T = 1 - omega, where omega is the SVO weight from Groom & Venmans (2023).
# Higher T = larger deduction for impermanence. T ∈ [0,1].
#
# Afforestation and reforestation on legally protected land use have their
# storage horizon set to H_perm (GLOBAL_PARAMS$H_perm, default 100 yr) rather
# than the contract length tau_2. National forest laws (Germany BWaldG §11,
# France Code forestier, Sweden Skogsvardslagen, Finland Metsalaki, etc.)
# prohibit deforestation and mandate replant after clearfell, so the long-run
# mean stock persists beyond the contract — managed cycles average around the
# practice-specific S_bar, unmanaged stands keep accumulating to climax. The
# buffer is sized to the same H_perm to keep reversal pricing coherent (see
# 26_empirical_buffer.R, empirical_buffer()).
# Pure NPV time-weight deduction from the net discount rate k0. Vector-safe
# (no scalar-only branch), so the Monte Carlo path can reuse it directly on a
# k0 vector instead of re-inlining the formula. T = 1 - omega.
calc_T_k0 <- function(k0, phi_add, tau_1, tau_2, H_ref) {
  rate  <- k0 + phi_add
  num   <- (1 - exp(-rate * (tau_2 - tau_1))) / rate
  denom <- (1 - exp(-k0 * H_ref)) / k0
  1 - clip(num / denom, 0, 1)
}

# Scalar wrapper: derive k0 = r - g (floored near zero) and delegate. Callers
# that pass (r, g) are unchanged.
calc_T <- function(r, g, phi_add, tau_1, tau_2, H_ref) {
  k0 <- r - g
  if (k0 <= 0) k0 <- 0.001  # clamp near-zero k0 (expected at MC tails where r ≈ g)
  calc_T_k0(k0, phi_add, tau_1, tau_2, H_ref)
}

# Replacement share (Methods Eq. 3)
# Armington replacement rate (Murray 2004). s dampens import response only;
# domestic supply responds at full elasticity.
calc_rho_rep <- function(u, s, eps_d, eps_s_dom, eps_s_imp) {
  num <- u * eps_s_dom + (1 - u) * s * eps_s_imp
  denom <- abs(eps_d) + num
  if (denom == 0) return(0)
  num / denom
}

# Hazard phi (Methods Eq. 7)
# Note: (1+c) moved to safety loading in calc_buffer_rate. The hazard phi
# is the project-level precautionary hazard rate used for temporality and
# buffer sizing. gamma (1.10) is the project-level precautionary factor.
# Spatial correlation enters only through the buffer safety loading theta,
# not through the hazard rate itself.
calc_phi <- function(lambda_obs, U_H, c, gamma, R, S_bar, S_ref, alpha=1, beta=1.2) {
  lambda_H <- lambda_obs * (1 + U_H)
  gamma * lambda_H * (R ^ alpha) * ((S_bar / S_ref) ^ beta)
}

# =============================================================================
# BUFFER CALCULATION — Actuarial Expected-Value Premium Principle
# =============================================================================
# The model uses a SINGLE buffer formula:
#   b = (1 + theta') * lambda * R^a * S_ratio^b * E[Z] / tau
#   where theta' = theta_base * sqrt(H/H_ref) * (1+c)
# where theta = theta_base * sqrt(H / H_ref) is a VaR-motivated safety loading.
#
# Used by: calc_project_issuance (baseline), run_mc_project (MC sensitivity),
#          10_buffer_backtest.R (forward projections & solvency tests).
# =============================================================================

# Expected severity E[Z] for compound loss model
# Z ~ p * 1.0 + (1-p) * Beta(a, b)  (mixture: stand-replacing or partial)
calc_expected_severity <- function(p_sr,
                                   a = ACTUARIAL_PARAMS$partial_severity_a,
                                   b_shape = ACTUARIAL_PARAMS$partial_severity_b) {
  p_sr + (1 - p_sr) * a / (a + b_shape)
}

# Actuarial buffer rate (expected-value premium with SD-loading principle).
# Premium = (1 + theta) * E[L]; the SD-loading theta is built from three
# multiplicative factors:
#   theta = theta_base * sqrt(H / H_ref) * sqrt(c + (1 - c) / N_pool)
#
#   - sqrt(H/H_ref): time scaling under approximately IID losses (Bühlmann
#     1970, Klugman/Panjer/Willmot 2012 §5.3). With AR(1) autocorrelation
#     rho the exact scaling is sqrt(H * (1+rho)/(1-rho)/H_ref); the IID
#     form is a first-order approximation absorbed into theta_base by the
#     MC bisection in 25_finite_N_mc.R.
#
#   - sqrt(c + (1-c)/N_pool): pool-asymptotic SD factor for a buffer pool
#     of N_pool exposures with pairwise correlation c. Limits:
#         N_pool = 1    -> 1               (per-project / JRC framing)
#         N_pool -> Inf -> sqrt(c)         (diversification floor)
#
# Default N_pool = Inf gives the diversification floor appropriate for a
# shared scheme-level buffer (e.g. CRCF, Verra AFOLU, ART). Pass N_pool = 1
# for per-project framing.
#
# theta_base is calibrated against a solvency target by 25_finite_N_mc.R
# (cross-project sampling, AR(1) noise, vintage rolling stock).
calc_buffer_rate <- function(lambda, R = 1.0, S_ratio = 1.0,
                             c = NULL, alpha = 1.0, beta = 1.20,
                             H = NULL, tau = NULL,
                             theta_base = NULL, H_ref = NULL,
                             E_Z = NULL, N_pool = Inf) {
  if (is.null(c)) {
    if (!exists("CLIMATE_TREND_COEF")) stop("CLIMATE_TREND_COEF not defined (03_parameters.R not loaded?)")
    c <- CLIMATE_TREND_COEF
  }
  if (is.null(H)) {
    if (!exists("LIABILITY_HORIZON")) stop("LIABILITY_HORIZON not defined (03_parameters.R not loaded?)")
    H <- LIABILITY_HORIZON
  }
  if (is.null(tau)) {
    if (!exists("PROJECT_TURNOVER")) stop("PROJECT_TURNOVER not defined (03_parameters.R not loaded?)")
    tau <- PROJECT_TURNOVER
  }
  if (is.null(theta_base)) theta_base <- ACTUARIAL_PARAMS$theta_base
  if (is.null(H_ref)) H_ref <- ACTUARIAL_PARAMS$H_ref
  if (is.null(E_Z)) E_Z <- 1.0

  lambda_adj <- lambda * (R^alpha) * (S_ratio^beta) * E_Z
  pool_factor <- sqrt(pmax(c, 0) + pmax(1 - c, 0) / N_pool)
  theta <- theta_base * sqrt(H / H_ref) * pool_factor
  b <- (1 + theta) * lambda_adj / tau
  pmin(pmax(b, 0), 1)
}

# Country/zone-level buffer rate: convenience wrapper around calc_buffer_rate
# that pulls λ, severity, c from params_for(zone_or_country) and applies
# PRACTICE_DEFS multipliers. Returns a single buffer rate.
calc_buffer_rate_zone <- function(zone_or_country, practice,
                                  H = NULL, tau = NULL, R = NULL) {
  bp <- params_for(zone_or_country)
  pd <- PRACTICE_DEFS[PRACTICE_DEFS$practice_name == practice, ]
  if (nrow(pd) == 0) stop("Unknown practice: ", practice)
  if (is.null(H))   H   <- pd$tau_2
  if (is.null(tau)) tau <- 1 / pd$tau_2
  if (is.null(R))   R   <- pd$R_mult
  calc_buffer_rate(
    lambda  = bp$lambda_obs * pd$lambda_mult,
    R       = R,
    c       = bp$c * pd$c_mult,
    H       = H,
    tau     = tau,
    E_Z     = calc_expected_severity(bp$severity)
  )
}

# Draw n severity samples from compound loss mixture (used by 10_buffer_backtest.R)
draw_severity <- function(n, p_sr,
                          a = ACTUARIAL_PARAMS$partial_severity_a,
                          b_shape = ACTUARIAL_PARAMS$partial_severity_b) {
  is_sr <- rbinom(n, 1, p_sr)
  partial <- rbeta(n, a, b_shape)
  ifelse(is_sr == 1, 1.0, partial)
}

# AR(1) autocorrelated multiplicative noise (used by 10_buffer_backtest.R)
ar1_noise <- function(n, sigma = ACTUARIAL_PARAMS$sigma_AR1, rho = ACTUARIAL_PARAMS$rho_ar1) {
  eps <- numeric(n)
  innovation_sd <- sigma * sqrt(1 - rho^2)
  eps[1] <- rnorm(1, 0, sigma)
  for (t in 2:n) {
    eps[t] <- rho * eps[t - 1] + rnorm(1, 0, innovation_sd)
  }
  pmax(1 + eps, 0.5)
}

# Vintage rolling stock for buffer-pool cash-flow simulation.
# Live exposed stock at year t = sum of issuance vintages still on the books,
# i.e. issued in the last `tau_life` years, net of the buffer set-aside.
# At steady state (constant Q) credited(t) -> Q * tau_life * (1 - mean(b)),
# matching the analytic premium b = lambda * E[Z] / tau_life.
vintage_rolling_stock <- function(issuance_net, tau_life) {
  n <- length(issuance_net)
  tau_life <- max(1, round(tau_life))
  csum <- cumsum(issuance_net)
  if (tau_life >= n) return(csum)
  csum_lag <- c(rep(0, tau_life), csum[seq_len(n - tau_life)])
  csum - csum_lag
}

# =============================================================================
# PROJECT- AND REGION-SPECIFIC BUFFER LOOKUP
# =============================================================================
# Each project's buffer rate matches its actual disturbance risk: the rate
# is set from forest disturbance records for that project's country and
# forest type (broadleaf or conifer) over 1986-2023, focusing on the worst
# 1% of years to capture extreme events (storms, droughts, fires, beetle
# outbreaks).
#
# Operational national schemes set buffer rates within their own
# country. Several apply a single flat rate scheme-wide: WCC and the
# Peatland Code at 20% (UK), Klimaskovfonden at 15% (Denmark),
# Wald-Klima-Standard at 20% (Germany). The Label Bas-Carbone (France)
# differentiates within its territory: a 10% base plus a 0-15%
# fire-zone supplement and a 5-10% windfall component, averaging
# around 22% scheme-wide. Both designs are defensible at national
# scope, where disturbance hazard is relatively homogeneous and any
# within-country variation can be captured with a small number of
# administrative zone modifiers. It becomes a problem at EU scope, where the disturbance
# rate of a Finnish broadleaf restoration is roughly an order of
# magnitude below that of a Maritime-pine plantation in southern
# Iberia, and a single rate cross-subsidises high-risk enrolments at
# the expense of low-risk ones. The proposed EU CRCF Regulation
# operates at exactly this scope. The Joint Research Centre has
# independently concluded that EU-wide risk segmentation is required
# and produced a scientific proposal that segments the buffer at
# 35-km hexagon resolution via a biophysical forest-growth Monte
# Carlo (Marinelli et al. 2026); their proposal currently covers
# afforestation only. The approach implemented here extends
# segmentation to the full set of carbon-farming practices
# potentially suited for the CRCF (afforestation, peatland rewetting,
# extended rotation, reduced harvest, agroforestry, structural and
# species diversification, fuel management) by anchoring each
# project's rate
# directly to the EFDA disturbance record at country × forest-type
# resolution, with semi-parametric GPD tail extrapolation
# (Embrechts/Klüppelberg/Mikosch 1997 §6) and a coherent tail-risk
# measure (TVaR99, Artzner et al. 1999).
#
# The lookup table is built by 26_empirical_buffer.R from the European
# Forest Disturbance Atlas (EFDA, Viana-Soto & Senf 2025).

# Coniferous and broadleaf species used in ALL_PROJECTS. Anything else
# (peatland, paludiculture) defaults to broadleaf, which is the more
# conservative regime in the empirical bootstrap for non-tree practices.
CONIFER_SPECIES <- c(
  "Maritime pine", "Mixed conifers", "Norway spruce",
  "Scots pine", "Sitka spruce",
  "Native pinewood"                  # Caledonian-style protected pine
)
BROADLEAF_SPECIES <- c(
  "Chestnut/oak", "Climate-adapted mix", "Cork oak", "Eucalyptus",
  "Holm oak", "Mixed broadleaves", "Mixed species", "Native broadleaves",
  "Old growth",
  "Beech/oak", "Productive oak/beech" # BL variants for ER and Productive affor
)

# Abbreviated label combining forest type and biome for figure axes.
# Format: "Practice [BL · Tem]". Forest-type code is "—" for non-tree
# practices (peatland, paludiculture); biome code is one of Bor / Tem /
# Tem-UK / Med. Vectorised.
BIOME_ABBREV <- c(Boreal = "Bor", Temperate = "Tem",
                  Temperate_UK = "Tem-UK", Mediterranean = "Med")
FOREST_TYPE_ABBREV <- c(broadleaf = "BL", conifer = "CF")

# Secondary BL/CF variants — excluded from the headline rankings figure to
# keep it compact. These are the BL or CF variants added on top of the
# original anchor for short-horizon (~30-yr contract) management practices,
# where the broadleaf/conifer difference in NCV is small (a few percentage
# points). The split is preserved in supplementary Table S7 and the
# cross-biome ED figure. Legally-protected afforestation practices
# (Productive/Protected afforestation, H_perm = 100 yr) retain both BL and CF
# variants in the headline because their buffer differences are material.
SECONDARY_BLCF_VARIANTS <- list(
  c("Extended rotation",          "Beech/oak"),
  c("Reduced harvest intensity",  "Mixed broadleaves"),
  c("Set-aside",                  "Norway spruce"),
  c("Continuous stock management", "Mixed conifers"),
  c("Reforestation",              "Mixed conifers")
)

is_secondary_variant <- function(practice, species) {
  keys <- paste(practice, species, sep = "|")
  sec_keys <- vapply(SECONDARY_BLCF_VARIANTS,
                     function(x) paste(x[1], x[2], sep = "|"), character(1))
  keys %in% sec_keys
}

practice_full_label <- function(practice, species, biome) {
  ft <- forest_type_from_species(species)
  ft_code <- unname(FOREST_TYPE_ABBREV[ft])
  ft_code[is.na(ft_code)] <- "—"   # em-dash for non-BL/CF (peatland, etc.)
  # Non-tree species default to em-dash regardless of forest_type_from_species
  ft_code[species %in% c("Paludiculture", "Drained peatland",
                          "Drained peatland forest")] <- "—"
  biome_code <- unname(BIOME_ABBREV[biome])
  biome_code[is.na(biome_code)] <- biome[is.na(biome_code)]
  paste0(practice, " [", ft_code, " · ", biome_code, "]")
}

forest_type_from_species <- function(species) {
  if (length(species) > 1) {
    return(vapply(species, forest_type_from_species, character(1),
                  USE.NAMES = FALSE))
  }
  if (is.na(species))                   return("broadleaf")
  # Idempotent: a forest-type string ("broadleaf"/"conifer") passed in (e.g. from
  # empirical_buffer/empirical_buffer_se) is already a forest type, not a species
  # -- return it unchanged, no warning.
  if (species %in% c("broadleaf", "conifer")) return(species)
  if (species %in% CONIFER_SPECIES)     return("conifer")
  if (species %in% BROADLEAF_SPECIES)   return("broadleaf")
  # Intentional broadleaf default for non-tree practices (peatland/paludiculture).
  # Warn for anything ELSE unrecognised so a typo or a newly added species can't
  # silently inherit the broadleaf buffer regime.
  if (!species %in% c("Paludiculture", "Drained peatland", "Drained peatland forest"))
    warning("forest_type_from_species: unrecognised species '", species,
            "' - defaulting to broadleaf. Add it to CONIFER_SPECIES / ",
            "BROADLEAF_SPECIES (04_functions.R) if it is a tree.")
  "broadleaf"
}

# Lookup: (zone, forest_type) -> empirical TVaR_99 buffer rate.
# `zone` accepts a country name (e.g. "Finland") or a biome name
# (e.g. "Temperate"). Country resolution is preferred when available;
# biome resolution returns the forest-area-weighted mean across
# countries in that biome. `mult` is a practice-level multiplier
# (e.g. 0.5 for agroforestry, where standing biomass is partial).
# `legally_protected = TRUE` returns the rate sized to the permanence
# horizon (H_perm column), used by afforestation where the land-use
# classification persists beyond the contract.
# `establishment_risk = TRUE` adds the biome-specific establishment-failure
# floor (afforestation AND reforestation; see EST_FLOOR_AFFOREST_BY_BIOME).
empirical_buffer <- function(zone, forest_type, mult = 1.0,
                             legally_protected = FALSE,
                             establishment_risk = FALSE) {
  if (!exists("EMPIRICAL_BUFFER_LOOKUP", envir = .GlobalEnv)) {
    stop("EMPIRICAL_BUFFER_LOOKUP not loaded. ",
         "Run 26_empirical_buffer.R first.")
  }
  L <- get("EMPIRICAL_BUFFER_LOOKUP", envir = .GlobalEnv)
  ftv <- forest_type_from_species(forest_type)
  if (forest_type %in% c("broadleaf", "conifer")) ftv <- forest_type
  b_col <- if (legally_protected) "b_perm" else "b"
  # Newly-established stands (afforestation AND reforestation; establishment_risk
  # = TRUE) carry an establishment-risk floor: young-stand mortality (frost,
  # drought, browsing, planting failure) in the first ~5-10 yr is independent of
  # the mature-forest disturbance rate EFDA observes, so it is missing from the
  # bootstrap. Added as an independent compound term: b -> 1 - (1 - b)(1 - floor).
  # The floor is BIOME-SPECIFIC (EST_FLOOR_AFFOREST_BY_BIOME, 03_parameters.R),
  # re-derived from the establishment-failure literature (NOT tuned to the JRC
  # benchmark): drought-driven Mediterranean failure >> boreal. NB gated on
  # establishment_risk, NOT legally_protected (which only sets the horizon) --
  # reforestation is not legally protected but IS newly established. Fail loud if
  # the biome cannot be resolved.
  est_floor <- 0
  if (establishment_risk) {
    if (!exists("EST_FLOOR_AFFOREST_BY_BIOME"))
      stop("EST_FLOOR_AFFOREST_BY_BIOME not defined (03_parameters.R not loaded?)")
    .biome_z <- if (!is.null(zone) && zone %in% names(EST_FLOOR_AFFOREST_BY_BIOME)) {
      zone
    } else {
      tryCatch(params_for(zone)$biome, error = function(e) NA_character_)
    }
    if (is.na(.biome_z) || !(.biome_z %in% names(EST_FLOOR_AFFOREST_BY_BIOME)))
      stop("empirical_buffer: cannot resolve biome for establishment floor, zone='",
           zone, "'")
    est_floor <- EST_FLOOR_AFFOREST_BY_BIOME[[.biome_z]]
  }
  apply_floor <- function(b) if (est_floor > 0) 1 - (1 - b) * (1 - est_floor) else b
  hit_country <- L$country[L$country$zone == zone &
                            L$country$forest_type == ftv, ]
  if (nrow(hit_country) == 1) {
    b <- hit_country[[b_col]]
    if (is.na(b)) {
      warning("empirical_buffer: '", b_col, "' NA for zone='", zone,
              "', forest_type='", ftv, "' - falling back to 'b'")
      b <- hit_country$b
    }
    if (is.na(b)) stop("empirical_buffer: NA rate for zone='", zone,
                       "', forest_type='", ftv, "'")
    return(min(apply_floor(b) * mult, 1))
  }
  hit_biome <- L$biome[L$biome$zone == zone &
                        L$biome$forest_type == ftv, ]
  if (nrow(hit_biome) == 1) {
    b <- hit_biome[[b_col]]
    if (is.na(b)) {
      warning("empirical_buffer: '", b_col, "' NA for biome zone='", zone,
              "', forest_type='", ftv, "' - falling back to 'b'")
      b <- hit_biome$b
    }
    if (is.na(b)) stop("empirical_buffer: NA rate for biome zone='", zone,
                       "', forest_type='", ftv, "'")
    return(min(apply_floor(b) * mult, 1))
  }
  stop("No empirical buffer rate for zone='", zone,
       "', forest_type='", ftv, "'")
}

# Bootstrap standard error of the empirical buffer rate for (zone, forest_type),
# at the contract horizon (b_se) or permanence horizon (b_perm_se when
# legally_protected). This is the batch-means SE of the TVaR99 estimate emitted
# by 26_empirical_buffer.R. run_mc_project uses it to perturb the buffer by its
# REAL sampling uncertainty instead of a flat ±15%. Returns 0 if the SE columns
# are absent (older lookup) — the MC then degrades to a fixed buffer.
empirical_buffer_se <- function(zone, forest_type, legally_protected = FALSE) {
  if (!exists("EMPIRICAL_BUFFER_LOOKUP", envir = .GlobalEnv)) return(0)
  L <- get("EMPIRICAL_BUFFER_LOOKUP", envir = .GlobalEnv)
  ftv <- forest_type_from_species(forest_type)
  if (forest_type %in% c("broadleaf", "conifer")) ftv <- forest_type
  se_col <- if (legally_protected) "b_perm_se" else "b_se"
  hit_c <- L$country[L$country$zone == zone & L$country$forest_type == ftv, ]
  if (nrow(hit_c) == 1 && se_col %in% names(hit_c) && !is.na(hit_c[[se_col]]))
    return(hit_c[[se_col]])
  hit_b <- L$biome[L$biome$zone == zone & L$biome$forest_type == ftv, ]
  if (nrow(hit_b) == 1 && se_col %in% names(hit_b) && !is.na(hit_b[[se_col]]))
    return(hit_b[[se_col]])
  0
}

# EWMA smoother (used by 10_buffer_backtest.R and 17_figures_manuscript.R)
ewma_smooth <- function(x, alpha = 0.20) {
  out <- numeric(length(x))
  out[1] <- x[1]
  for (j in 2:length(x)) {
    out[j] <- alpha * x[j] + (1 - alpha) * out[j - 1]
  }
  out
}

# Deductions (Methods Eqs. 9-11) — uses T (temporality deduction)
calc_deductions <- function(T_temp, L, b) {
  list(delta_leak = L, delta_temp = (1 - L) * T_temp,
       delta_buf = (1 - L) * (1 - T_temp) * b,
       delta_total = L + (1 - L) * T_temp + (1 - L) * (1 - T_temp) * b)
}

# Issuance (Methods Eqs. 12-14) — uses T (temporality deduction)
# I_net = Q × (1-L) × (1-T) × (1-b)
calc_issuance <- function(Q, T_temp, L, b) {
  I_elig <- (1 - L) * (1 - T_temp) * Q
  I_net <- (1 - b) * I_elig
  list(I_net = I_net, B = b * I_elig, I_elig = I_elig, net_share = I_net / Q)
}

# =============================================================================
# LEAKAGE PARAMETER RESOLUTION (shared helper)
# =============================================================================
# Resolves leak_params for a given biome and product class.
# Tries: (1) biome-specific product class, (2) generic elasticities_by_product,
# (3) DEFAULT_LEAK_PARAMS from 03_parameters.R.
# Used by calc_project_issuance(), run_mc_project(), and 09_scheme_comparison.R.

resolve_leak_params <- function(biome_key, product_class) {
  # (1) Biome-specific product class
  biome_leak <- LEAKAGE_PARAMS[[biome_key]]
  if (is.list(biome_leak) && product_class %in% names(biome_leak)) {
    pc <- biome_leak[[product_class]]
    if (is.list(pc) && all(c("u", "s", "eps_d", "eps_s_dom", "eps_s_imp") %in% names(pc)))
      return(pc)
  }
  # (2) Generic product class
  pclass <- LEAKAGE_PARAMS$elasticities_by_product[[product_class]]
  trade <- LEAKAGE_PARAMS$trade_shares[[biome_key]]
  if (is.null(trade)) trade <- DEFAULT_TRADE_SHARES
  if (is.list(pclass) && "gamma" %in% names(pclass)) {
    return(list(u = trade$alpha_dom, s = pclass$gamma,
                eps_d = pclass$eps_d, eps_s_dom = pclass$eps_s_dom,
                eps_s_imp = pclass$eps_s_imp))
  }
  # (3) Last-resort fallback (Temperate SL generic). Should not fire for any
  # mapped practice/biome; warn loudly if it does so it is never silent.
  warning("resolve_leak_params: no biome/generic match for biome='", biome_key,
          "', product_class='", product_class, "' - using DEFAULT_LEAK_PARAMS")
  DEFAULT_LEAK_PARAMS
}

# =============================================================================
# WEIGHTED ρ_rep ACROSS PRODUCT CLASSES (Methods Eq. 2a)
# =============================================================================
# Each practice affects multiple product classes (SL, PW, WF) with different
# weights. ρ_rep is computed per class and averaged by the practice's exposure.
# When practice_product_weights are undefined or sum to zero (e.g., peatland
# rewetting), falls back to the practice's primary product_class.

calc_weighted_rho_rep <- function(practice_name, biome_key, product_class) {
  ppw <- LEAKAGE_PARAMS$practice_product_weights[[practice_name]]
  if (!is.null(ppw) && sum(unlist(ppw)) > 0) {
    rho <- 0
    for (pc in names(ppw)) {
      w <- ppw[[pc]]
      if (w > 0) {
        lp <- resolve_leak_params(biome_key, pc)
        rho <- rho + w * calc_rho_rep(lp$u, lp$s, lp$eps_d,
                                       lp$eps_s_dom, lp$eps_s_imp)
      }
    }
    return(rho)
  }
  # Fallback: single product class
  lp <- resolve_leak_params(biome_key, product_class)
  calc_rho_rep(lp$u, lp$s, lp$eps_d, lp$eps_s_dom, lp$eps_s_imp)
}

# Per-product-class components of rho_rep, for VECTORIZED elasticity-ratio
# perturbation in the Monte Carlo. Returns a matrix (one row per active product
# class) with columns weight, num (= u*eps_s_dom + (1-u)*s*eps_s_imp), abs_eps_d.
# Then rho_rep(m) = sum_pc weight * (m*num) / (|eps_d| + m*num), where m is the
# supply/demand elasticity-RATIO multiplier. m = 1 reproduces
# calc_weighted_rho_rep exactly (a COMMON elasticity multiplier cancels in the
# Armington ratio; only the ratio moves rho — this is why the MC must perturb
# the ratio, not a common multiplier, for leakage uncertainty to propagate).
rho_components <- function(practice_name, biome_key, product_class) {
  ppw <- LEAKAGE_PARAMS$practice_product_weights[[practice_name]]
  .row <- function(w, lp) {
    num <- lp$u * lp$eps_s_dom + (1 - lp$u) * lp$s * lp$eps_s_imp
    c(weight = w, num = num, abs_eps_d = abs(lp$eps_d))
  }
  rows <- list()
  if (!is.null(ppw) && sum(unlist(ppw)) > 0) {
    for (pc in names(ppw)) {
      w <- ppw[[pc]]
      if (w > 0) rows[[length(rows) + 1]] <- .row(w, resolve_leak_params(biome_key, pc))
    }
  } else {
    rows[[1]] <- .row(1, resolve_leak_params(biome_key, product_class))
  }
  do.call(rbind, rows)
}

# =============================================================================
# FULL PROJECT CALCULATION - uses PRACTICE_DEFS for all lookups
# =============================================================================

calc_project_issuance <- function(project, global_params = GLOBAL_PARAMS,
                                  biome_params = NULL, leak_params = NULL,
                                  practice_defs = PRACTICE_DEFS) {
  if (is.null(biome_params)) biome_params <- BIOME_PARAMS[[project$biome]]

  practice <- project$practice

  # =========================================================================
  # GET PRACTICE PARAMETERS FROM PRACTICE_DEFS (single source of truth)
  # =========================================================================
  practice_def <- get_practice_def(practice, practice_defs)
  if (is.null(practice_def)) stop("Unknown practice: ", practice)

  phi_add <- practice_def$phi_add
  R_mult <- practice_def$R_mult
  lambda_mult <- practice_def$lambda_mult
  c_mult <- practice_def$c_mult
  x_harvest <- practice_def$harvest_displacement
  product_class <- practice_def$product_class

  # Handle NA product class (no market impact practices)
  if (is.na(product_class)) product_class <- "SL"

  # Dynamic x for zero-baseline practices (x = -H/Q, depends on silvicultural regime)
  # Triggered when project has non-NA silvicultural params (T_rot_silv, etc.)
  if (!is.null(project$T_rot_silv) && !is.na(project$T_rot_silv)) {
    if (is.null(practice_def$tau_2)) stop("practice_def$tau_2 missing for ", practice)
    tau_2_eff <- practice_def$tau_2
    if (!is.null(project$tau_2_override)) tau_2_eff <- max(tau_2_eff, project$tau_2_override)
    x_harvest <- calc_x_afforestation(tau_2_eff,
                                       T_thin = project$T_thin_silv,
                                       T_rot  = project$T_rot_silv,
                                       f_thin = project$f_thin_silv)
  }

  # =========================================================================
  # LEAKAGE CALCULATION - Product-Class Aware (Methods Eqs. 3-4)
  # =========================================================================
  # 
  # Uses product-class specific elasticities from:
  # - Forest Research UK (2024): demand elasticities
  # - Kallio & Solberg (2018): supply elasticities, sawlog/pulpwood 60-100% leakage
  # - Tian et al. (2017): meta-analysis of supply elasticities
  #
  # Product classes:
  # - Sawlogs (SL): lengthened rotation, no harvesting, afforestation
  # - Pulpwood (PW): reduced thinning (thinnings → pulp)
  # - Woodfuel (WF): agroforestry
  
  # =========================================================================
  # MARKET LEAKAGE — weighted ρ_rep across product classes (Methods Eq. 2a)
  # =========================================================================
  if (is.null(leak_params)) {
    rho_rep <- calc_weighted_rho_rep(practice, project$biome, product_class)
  } else {
    # Caller-provided override (e.g., scheme comparison with regional biome)
    rho_rep <- calc_rho_rep(leak_params$u, leak_params$s, leak_params$eps_d,
                            leak_params$eps_s_dom, leak_params$eps_s_imp)
  }

  L_market <- global_params$kappa * rho_rep * x_harvest

  # Total leakage clipped per Methods Eq. 4
  L <- clip(L_market, -global_params$ell_max, 1)
  
  # Cap for non-harvest-reducing practices (Murray 2004: afforestation 7-17%)
  if (x_harvest <= 0) {
    L <- min(L, global_params$ell_max)
  }
  
  # =========================================================================
  # REVERSAL RISK AND BUFFER CALCULATION
  # =========================================================================
  # Species-specific risk factor (from BIOME_PARAMS)
  R_species_key <- paste0("R_", gsub(" ", "_", project$species))
  R_base <- biome_params[[R_species_key]]
  if (is.null(R_base)) R_base <- biome_params$R_mixed
  if (is.null(R_base)) R_base <- 1.0
  
  R <- R_base * R_mult
  lambda_adj <- biome_params$lambda_obs * lambda_mult
  c_adj <- biome_params$c * c_mult
  S_bar <- project$S_bar
  
  # =========================================================================
  # DEDUCTIONS AND ISSUANCE (Methods Eqs. 2, 7-14)
  # =========================================================================
  # Use practice-specific τ₂ (realistic VCM contract duration).
  # For scheme comparison: use max(practice τ₂, scheme liability) as effective
  # storage duration — longer liability means less temporality deduction.
  # tau_2_override is set by calc_proposed_regional() in 09_scheme_comparison.R.
  if (is.null(practice_def$tau_2)) stop("practice_def$tau_2 missing")
  tau_2_practice <- practice_def$tau_2
  if (!is.null(project$tau_2_override)) {
    tau_2_practice <- max(tau_2_practice, project$tau_2_override)
  }

  # Legally protected land use (afforestation / reforestation): the storage
  # horizon is the legal-permanence reference H_perm, not the contract length.
  # National forest laws mandate replant after clearfell, so the long-run mean
  # stock persists beyond tau_2.
  legally_protected <- isTRUE(practice_def$legally_protected_landuse)
  tau_2_temp <- if (legally_protected) global_params$H_perm else tau_2_practice

  T_temp <- calc_T(global_params$r, global_params$g, phi_add,
                    global_params$tau_1, tau_2_temp, global_params$H_ref)

  # phi: PARAMETRIC reference hazard (exponential form b = 1 - exp(-phi*H)),
  # reported for the parametric sensitivity (scripts 15, 19) and the
  # parametric-vs-empirical comparison. It is NOT the headline buffer — the
  # headline b below comes from empirical_buffer() (EFDA TVaR99).
  phi <- calc_phi(lambda_adj, biome_params$U_50, c_adj, global_params$gamma,
                  R, S_bar, biome_params$S_ref, global_params$alpha, global_params$beta)

  # Buffer rate: project- and region-specific.
  # Each project pays the rate that matches its country (or biome) and
  # forest type — high-risk regions and vulnerable species pay more, low-
  # risk projects pay less. The differentiation is anchored to empirical
  # disturbance records, not expert risk scores or zone overlays.
  # Legally protected practices are priced against the H_perm horizon (symmetric
  # with calc_T); other practices against their contract horizon.
  ft_proj <- forest_type_from_species(project$species)
  buffer_mult <- if ("buffer_mult" %in% names(practice_def)) practice_def$buffer_mult else 1.0
  zone_proj <- if (!is.null(project$country) && !is.na(project$country)) {
    project$country
  } else {
    project$biome
  }
  establishment_risk <- isTRUE(practice_def$establishment_risk)
  b <- empirical_buffer(zone_proj, ft_proj, mult = buffer_mult,
                         legally_protected = legally_protected,
                         establishment_risk = establishment_risk)

  issuance <- calc_issuance(project$Q, T_temp, L, b)
  deductions <- calc_deductions(T_temp, L, b)

  tibble(practice = practice, biome = project$biome, species = project$species,
         Q = project$Q, x = x_harvest, T_temp = T_temp, L = L, b = b, phi = phi,
         I_net = issuance$I_net, B = issuance$B, I_elig = issuance$I_elig,
         net_share = issuance$net_share, delta_temp = deductions$delta_temp,
         delta_leak = deductions$delta_leak, delta_buf = deductions$delta_buf)
}

calc_batch_issuance <- function(projects, ...) {
  map_dfr(1:nrow(projects), function(i) {
    calc_project_issuance(as.list(projects[i, ]), ...)
  })
}

# MC helpers
draw_mc_params <- function(n, ranges = SENSITIVITY_RANGES, seed = 42) {
  set.seed(seed)
  # k0 range: clamp r-g to Ramsey-consistent bounds (independent draws overstate
  # uncertainty because r and g are structurally linked via Ramsey rule r = δ + η·g)
  k0_min <- if (!is.null(ranges$k0)) ranges$k0$min else 0.005
  k0_max <- if (!is.null(ranges$k0)) ranges$k0$max else 0.025
  tibble(iteration = 1:n,
         r = runif(n, ranges$r$min, ranges$r$max),
         g = runif(n, ranges$g$min, ranges$g$max),
         eps_mult = runif(n, ranges$eps_mult$min, ranges$eps_mult$max),
         lambda_mult = runif(n, ranges$lambda_mult$min, ranges$lambda_mult$max),
         U_mult = runif(n, ranges$U_mult$min, ranges$U_mult$max),
         kappa = runif(n, ranges$kappa$min, ranges$kappa$max)) %>%
    mutate(k0 = pmin(pmax(r - g, k0_min), k0_max))
}

run_mc_project <- function(project, n_iter = 10000, seed = 42) {
  mc_params <- draw_mc_params(n_iter, seed = seed)
  biome_params <- BIOME_PARAMS[[project$biome]]

  # ---- Static lookups (same for all iterations) ----
  practice_def <- get_practice_def(project$practice)
  product_class <- if (!is.null(practice_def)) practice_def$product_class else "SL"
  if (is.na(product_class)) product_class <- "SL"

  phi_add      <- practice_def$phi_add
  R_mult       <- practice_def$R_mult
  lambda_mult0 <- practice_def$lambda_mult
  c_mult       <- practice_def$c_mult
  x_harvest    <- practice_def$harvest_displacement
  ell_max      <- GLOBAL_PARAMS$ell_max

  if (is.null(practice_def$tau_2)) stop("practice_def$tau_2 missing")
  tau_2_practice <- practice_def$tau_2

  # Dynamic x for zero-baseline practices (uses project-level silvicultural params)
  if (!is.null(project$T_rot_silv) && !is.na(project$T_rot_silv)) {
    x_harvest <- calc_x_afforestation(tau_2_practice,
                                       T_thin = project$T_thin_silv,
                                       T_rot  = project$T_rot_silv,
                                       f_thin = project$f_thin_silv)
  }

  # Species risk factor (static per project)
  R_species_key <- paste0("R_", gsub(" ", "_", project$species))
  R_base <- biome_params[[R_species_key]]
  if (is.null(R_base)) R_base <- biome_params$R_mixed
  if (is.null(R_base)) R_base <- 1.0
  R_val <- R_base * R_mult

  # ρ_rep components per product class (weight, num, |eps_d|) for the vectorized
  # elasticity-ratio perturbation below. rho_rep_base (m=1) is the deterministic
  # value; the MC perturbs the supply/demand elasticity RATIO via eps_mult so
  # elasticity uncertainty propagates to leakage and net_share.
  rho_rep_base <- calc_weighted_rho_rep(project$practice, project$biome, product_class)
  rho_comp     <- rho_components(project$practice, project$biome, product_class)

  # Biome constants
  lambda_obs0 <- biome_params$lambda_obs
  U_50_0      <- biome_params$U_50
  c0          <- biome_params$c
  S_ref       <- biome_params$S_ref
  S_bar       <- project$S_bar
  Q           <- project$Q
  gamma       <- GLOBAL_PARAMS$gamma
  alpha       <- GLOBAL_PARAMS$alpha
  beta        <- GLOBAL_PARAMS$beta
  tau_1       <- GLOBAL_PARAMS$tau_1
  H_ref       <- GLOBAL_PARAMS$H_ref

  # ---- Vectorized MC computation over all n_iter at once ----
  r_vec     <- mc_params$r
  g_vec     <- mc_params$g
  k0_vec    <- mc_params$k0
  eps_m_vec <- mc_params$eps_mult
  lam_m_vec <- mc_params$lambda_mult
  U_m_vec   <- mc_params$U_mult
  kappa_vec <- mc_params$kappa

  # Legally protected land use → integrate to H_perm in calc_T (symmetric
  # with calc_project_issuance; see calc_T docstring).
  legally_protected <- isTRUE(practice_def$legally_protected_landuse)
  tau_2_temp <- if (legally_protected) GLOBAL_PARAMS$H_perm else tau_2_practice

  # Temporality deduction T — vectorized via the shared calc_T_k0 (k0_vec is
  # already Ramsey-clamped to [0.005, 0.025] in draw_mc_params, so it is > 0).
  T_vec     <- calc_T_k0(k0_vec, phi_add, tau_1, tau_2_temp, H_ref)
  omega_vec <- 1 - T_vec

  # Leakage — vectorized. ρ_rep varies per draw through the supply/demand
  # elasticity-ratio multiplier eps_mult: rho_pc = (m*num)/(|eps_d| + m*num),
  # summed over product classes by weight. BOTH elasticity uncertainty
  # (eps_mult) and the carbon/harvest ratio (kappa) now propagate to leakage and
  # net_share. eps_mult = 1 recovers the deterministic rho_rep_base.
  rho_rep_vec <- rowSums(vapply(seq_len(nrow(rho_comp)), function(k) {
    num_k <- rho_comp[k, "num"]; absd_k <- rho_comp[k, "abs_eps_d"]
    rho_comp[k, "weight"] * (eps_m_vec * num_k) / (absd_k + eps_m_vec * num_k)
  }, numeric(n_iter)))
  L_market_vec <- kappa_vec * rho_rep_vec * x_harvest
  L_vec <- clip(L_market_vec, -ell_max, 1)
  if (x_harvest <= 0) L_vec <- pmin(L_vec, ell_max)

  # Lambda and phi (kept for output diagnostics only)
  lambda_adj_vec <- lambda_obs0 * lambda_mult0 * lam_m_vec
  U_50_vec       <- U_50_0 * U_m_vec
  lambda_H_vec   <- lambda_adj_vec * (1 + U_50_vec)
  c_adj          <- c0 * c_mult
  phi_vec <- gamma * lambda_H_vec *
             (R_val ^ alpha) * ((S_bar / S_ref) ^ beta)

  # Buffer rate: empirical TVaR_99 lookup, project- and region-specific. The MC
  # perturbs it by its REAL per-cell bootstrap standard error (batch-means SE of
  # the TVaR99 estimate from 26_empirical_buffer.R), not a flat ±15%.
  ft_proj <- forest_type_from_species(project$species)
  buffer_mult <- if ("buffer_mult" %in% names(practice_def)) practice_def$buffer_mult else 1.0
  zone_proj <- if (!is.null(project$country) && !is.na(project$country)) {
    project$country
  } else {
    project$biome
  }
  establishment_risk <- isTRUE(practice_def$establishment_risk)
  b_central <- empirical_buffer(zone_proj, ft_proj, mult = buffer_mult,
                                 legally_protected = legally_protected,
                                 establishment_risk = establishment_risk)
  b_se  <- empirical_buffer_se(zone_proj, ft_proj, legally_protected = legally_protected)
  b_vec <- pmin(pmax(rnorm(n_iter, b_central, b_se), 0), 1)

  # Issuance & deductions — vectorized via the shared helpers (same formulas as
  # the deterministic path; calc_issuance/calc_deductions are pure arithmetic).
  .iss <- calc_issuance(Q, T_vec, L_vec, b_vec)
  .ded <- calc_deductions(T_vec, L_vec, b_vec)
  I_elig_vec    <- .iss$I_elig
  I_net_vec     <- .iss$I_net
  net_share_vec <- .iss$net_share
  delta_leak_vec <- .ded$delta_leak
  delta_temp_vec <- .ded$delta_temp
  delta_buf_vec  <- .ded$delta_buf

  # ---- Build result tibble (one allocation, no row-by-row binding) ----
  tibble(
    practice   = project$practice,
    biome      = project$biome,
    species    = project$species,
    Q          = Q,
    x          = x_harvest,
    omega      = omega_vec,
    L          = L_vec,
    b          = b_vec,
    phi        = phi_vec,
    I_net      = I_net_vec,
    B          = b_vec * I_elig_vec,
    I_elig     = I_elig_vec,
    net_share  = net_share_vec,
    delta_temp = delta_temp_vec,
    delta_leak = delta_leak_vec,
    delta_buf  = delta_buf_vec,
    iteration  = seq_len(n_iter),
    r          = r_vec,
    g          = g_vec,
    k0         = k0_vec,
    eps_mult   = eps_m_vec,
    lambda_mult = lam_m_vec,
    U_mult     = U_m_vec,
    kappa      = kappa_vec
  )
}

# =============================================================================
# SHARED COLOUR PALETTE & THEME
# =============================================================================
# Single source of truth for all plotting scripts (15, 16, 17).
# Defined here so that any script sourced after 04_functions.R can use them
# without fallback guards.

NATURE_BLUE <- "#4A90D9"
NATURE_RED <- "#D55E00"
NATURE_GREY <- "#5A5A5A"
NATURE_GREY_LIGHT <- "#F5F5F5"

NATURE_PALETTE <- c(
  "#4A90D9", "#D55E00", "#009E73", "#E69F00",
  "#CC79A7", "#56B4E9", "#0072B2", "#F0E442"
)

BIOME_COLOURS <- c(
  Boreal = "#4A90D9",
  Temperate = "#009E73",
  Mediterranean = "#E69F00"
)

SCHEME_COLOURS <- c(
  Proposed = "#5A5A5A",
  LBC = "#E69F00",
  WCC = "#4A90D9",
  PLC = "#56B4E9",
  WKS = "#009E73",
  KSF = "#CC79A7",
  CA_USFP = "#D55E00"
)

DEDUCTION_COLOURS <- c(
  `Temporality`  = "#C8C8C8",
  `Leakage`      = "#8C8C8C",
  `Buffer`       = "#5C5C5C",
  `Net issuance` = "#4A90D9"
)

PRACTICE_TYPE_COLOURS <- c(
  "Harvest-reducing" = "#D55E00",
  "Harvest-neutral" = "#009E73",
  "Harvest-increasing" = "#4A90D9"
)

theme_nature <- function(base_size = 10, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title = element_text(face = "bold", size = rel(1.1), hjust = 0,
                                margin = margin(b = 8)),
      plot.subtitle = element_text(size = rel(0.9), colour = NATURE_GREY, hjust = 0,
                                   margin = margin(b = 12)),
      plot.caption = element_text(size = rel(0.75), colour = "#888888", hjust = 1,
                                  margin = margin(t = 10)),
      axis.title = element_text(size = rel(0.9), face = "plain", colour = NATURE_GREY),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text = element_text(size = rel(0.85), colour = NATURE_GREY),
      axis.line = element_line(colour = "#CCCCCC", linewidth = 0.4),
      axis.ticks = element_line(colour = "#CCCCCC", linewidth = 0.3),
      axis.ticks.length = unit(3, "pt"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = rel(0.85)),
      legend.text = element_text(size = rel(0.8)),
      legend.key.size = unit(12, "pt"),
      legend.key = element_rect(fill = NA, colour = NA),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.margin = margin(t = 8),
      panel.grid.major.y = element_line(colour = "#EEEEEE", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      plot.background = element_rect(fill = NA, colour = NA),
      strip.text = element_text(face = "bold", size = rel(0.9)),
      strip.background = element_rect(fill = NATURE_GREY_LIGHT, colour = NA),
      plot.margin = margin(12, 12, 12, 12)
    )
}

cat("[OK] Core functions loaded\n")