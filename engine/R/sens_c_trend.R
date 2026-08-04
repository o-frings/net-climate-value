# =============================================================================
# sens_c_trend.R  —  does c rise in the future projections, and by how much?
# =============================================================================
# Standalone SI analysis (not part of run_engine.R). Run from analysis/, after
# sens_within_country_correlation.R:
#   Rscript engine/R/sens_c_trend.R
#
# Question. The engine applies an RCP uplift to the mean hazard lambda but holds the
# within-country spatial correlation c fixed. If warming also synchronises losses, a fixed c
# overstates end-of-century diversification. sens_within_country_correlation.R shows c is not
# detectably drifting in calendar time; this script asks the projection question: if lambda
# rises by the amount the engine already assumes, what does the record imply c does?
#
# Elasticity rather than a time trend. Extrapolating a 39-year calendar trend to 2100 is
# unbounded and unconnected to the emission scenario, and the engine's projection mechanism is
# the hazard uplift U, not the year. The primary estimate is therefore the elasticity of
# correlation to hazard, measured within each country across non-overlapping time blocks:
#     z(rho_block) = a_country + beta * log(lambda_block)
# Within-country variation only, so cross-country differences (biome, size, species mix) do not
# enter. beta is then applied to the engine's own uplift:
#     z_future = z_observed + beta * log(1 + U)
# with U from derived_biome_params.csv (U_100 for RCP4.5, U_100_rcp85 for RCP8.5), keeping the
# projection consistent with the rest of the model. The calendar-time slope is reported
# alongside as a cross-check.
#
# Fisher z. A correlation is bounded and a linear trend in rho is not, so extrapolating to 2100
# can leave [0,1). Fitting and projecting on z = atanh(rho) and back-transforming keeps every
# projected c admissible.
#
# Inference. Countries share calendar years, so a common European trend moves every country's
# slope together. A bootstrap over countries treats them as independent and understates the
# standard error: it puts the calendar slope's interval clear of zero, contradicting the
# correctly specified no-drift result in sens_within_country_correlation.R. Both are reported;
# the permutation p-value is the one to quote, since it draws one year permutation and applies
# it to every country, preserving the shared-year dependence. Bootstrap intervals are retained
# to show cross-country spread and are labelled anticonservative in the output.
#
# Two multipliers, reported separately because they answer different questions:
#   c_mult_climate_only = c_future / c_observed   the climate-driven relative rise. Applied on
#       top of the assumed c, it keeps the present-day assumption (which the data show is
#       already conservative) and adds the climate response.
#   c_mult_recalibrated = c_future / c_assumed    replaces the assumed c with the projected one.
#       Lower, because the assumed c already exceeds the observed one.
#
# Range of the calendar route. Extrapolating the fitted slope linearly in z over ten decades
# implies c around 0.5 by 2100 (N_eff about 2), i.e. within-country diversification nearly
# gone. That is outside anything in the record and rests on a slope fitted to 32 years at
# p = 0.06, so it is an upper bound for stress-testing rather than a forecast; the 2050 figures
# are the defensible end. sens_c_uplift.R additionally reports the K = 1 case, which bounds the
# effect whatever c does.
#
# Emits: engine/output/sens_c_trend_country.csv    (per country: elasticity, calendar slope)
#        engine/output/sens_c_trend_pooled.csv     (pooled slopes + permutation p)
#        engine/output/sens_c_trend_projection.csv (per biome x scenario: projected c, mults)
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
cat("[sens_c_trend] hazard-elasticity of the within-country correlation\n")

MIN_HEX     <- 8L
MIN_NONZERO <- 8L
# 5 blocks over the 39-yr record and 4 over the 32-yr window without the constructed years
# gives ~8-yr blocks in both, so the two windows are comparable AND the joint (time + hazard)
# fit is identified in both — at 3 blocks it would not be.
N_BLOCK     <- 5L
MIN_BLOCK   <- 3L      # a country needs this many usable blocks to fit a slope
N_BOOT      <- 4000L
N_PERM      <- 1000L
SEED        <- 4242L
CONSTRUCTED <- 2017:2023

.need <- function(p) { if (!file.exists(p)) stop("MISSING input: ", p); p }
H <- as.data.frame(readRDS(.need("data/processed/efda_hex_rates.rds")))
if (anyNA(H$lambda_natural)) stop("NA lambda_natural in the hexagon series")

mean_pair_cor <- function(M) {
  s <- apply(M, 2, sd); M <- M[, s > 0, drop = FALSE]
  if (ncol(M) < 3L) return(NA_real_)
  R <- cor(M); mean(R[upper.tri(R)])
}
fz <- function(r) atanh(pmin(pmax(r, -0.999), 0.999))
inv_fz <- function(z) tanh(z)

# --- per-country year x hexagon matrices, on a common year grid -------------------
build_mats <- function(years) {
  mats <- list()
  for (cn in sort(unique(H$country))) {
    d <- H[H$country == cn & H$year %in% years, ]
    M <- tapply(d$lambda_natural, list(as.character(d$year), as.character(d$hex_id)), identity)
    if (anyNA(M)) stop("incomplete hexagon panel for ", cn)
    ok <- colSums(M > 0) >= MIN_NONZERO
    M <- M[, ok, drop = FALSE]
    if (ncol(M) >= MIN_HEX) mats[[cn]] <- M
  }
  ny <- unique(vapply(mats, nrow, integer(1)))
  if (length(ny) != 1L)
    stop("countries differ in year count; the shared-year permutation needs a common grid")
  mats
}

# --- slopes for one country, given a row order (identity = observed) ---------------
# Blocks are contiguous and non-overlapping, so successive rho estimates are independent
# given the year process; overlapping windows would make the slope's nominal SE too small.
slopes_one <- function(M, blk, ord) {
  Mp <- M[ord, , drop = FALSE]
  rho <- vapply(blk, function(ii) mean_pair_cor(Mp[ii, , drop = FALSE]), numeric(1))
  lam <- vapply(blk, function(ii) mean(Mp[ii, , drop = FALSE]), numeric(1))
  mid <- vapply(blk, mean, numeric(1))              # block position, fixed under permutation
  keep <- is.finite(rho) & lam > 0
  if (sum(keep) < MIN_BLOCK)
    return(c(beta = NA_real_, dec = NA_real_, beta_j = NA_real_, dec_j = NA_real_,
             nb = sum(keep)))
  z <- fz(rho[keep]); lg <- log(lam[keep]); md <- mid[keep]
  # Joint model separates the two stories: does correlation still rise with TIME once the
  # block's hazard level is controlled for? If the calendar coefficient collapses here, the
  # marginal calendar trend was hazard-driven (or an attenuation artefact: low-hazard early
  # blocks have more near-zero hexagon-years, which biases a correlation toward zero).
  # Needs one more block than a bivariate fit to be identified.
  j <- if (sum(keep) >= MIN_BLOCK + 1L) coef(lm(z ~ md + lg)) else c(NA, NA, NA)
  c(beta   = unname(coef(lm(z ~ lg))[2]),
    dec    = 10 * unname(coef(lm(z ~ md))[2]),
    beta_j = unname(j[3]),
    dec_j  = 10 * unname(j[2]),
    nb     = sum(keep))
}

run_window <- function(years, nb_blocks, label) {
  mats <- build_mats(years)
  ny <- nrow(mats[[1]])
  blk <- split(seq_len(ny), cut(seq_len(ny), breaks = nb_blocks, labels = FALSE))
  cat(sprintf("[sens_c_trend] %-14s %d countries, %d blocks of ~%d yr\n",
              label, length(mats), length(blk), round(ny / length(blk))))

  obs <- t(vapply(mats, function(M) slopes_one(M, blk, seq_len(ny)), numeric(5)))
  cty <- data.frame(year_window = label, country = rownames(obs),
                    n_blocks = obs[, "nb"], beta_hazard = obs[, "beta"],
                    slope_per_decade = obs[, "dec"],
                    beta_hazard_joint = obs[, "beta_j"],
                    slope_per_decade_joint = obs[, "dec_j"], stringsAsFactors = FALSE)

  # Shared-year permutation null: ONE permutation per replicate, applied to every country,
  # so cross-country co-movement is preserved under the null.
  set.seed(SEED)
  nulls <- vapply(seq_len(N_PERM), function(i) {
    ord <- sample(ny)
    s <- t(vapply(mats, function(M) slopes_one(M, blk, ord), numeric(5)))
    c(beta = mean(s[, "beta"], na.rm = TRUE), dec = mean(s[, "dec"], na.rm = TRUE),
      beta_j = mean(s[, "beta_j"], na.rm = TRUE), dec_j = mean(s[, "dec_j"], na.rm = TRUE))
  }, numeric(4))

  bootci <- function(v) {
    v <- v[is.finite(v)]
    if (length(v) < 3L) return(c(NA_real_, NA_real_))
    set.seed(SEED)
    unname(quantile(vapply(seq_len(N_BOOT), function(i)
      mean(sample(v, length(v), replace = TRUE)), numeric(1)), c(0.025, 0.975)))
  }
  pv <- function(nl, o) { nl <- nl[is.finite(nl)]
    if (!length(nl) || !is.finite(o)) NA_real_ else mean(abs(nl) >= abs(o)) }
  bh <- cty$beta_hazard[is.finite(cty$beta_hazard)]
  ob <- mean(bh); od <- mean(cty$slope_per_decade, na.rm = TRUE)
  ci_b <- bootci(cty$beta_hazard); ci_d <- bootci(cty$slope_per_decade)
  list(cty = cty, pooled = data.frame(
    year_window = label, n_countries = length(bh),
    beta_hazard_mean = ob, beta_boot_lo = ci_b[1], beta_boot_hi = ci_b[2],
    beta_p_permutation = pv(nulls["beta", ], ob),
    n_positive = sum(bh > 0),
    p_sign = if (length(bh) >= 2) binom.test(sum(bh > 0), length(bh), 0.5)$p.value else NA_real_,
    slope_per_decade_mean = od, slope_boot_lo = ci_d[1], slope_boot_hi = ci_d[2],
    slope_p_permutation = pv(nulls["dec", ], od),
    beta_joint_mean = mean(cty$beta_hazard_joint, na.rm = TRUE),
    beta_joint_p_permutation = pv(nulls["beta_j", ], mean(cty$beta_hazard_joint, na.rm = TRUE)),
    slope_joint_mean = mean(cty$slope_per_decade_joint, na.rm = TRUE),
    slope_joint_p_permutation = pv(nulls["dec_j", ], mean(cty$slope_per_decade_joint, na.rm = TRUE)),
    record_mid_year = mean(range(years)), stringsAsFactors = FALSE))
}

allY <- sort(unique(H$year))
res <- list(run_window(allY, N_BLOCK, "full"),
            run_window(setdiff(allY, CONSTRUCTED), N_BLOCK - 1L, "excl_2017_2023"))
cty <- do.call(rbind, lapply(res, `[[`, "cty")); rownames(cty) <- NULL
pooled <- do.call(rbind, lapply(res, `[[`, "pooled"))
write.csv(cty, "engine/output/sens_c_trend_country.csv", row.names = FALSE)
write.csv(pooled, "engine/output/sens_c_trend_pooled.csv", row.names = FALSE)

cat("\n--- pooled slopes (quote the permutation p; the bootstrap CI is anticonservative) ---\n")
for (i in seq_len(nrow(pooled))) with(pooled[i, ], {
  cat(sprintf("  %-14s n=%2d\n", year_window, n_countries))
  cat(sprintf("      elasticity beta   %+.3f  boot [%+.3f, %+.3f]  perm p=%.3f  %d/%d positive (sign p=%.3f)\n",
              beta_hazard_mean, beta_boot_lo, beta_boot_hi, beta_p_permutation,
              n_positive, n_countries, p_sign))
  cat(sprintf("      calendar z/decade %+.4f  boot [%+.4f, %+.4f]  perm p=%.3f\n",
              slope_per_decade_mean, slope_boot_lo, slope_boot_hi, slope_p_permutation))
  cat(sprintf("      JOINT (both regressors): beta %+.3f (perm p=%.3f)   calendar %+.4f z/decade (perm p=%.3f)\n",
              beta_joint_mean, beta_joint_p_permutation,
              slope_joint_mean, slope_joint_p_permutation))
})

# --- project c under the engine's own uplift --------------------------------------
bp   <- read.csv(.need("engine/output/derived_biome_params.csv"), stringsAsFactors = FALSE)
cpar <- read.csv(.need("engine/params/biome_correlation.csv"), stringsAsFactors = FALSE)
obs  <- read.csv(.need("engine/output/sens_within_country_c_biome.csv"), stringsAsFactors = FALSE)
pubm <- unique(read.csv(.need("engine/output/mc_summary.csv"), stringsAsFactors = FALSE)$biome)

SCEN <- list(list(name = "rcp45_2050", col = "U_50"),
             list(name = "rcp85_2050", col = "U_50_rcp85"),
             list(name = "rcp45_2100", col = "U_100"),
             list(name = "rcp85_2100", col = "U_100_rcp85"))

proj <- do.call(rbind, lapply(unique(cty$year_window), function(w) {
  pl <- pooled[pooled$year_window == w, ]
  o  <- obs[obs$basis == "detrended" & obs$year_window == w & obs$biome != "ALL", ]
  if (!nrow(o)) stop("no observed c rows for window ", w)
  do.call(rbind, lapply(seq_len(nrow(o)), function(i) {
    bm <- o$biome[i]; c_obs <- o$c_observed_mean[i]
    c_ass <- cpar$c[cpar$biome == bm]
    if (length(c_ass) != 1L) stop("no assumed c for biome ", bm)
    row <- bp[bp$biome == bm, ]
    if (nrow(row) != 1L) stop("no derived params for biome ", bm)
    el <- do.call(rbind, lapply(SCEN, function(s) {
      U <- row[[s$col]]
      if (!is.finite(U)) stop("non-finite ", s$col, " for ", bm)
      # Same multiplicative hazard uplift the buffer already applies, mapped onto the
      # correlation through the fitted elasticity.
      shift <- function(b) inv_fz(fz(c_obs) + b * log(1 + U))
      cf <- shift(pl$beta_hazard_mean); chi <- shift(pl$beta_boot_hi)
      data.frame(year_window = w, biome = bm, in_published_mc = bm %in% pubm,
                 route = "hazard_elasticity", scenario = s$name, uplift_U = U,
                 bound_basis = "bootstrap CI on beta (over countries; anticonservative)",
                 c_observed = c_obs, c_assumed = c_ass,
                 c_future = cf, c_future_lo = shift(pl$beta_boot_lo), c_future_hi = chi,
                 c_mult_climate_only = cf / c_obs, c_mult_climate_only_hi = chi / c_obs,
                 c_mult_recalibrated = cf / c_ass, c_mult_recalibrated_hi = chi / c_ass,
                 stringsAsFactors = FALSE)
    }))
    # --- calendar route -----------------------------------------------------------
    # NOT scenario-tied: a linear extrapolation of the observed calendar slope, which is the
    # only channel that actually survives inference here. Reported as a BOUND, not a
    # projection: it assumes a century of continued linear rise in z and is blind to the
    # emission pathway, so it cannot be attributed to RCP4.5 vs 8.5. The hazard-controlled
    # (joint) slope is the primary, since the marginal slope partly reflects rising hazard.
    cal <- do.call(rbind, lapply(c(2050, 2100), function(yr) {
      dec <- (yr - pl$record_mid_year) / 10
      shc <- function(sl) inv_fz(fz(c_obs) + sl * dec)
      cf <- shc(pl$slope_joint_mean)
      data.frame(year_window = w, biome = bm, in_published_mc = bm %in% pubm,
                 route = "calendar_extrapolation",
                 scenario = paste0("calendar_", yr), uplift_U = NA_real_,
                 # NOT a confidence interval: lo = marginal slope, hi = hazard-controlled
                 # (joint) slope. A specification range between two estimators.
                 bound_basis = "marginal vs hazard-controlled slope (specification range, not a CI)",
                 c_observed = c_obs, c_assumed = c_ass,
                 c_future = cf,
                 c_future_lo = shc(pl$slope_per_decade_mean),   # marginal slope = lower
                 c_future_hi = shc(pl$slope_joint_mean),
                 c_mult_climate_only = cf / c_obs,
                 c_mult_climate_only_hi = shc(pl$slope_joint_mean) / c_obs,
                 c_mult_recalibrated = cf / c_ass,
                 c_mult_recalibrated_hi = shc(pl$slope_joint_mean) / c_ass,
                 stringsAsFactors = FALSE)
    }))
    rbind(el, cal)
  }))
}))
rownames(proj) <- NULL
write.csv(proj, "engine/output/sens_c_trend_projection.csv", row.names = FALSE)

cat("\n--- projected c (detrended, excl. constructed years; * = not in mc_summary) ---\n")
p <- proj[proj$year_window == "excl_2017_2023", ]
p <- p[order(p$route, p$biome, p$scenario), ]
for (i in seq_len(nrow(p))) with(p[i, ],
  cat(sprintf("  %-14s%-2s %-14s U=%-5s c %.3f -> %.3f (%.3f-%.3f)  x_vs_observed %.2f  x_vs_assumed %.2f\n",
              biome, if (!in_published_mc) " *" else "", scenario,
              if (is.na(uplift_U)) "n/a" else sprintf("%.2f", uplift_U),
              c_observed, c_future, c_future_lo, c_future_hi,
              c_mult_climate_only, c_mult_recalibrated)))
cat("  ranges in ( ): hazard_elasticity = bootstrap CI on beta (anticonservative);\n")
cat("                 calendar_extrapolation = marginal vs hazard-controlled slope, NOT a CI\n")
cat("\n[sens_c_trend] wrote 3 tables to engine/output/\n")
