# =============================================================================
# 09_buffer_trajectory.R  —  empirical TVaR99 buffer trajectory 2025-2100 (fig4)
# =============================================================================
# The climate buffer trajectory, computed the CLEAN way: the engine's own
# empirical TVaR99 bootstrap (03_buffer) evaluated at each calendar year's climate
# uplift, correlation-limited N_eff=1/c. Supersedes the legacy parametric
# experience-rated trajectory (the old fig4 already anchored its parametric shape
# to this same empirical TVaR99 at year 1; this removes the parametric middleman).
#
# Per biome b, RCP r, the uplift ramps linearly 2025->2100 (script 10 def):
#   uplift(t) = 0.05 + ((t-2025)/75) * (U_100_r[b] - 0.05)
# buffer(t) reflects the CONTEMPORANEOUS uplift uplift(t) sustained over the H-yr
# liability window (a project facing year-t climate), not the headline's ramp from
# present-day climate (which pins every year's window at ~present and understates
# the late-century reserve). Buffer is monotone in this sustained level, so per
# country we bootstrap TVaR99 at a GRID of sustained-uplift anchors (bootstrap_buffer
# with a flat uplift_vec), interpolate, then evaluate along uplift(t). Country ->
# biome by forest-area weight (as headline);
# biome -> scenario by CRCF practice-area weight (08). Outputs:
#   buffer_biome_trajectory.csv   (year x biome x forest_type x rcp)  -> ED spread
#   scenario_buffer_trajectory.csv(year x scenario x rcp)             -> fig4 panel a
#   scenario_buffer_summary_clean.csv (scenario x rcp: mean, b_2100)  -> fig4 / verify
# Reuses globals from 03_buffer.R (bootstrap_buffer, R_of, files, file2country,
# sev_by_country, dom_biome, c_by_biome) and biome_data (01) + scenario_practices (08).
# =============================================================================
cat("[09_buffer_trajectory] empirical TVaR99 buffer trajectory 2025-2100...\n")

TRAJ_N_MC   <- 6000L          # per-anchor bootstrap depth (interp smooths MC noise)
TRAJ_SEED   <- 4040L
H_TRAJ      <- 40L            # pool liability horizon (matches headline H40)
N_UANCHOR   <- 6L            # uplift grid points
years       <- 2025:2100

bd <- biome_data                                  # from 01_data (U_100 per biome/rcp)
U100 <- list(RCP45 = setNames(bd$U_100, bd$biome),
             RCP85 = setNames(bd$U_100_rcp85, bd$biome))
biomes3 <- c("Boreal", "Temperate", "Mediterranean")

uplift_traj <- function(biome, rcp) {
  Uend <- U100[[rcp]][[biome]]
  0.05 + ((years - 2025) / 75) * (Uend - 0.05)
}

# --- per country x forest_type: buffer-vs-uplift interpolation -----------------
set.seed(TRAJ_SEED)
efda_sum2 <- as.data.frame(readRDS("data/processed/efda_country_summary.rds"))
fkha_by_country <- tapply(efda_sum2$forest_kha, efda_sum2$country_root, sum, na.rm = TRUE)

# max uplift across biomes/RCPs sets the grid ceiling
u_ceiling <- max(unlist(U100))
u_grid <- seq(0.05, u_ceiling, length.out = N_UANCHOR)

country_interp <- list()                          # key "country|ft" -> approxfun
biome_of_country <- list()
for (f in files) {
  ann  <- as.data.frame(readRDS(f))
  cn   <- file2country[[unique(ann$country)]]
  if (is.null(cn) || is.na(cn)) next
  bm <- dom_biome[[cn]]; if (is.null(bm) || is.na(bm)) next
  cc <- c_by_biome[[bm]]
  series <- ann$lambda_natural[ann$year >= 1986]
  sev <- sev_by_country[[cn]]; if (is.null(sev) || is.na(sev)) next
  biome_of_country[[cn]] <- bm
  for (ft in c("broadleaf", "conifer")) {
    R <- R_of(bm, ft)
    # Each anchor = a sustained climate LEVEL u over the H-yr window (flat uplift_vec),
    # so buffer(t) reflects year-t climate held over the liability window — not the
    # headline's ramp-from-present, which pins every year at present-day climate.
    bvals <- vapply(u_grid, function(u)
      bootstrap_buffer(series, sev, u, R, cc, H_TRAJ, n_mc = TRAJ_N_MC,
                       uplift_vec = rep(u, H_TRAJ))$b, numeric(1))
    country_interp[[paste(cn, ft, sep = "|")]] <-
      approxfun(u_grid, bvals, rule = 2)          # rule=2: clamp outside grid
  }
}

# --- biome-level trajectory = forest-area-weighted country buffers --------------
# Parameterised by an uplift multiplier (u_mult = 1 -> central path): the buffer-
# vs-uplift interp is evaluated along uplift_traj * u_mult. Reused by the forward
# Monte Carlo that builds the fig4 uncertainty band below.
biome_buf_at <- function(u_mult) {
  rows <- list()
  for (rcp in c("RCP45", "RCP85")) {
    for (bm in biomes3) {
      up <- uplift_traj(bm, rcp) * u_mult
      cns <- names(biome_of_country)[vapply(names(biome_of_country),
              function(c) identical(biome_of_country[[c]], bm), logical(1))]
      cns <- cns[cns %in% names(fkha_by_country)]
      if (length(cns) == 0) next
      w <- fkha_by_country[cns]; w <- w / sum(w)
      for (ft in c("broadleaf", "conifer")) {
        bmat <- vapply(cns, function(c) {
          fn <- country_interp[[paste(c, ft, sep = "|")]]
          if (is.null(fn)) rep(NA_real_, length(up)) else fn(up)
        }, numeric(length(up)))
        rows[[paste(rcp, bm, ft)]] <- data.frame(
          year = years, biome = bm, forest_type = ft, rcp = rcp,
          uplift = up, buffer_rate = as.numeric(bmat %*% w), stringsAsFactors = FALSE)
      }
    }
  }
  rows
}
biome_traj_rows <- biome_buf_at(1.0)               # central trajectory
biome_traj <- do.call(rbind, biome_traj_rows); rownames(biome_traj) <- NULL
write.csv(biome_traj, "engine/output/buffer_biome_trajectory.csv", row.names = FALSE)

# --- scenario-level trajectory = CRCF practice-area-weighted biome buffers ------
scen <- read.csv("engine/output/scenario_practices.csv", stringsAsFactors = FALSE)
practices <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
ft_of_practice <- tapply(practices$forest_type, practices$practice, function(x) x[1])
scen$forest_type <- ft_of_practice[scen$practice]
scen <- scen[scen$eu_area_ha > 0 & scen$biome %in% biomes3 & !is.na(scen$forest_type), ]
btraj_key <- function(rcp, bm, ft) paste(rcp, bm, ft)

# aggregate biome-trajectory rows to a scenario buffer path (area-weighted)
agg_scen <- function(rows, sc, rcp) {
  s <- scen[scen$scenario == sc, ]; wsum <- sum(s$eu_area_ha); acc <- rep(0, length(years))
  for (i in seq_len(nrow(s))) {
    tr <- rows[[btraj_key(rcp, s$biome[i], s$forest_type[i])]]
    if (!is.null(tr)) acc <- acc + (s$eu_area_ha[i] / wsum) * tr$buffer_rate
  }
  acc
}
scen_rows <- list(); summ_rows <- list()
for (rcp in c("RCP45", "RCP85")) {
  for (sc in unique(scen$scenario)) {
    acc <- agg_scen(biome_traj_rows, sc, rcp)        # smooth climate-driven trend
    scen_rows[[paste(rcp, sc)]] <- data.frame(
      year = years, scenario = sc, rcp = rcp, buffer_rate = acc, stringsAsFactors = FALSE)
    summ_rows[[paste(rcp, sc)]] <- data.frame(
      scenario = sc, rcp = rcp, mean_buffer_rate = mean(acc),
      buffer_2025 = acc[1], buffer_2100 = acc[length(acc)], stringsAsFactors = FALSE)
  }
}
scenario_summ <- do.call(rbind, summ_rows); rownames(scenario_summ) <- NULL

# fig4 uncertainty band: forward Monte Carlo over the regime-parameter uncertainty
# the engine already samples — disturbance multiplier lam ~ U(0.5,1.5) and climate-
# uplift multiplier u ~ U(0.7,1.3) — propagated through the actual buffer-vs-uplift
# curve per year (not an assumed envelope). The band fans out toward 2100 because
# the uplift, and hence the climate multiplier's leverage on the buffer, grows.
set.seed(909L); N_BAND <- 400L
band <- lapply(seq_len(N_BAND), function(d) {
  br <- biome_buf_at(runif(1, 0.7, 1.3)); lm <- runif(1, 0.5, 1.5)
  vapply(names(scen_rows), function(k) {
    p <- strsplit(k, " ", fixed = TRUE)[[1]]
    agg_scen(br, paste(p[-1], collapse = " "), p[1]) * lm
  }, numeric(length(years)))                         # years x scenarios
})
for (k in names(scen_rows)) {
  M <- vapply(band, function(x) x[, k], numeric(length(years)))   # years x draws
  # Central line = per-year MC median of the band draws (matches fig5/ED median
  # convention), replacing the central-parameter path set above. They coincide to
  # <0.1pp here (symmetric multipliers), so the summary CSV — built above and used
  # only for the colour ramp — is left on the central-parameter path.
  scen_rows[[k]]$buffer_rate     <- apply(M, 1, median)  # central line (per-year MC median)
  scen_rows[[k]]$buffer_rate_min <- apply(M, 1, min)     # full MC range (fig4 panel a band)
  scen_rows[[k]]$buffer_rate_max <- apply(M, 1, max)
}
scenario_traj <- do.call(rbind, scen_rows); rownames(scenario_traj) <- NULL
write.csv(scenario_traj, "engine/output/scenario_buffer_trajectory.csv", row.names = FALSE)
write.csv(scenario_summ, "engine/output/scenario_buffer_summary_clean.csv", row.names = FALSE)

# =============================================================================
# fig5 panel b: forward NCV-adjusted area trajectory (year x scenario x rcp)
# =============================================================================
# L and T are time-invariant; only the buffer rises with climate. So each MC
# draw's net share scales by the deterministic biome buffer growth g(t):
#   net(t,draw) = net0(draw) * (1 - b0(draw)*g(t)) / (1 - b0(draw))
# with g(t) = biome buffer(t)/biome buffer(2025). Then per scenario x rcp x year
# area(t,draw) = sum_p area_p / max(net_p(t,draw), 0.02); report median + IQR.
.hlmc  <- readRDS("engine/output/mc_results.rds"); .hlmc <- .hlmc[.hlmc$is_anchor, ]
.anc   <- practices[as.logical(practices$is_anchor), ]
anc_bm <- setNames(.anc$biome, .anc$practice)              # anchor biome per practice
biome_growth <- function(bm, rcp) {                        # buffer(t)/buffer(2025) path
  z <- biome_traj[biome_traj$biome == bm & biome_traj$forest_type == "broadleaf" &
                  biome_traj$rcp == rcp, ]
  if (nrow(z) == 0) return(rep(1, length(years)))
  z <- z[order(z$year), ]; z$buffer_rate / z$buffer_rate[1]
}
draws_of <- function(p, col) {                             # per-draw vector, anchor variant
  bm <- anc_bm[[p]]; if (is.null(bm)) return(NULL)
  d <- .hlmc[.hlmc$practice == p & .hlmc$biome == bm, ]; if (!nrow(d)) return(NULL)
  d <- d[d$species == d$species[1], ]; d[order(d$iteration), col]
}
area_fwd_rows <- list()
for (rcp in c("RCP45", "RCP85")) {
  for (sc in unique(scen$scenario)) {
    s <- scen[scen$scenario == sc, ]; amat <- NULL
    for (p in unique(s$practice)) {
      net0 <- draws_of(p, "net_share"); b0 <- draws_of(p, "b"); if (is.null(net0)) next
      g <- biome_growth(anc_bm[[p]], rcp)                  # length = years
      bt <- pmin(outer(b0, g), 0.98)                       # n_iter x n_year
      net_t <- net0 * (1 - bt) / (1 - b0)                  # recycle net0,b0 down columns
      area_p <- sum(s$eu_area_ha[s$practice == p])
      amat <- if (is.null(amat)) area_p / pmax(net_t, 0.02) else amat + area_p / pmax(net_t, 0.02)
    }
    if (is.null(amat)) next
    amat <- amat / 1e6
    area_fwd_rows[[paste(rcp, sc)]] <- data.frame(
      year = years, scenario = sc, rcp = rcp,
      area_mean = apply(amat, 2, median),
      area_p25  = apply(amat, 2, quantile, 0.25),
      area_p75  = apply(amat, 2, quantile, 0.75), stringsAsFactors = FALSE)
  }
}
scenario_area_forward <- do.call(rbind, area_fwd_rows); rownames(scenario_area_forward) <- NULL
write.csv(scenario_area_forward, "engine/output/scenario_area_forward.csv", row.names = FALSE)

# =============================================================================
# ed_fig_buffer_practice_spread panel b: biome disturbance rate lambda(t)
# =============================================================================
# Observed (EFDA 1986-2023) + projected lambda_obs*(1+uplift(t)) per biome x rcp.
.efd <- read.csv("data/processed/efda_biome_timeseries.csv", stringsAsFactors = FALSE)
.dbp <- read.csv("engine/output/derived_biome_params.csv", stringsAsFactors = FALSE)
# Unlike the buffer (a reserve LEVEL), the disturbance rate is the raw stochastic
# process — it genuinely varies year to year. Each realization scatters around the
# expected regime through the SAME uncertainty the headline Monte Carlo carries:
# a rate-level multiplier (lambda_mult) and a climate-uplift multiplier (U_mult),
# both drawn from engine/params/sensitivity_ranges.csv — the identical ranges
# 07_montecarlo.R uses — plus a per-year bootstrap of the historical EFDA relative
# interannual scatter for the year-to-year texture. So the line-to-line spread
# reproduces the MC's hazard uncertainty resolved over time; observed rows are
# realised data. Simulation-based, not assumed.
.sr_haz <- read.csv("engine/params/sensitivity_ranges.csv", stringsAsFactors = FALSE)
.lm_rng <- unlist(.sr_haz[.sr_haz$param == "lambda_mult", c("min", "max")])  # U(0.50,1.50)
.um_rng <- unlist(.sr_haz[.sr_haz$param == "U_mult",      c("min", "max")])  # U(0.70,1.30)
set.seed(414L); ND_DIST <- 3000L; N_SHOW <- 60L   # N_SHOW realizations kept for the spaghetti
dist_rows <- list(); path_rows <- list()
for (bm in biomes3) {
  oh <- .efd[.efd$biome == bm & .efd$year <= 2023, ]
  dist_rows[[paste("H", bm)]] <- data.frame(
    year = oh$year, biome = bm, scenario = "Historical", rate = oh$lambda_natural,
    rate_p10 = NA_real_, rate_p90 = NA_real_, stringsAsFactors = FALSE)
  lam0 <- .dbp$lambda_obs[.dbp$biome == bm]
  rel  <- oh$lambda_natural / mean(oh$lambda_natural)   # historical relative annual scatter
  for (rcp in c("RCP45", "RCP85")) {
    up <- uplift_traj(bm, rcp)
    M  <- vapply(seq_len(ND_DIST), function(d)            # year x draw predictive sample
      lam0 * runif(1, .lm_rng[1], .lm_rng[2]) *           # rate-level uncertainty (lambda_mult)
        (1 + up * runif(1, .um_rng[1], .um_rng[2])) *     # climate-uplift uncertainty (U_mult)
        sample(rel, length(up), replace = TRUE),          # interannual scatter (empirical)
      numeric(length(up)))
    dist_rows[[paste(rcp, bm)]] <- data.frame(
      year = years, biome = bm, scenario = paste0("Future: ", rcp),
      rate = apply(M, 1, median),                         # per-year MEDIAN (headline convention)
      rate_p10 = apply(M, 1, quantile, 0.10),
      rate_p90 = apply(M, 1, quantile, 0.90), stringsAsFactors = FALSE)
    # Keep a sample of individual realizations (each a jagged path) for the
    # spaghetti overlay — the figure draws these as thin faint lines so the
    # year-to-year hazard variation is visible directly, not just as a band.
    path_rows[[paste(rcp, bm)]] <- data.frame(
      year = rep(years, N_SHOW), biome = bm, rcp = rcp,
      draw = rep(seq_len(N_SHOW), each = length(years)),
      rate = as.vector(M[, seq_len(N_SHOW)]), stringsAsFactors = FALSE)
  }
}
buffer_biome_disturbance <- do.call(rbind, dist_rows); rownames(buffer_biome_disturbance) <- NULL
write.csv(buffer_biome_disturbance, "engine/output/buffer_biome_disturbance.csv", row.names = FALSE)
disturbance_paths <- do.call(rbind, path_rows); rownames(disturbance_paths) <- NULL
write.csv(disturbance_paths, "engine/output/buffer_biome_disturbance_paths.csv", row.names = FALSE)

cat(sprintf("[09_buffer_trajectory] OK — grid n_mc=%d, %d uplift anchors. RCP8.5 buffer_2100 by scenario: %.0f-%.0f%%\n",
            TRAJ_N_MC, N_UANCHOR,
            100*min(scenario_summ$buffer_2100[scenario_summ$rcp=="RCP85"]),
            100*max(scenario_summ$buffer_2100[scenario_summ$rcp=="RCP85"])))
