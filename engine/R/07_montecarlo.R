# =============================================================================
# 07_montecarlo.R  —  Monte Carlo uncertainty + PRCC (manuscript fig3 b/c/d, ED1)
# =============================================================================
# Propagates parameter uncertainty through the engine's deterministic core for
# every practice x biome x species. Faithful port of analysis/R run_mc_project:
#   draws  : r,g ~ U; k0 = clamp(r-g, .005,.025); eps_mult,lambda_mult,U_mult ~ U;
#            kappa ~ Triangular(min=.33, mode=.60, max=1.27): mode = central carbon/
#            harvest ratio (kappa<1 is the GTM central tendency, Daigneault 2025), with
#            a thin upper tail to 1.27 for the protect-low-density / replace-high-density
#            case (Schulte 2025). min/max from sensitivity_ranges.csv; mode = .const(kappa).
#   T      : calc_T_k0 with phi_add=0, tau_1=0, H_ref=Inf (the headline benchmark);
#            tau_2 = H_perm for legally-protected practices
#   L      : rho_rep perturbed by the elasticity-ratio multiplier eps_mult, scaled
#            by kappa and harvest displacement x; clipped to [-ell_max, 1] (x<=0 cap)
#   b      : drawn ~ N(b_central, b_se), b_central = headline buffer (floored),
#            b_se = biome batch-means SE (engine/output/biome_buffer.csv)
#   net_share = (1-L)(1-T)(1-b)   per iteration
# net_share does NOT depend on lambda_mult/U_mult (buffer is drawn, not recomputed
# from lambda) — identical to run_mc_project; those draws are kept for PRCC only.
# Seed = 42 + practice-row index (independent reproducible stream per practice).
# Assumes 02_model.R (rho parts, calc_x_afforestation, net_share) and 04_headline.R
# (clean_headline.csv, biome_buffer.csv) have run.
# =============================================================================
cat("[07_montecarlo] Monte Carlo uncertainty + PRCC...\n")
N_MC_ITER <- if (nzchar(Sys.getenv("ENGINE_MC_ITER"))) as.integer(Sys.getenv("ENGINE_MC_ITER")) else 10000L

practices <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
headline  <- read.csv("engine/output/clean_headline.csv", stringsAsFactors = FALSE)
bbuf      <- read.csv("engine/output/biome_buffer.csv", stringsAsFactors = FALSE)
.bd       <- read.csv("engine/output/derived_biome_params.csv", stringsAsFactors = FALSE)
U50_by_biome <- setNames(.bd$U_50, .bd$biome)   # central climate uplift per biome
.sr       <- read.csv("engine/params/sensitivity_ranges.csv", stringsAsFactors = FALSE)
rng <- function(p) { r <- .sr[.sr$param == p, ]; if (nrow(r) != 1) stop("range missing: ", p); c(r$min, r$max) }
# inverse-CDF triangular sampler (base R has no rtriangle); a=min, c=mode, b=max
rtri <- function(n, a, c, b) {
  u  <- runif(n)
  fc <- (c - a) / (b - a)
  ifelse(u < fc, a + sqrt(u * (b - a) * (c - a)),
                 b - sqrt((1 - u) * (b - a) * (b - c)))
}
H_perm    <- .const("H_perm"); tau_1 <- .const("tau_1"); ell_max <- .const("ell_max")

hkey <- function(p, b, s) paste(p, b, s, sep = "\r")
h_b  <- setNames(headline$b, hkey(headline$practice, headline$biome, headline$species))

# rho-component breakdown for the eps_mult perturbation (engine 02_model: LE, PPW)
rho_parts <- function(practice, biome) {
  w <- PPW[PPW$practice == practice, c("SL", "PW", "WF")]
  if (nrow(w) != 1) stop("no product weights for ", practice)
  out <- list()
  for (pc in c("SL", "PW", "WF")) {
    if (w[[pc]] == 0) next
    e <- LE[LE$biome == biome & LE$product == pc, ]
    if (nrow(e) != 1) stop("no elasticities for ", biome, " x ", pc)
    num <- e$u * e$eps_s_dom + (1 - e$u) * e$s * e$eps_s_imp
    out[[length(out) + 1]] <- c(weight = w[[pc]], num = num, abs_d = abs(e$eps_d))
  }
  if (length(out) == 0) return(NULL)             # harvest-neutral: rho = 0
  do.call(rbind, out)
}

# --- per-practice MC ----------------------------------------------------------
mc_one <- function(i) {
  row <- practices[i, ]
  set.seed(42L + i)
  n <- N_MC_ITER
  r <- runif(n, rng("r")[1], rng("r")[2]); g <- runif(n, rng("g")[1], rng("g")[2])
  eps_mult <- runif(n, rng("eps_mult")[1], rng("eps_mult")[2])
  lambda_mult <- runif(n, rng("lambda_mult")[1], rng("lambda_mult")[2])
  U_mult <- runif(n, rng("U_mult")[1], rng("U_mult")[2])
  kappa <- rtri(n, rng("kappa")[1], .const("kappa"), rng("kappa")[2])
  k0 <- pmin(pmax(r - g, rng("k0")[1]), rng("k0")[2])

  protected <- isTRUE(as.logical(row$legally_protected))
  tau_2_temp <- if (protected) H_perm else row$tau_2
  # T via calc_T_k0 (phi_add=0, H_ref=Inf): denom = 1/k0, num = (1-exp(-k0*tau))/k0
  T_vec <- 1 - pmin(pmax((1 - exp(-k0 * (tau_2_temp - tau_1))), 0), 1)

  x <- resolve_x(row)                            # 02_model (dynamic for afforestation)
  rp <- rho_parts(row$practice, row$biome)
  rho_vec <- if (is.null(rp)) rep(0, n) else
    rowSums(vapply(seq_len(nrow(rp)), function(k)
      rp[k, "weight"] * (eps_mult * rp[k, "num"]) / (rp[k, "abs_d"] + eps_mult * rp[k, "num"]),
      numeric(n)))
  L_vec <- pmin(pmax(kappa * rho_vec * x, -ell_max), 1)
  if (x <= 0) L_vec <- pmin(L_vec, ell_max)

  # Buffer: PARAMETER uncertainty in the hazard propagates into the reserve
  # (actuarial premium principle: b is a risk loading on expected loss, ~linear in
  # the climate-adjusted disturbance rate). Scale the empirical TVaR99 anchor
  # b_central by the drawn hazard relative to central, so lambda and U drive the
  # buffer channel. lambda_obs cancels in the ratio:
  #   scale = lambda_mult * (1 + U50*U_mult) / (1 + U50)   (=1 at central params)
  # A small additive batch-means SE retains estimation uncertainty of the TVaR99.
  b_central <- h_b[[hkey(row$practice, row$biome, row$species)]]
  if (is.null(b_central)) stop("no headline b for ", row$practice, "/", row$biome)
  bb <- bbuf[bbuf$biome == row$biome & bbuf$forest_type == row$forest_type, ]
  if (nrow(bb) != 1) stop("no biome_buffer for ", row$biome, "/", row$forest_type)
  b_se <- if (protected) bb$se_H100 else bb$se_H40
  U50 <- U50_by_biome[[row$biome]]; if (is.null(U50)) stop("no U_50 for ", row$biome)
  scale_haz <- lambda_mult * (1 + U50 * U_mult) / (1 + U50)
  b_vec <- pmin(pmax(b_central * scale_haz + rnorm(n, 0, b_se), 0), 1)

  ns <- (1 - L_vec) * (1 - T_vec) * (1 - b_vec)
  data.frame(practice = row$practice, biome = row$biome, species = row$species,
             is_anchor = as.logical(row$is_anchor), iteration = seq_len(n),
             r = r, g = g, k0 = k0, eps_mult = eps_mult, lambda_mult = lambda_mult,
             U_mult = U_mult, kappa = kappa,
             L = L_vec, T = T_vec, b = b_vec, net_share = ns,
             delta_leak = L_vec, delta_temp = (1 - L_vec) * T_vec,
             delta_buf = (1 - L_vec) * (1 - T_vec) * b_vec, stringsAsFactors = FALSE)
}

all_mc <- do.call(rbind, lapply(seq_len(nrow(practices)), mc_one))
rownames(all_mc) <- NULL

# --- summary per practice x biome x species -----------------------------------
agg <- function(d) data.frame(
  practice = d$practice[1], biome = d$biome[1], species = d$species[1],
  is_anchor = d$is_anchor[1],
  mean_share = mean(d$net_share), sd_share = sd(d$net_share),
  p5_share = quantile(d$net_share, .05), p50_share = quantile(d$net_share, .50),
  p95_share = quantile(d$net_share, .95),
  mean_leak = mean(d$delta_leak), mean_temp = mean(d$delta_temp),
  mean_buf = mean(d$delta_buf), stringsAsFactors = FALSE)
mc_summary <- do.call(rbind, by(all_mc, list(all_mc$practice, all_mc$biome, all_mc$species),
                                function(d) if (nrow(d)) agg(d) else NULL))
rownames(mc_summary) <- NULL
mc_summary <- mc_summary[order(-mc_summary$mean_share), ]

# --- cross-biome rank robustness (manuscript: Cross-biome transferability) ----
# Pairwise Spearman of practice MC-median net_share between biome pairs that share
# >=3 practices. Also reports the number of practice x biome combinations evaluated.
.cb <- aggregate(p50_share ~ practice + biome, mc_summary, mean)   # collapse species
.biomes_cb <- sort(unique(.cb$biome))
.cb_rows <- list()
for (ii in seq_along(.biomes_cb)) for (jj in seq_along(.biomes_cb)) if (ii < jj) {
  b1 <- .biomes_cb[ii]; b2 <- .biomes_cb[jj]
  m <- merge(.cb[.cb$biome == b1, c("practice", "p50_share")],
             .cb[.cb$biome == b2, c("practice", "p50_share")],
             by = "practice", suffixes = c("_1", "_2"))
  if (nrow(m) >= 3)
    .cb_rows[[length(.cb_rows) + 1]] <- data.frame(
      biome_1 = b1, biome_2 = b2, n_shared = nrow(m),
      spearman = cor(m$p50_share_1, m$p50_share_2, method = "spearman"))
}
cb_robust <- do.call(rbind, .cb_rows)
n_combos <- nrow(unique(mc_summary[, c("practice", "biome")]))
write.csv(cb_robust, "engine/output/rank_robustness_biome.csv", row.names = FALSE)
cat(sprintf("[07_montecarlo] cross-biome: %d practice x biome combos; pairwise rho %.3f-%.3f over %d biome pairs\n",
            n_combos, min(cb_robust$spearman), max(cb_robust$spearman), nrow(cb_robust)))

# --- PRCC per practice (partial rank correlation, params vs net_share) --------
mc_params <- c("k0", "r", "g", "kappa", "lambda_mult", "eps_mult", "U_mult")
compute_prcc <- function(d) {
  ranked <- as.data.frame(lapply(d[, c(mc_params, "net_share")], rank))
  # drop zero-variance inputs (e.g. constant for a degenerate practice)
  vp <- mc_params[vapply(mc_params, function(p) sd(ranked[[p]]) > 0, logical(1))]
  if (sd(ranked$net_share) == 0 || length(vp) < 2) {
    return(data.frame(parameter = mc_params, prcc = NA_real_))
  }
  do.call(rbind, lapply(vp, function(p) {
    others <- setdiff(vp, p)
    res_p <- residuals(lm(ranked[[p]] ~ ., data = ranked[others]))
    res_y <- residuals(lm(ranked$net_share ~ ., data = ranked[others]))
    data.frame(parameter = p, prcc = cor(res_p, res_y))
  }))
}
prcc_all <- do.call(rbind, by(all_mc, list(all_mc$practice, all_mc$biome, all_mc$species),
  function(d) { if (!nrow(d)) return(NULL)
    pr <- compute_prcc(d); pr$practice <- d$practice[1]; pr$biome <- d$biome[1]
    pr$species <- d$species[1]; pr$is_anchor <- d$is_anchor[1]; pr }))
rownames(prcc_all) <- NULL

saveRDS(all_mc, "engine/output/mc_results.rds")
write.csv(mc_summary, "engine/output/mc_summary.csv", row.names = FALSE)
write.csv(prcc_all,   "engine/output/mc_prcc.csv", row.names = FALSE)
anc <- mc_summary[mc_summary$is_anchor, ]
cat(sprintf("[07_montecarlo] OK — %d iters x %d practices; anchor mean_share %.1f%%-%.1f%%\n",
            N_MC_ITER, nrow(practices), 100*min(anc$mean_share), 100*max(anc$mean_share)))
