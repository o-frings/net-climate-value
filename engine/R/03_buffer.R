# =============================================================================
# 03_buffer.R  —  empirical TVaR99 buffer rate (catastrophe-model bootstrap)
# =============================================================================
# CORRELATION-LIMITED POOLING variant. Per country x forest-type x horizon:
#   1. take that country's observed annual NATURAL disturbance series (EFDA,
#      1986-2023; harvest excluded)
#   2. bootstrap H-year disturbance paths (sample with replacement), apply the
#      Grünig climate-uplift trajectory and the biome-specific forest-type
#      vulnerability R; the pool is spread over N_eff = round(1/c) effective
#      decorrelated cells (c = biome spatial correlation): within each year the
#      cell-level disturbed fraction is Beta-distributed about the country rate
#      with intra-cluster correlation c (mean-preserving, bounded), and the pool
#      loss compounds (1 - prod(1 - loss_t)) so it saturates below 1.
#   3. buffer rate b = TVaR99 (expected shortfall above the 99th percentile) of
#      the cumulative-loss distribution, GPD tail above the 75th pct.
# As c -> 0 this recovers the fully-diversified country aggregate. This matches
# analysis/R/26_empirical_buffer.R with BUFFER_SPATIAL_CORR = TRUE. Reads ONLY
# committed primary data + sourced param CSVs.
# =============================================================================
cat("[03_buffer] correlation-limited empirical TVaR99 bootstrap from EFDA...\n")

.need <- function(p) { if (!file.exists(p)) stop("MISSING input: ", p); p }
.mcv  <- setNames(read.csv(.need("engine/params/model_constants.csv"))$value,
                  read.csv("engine/params/model_constants.csv")$name)
.k <- function(n) { v <- .mcv[[n]]; if (is.null(v)||is.na(v)) stop("missing const ",n); unname(v) }

# biome x forest-type vulnerability R (Marinelli 2026 fitted ratios)
ft_R <- read.csv(.need("engine/params/forest_type_R.csv"), stringsAsFactors = FALSE)
stopifnot(all(c("biome","forest_type","R") %in% names(ft_R)))
R_of <- function(biome, ft) {
  v <- ft_R$R[ft_R$biome == biome & ft_R$forest_type == ft]
  if (length(v) != 1) stop("no R for biome=", biome, " forest_type=", ft)
  v
}
# biome spatial correlation c -> effective pool size N_eff = round(1/c)
cc_df <- read.csv(.need("engine/params/biome_correlation.csv"), stringsAsFactors = FALSE)
stopifnot(all(c("biome","c") %in% names(cc_df)))
c_by_biome <- setNames(cc_df$c, cc_df$biome)

BUF_N_MC <- 20000   # higher than the pipeline's 5000 to shrink clean-side MC noise
BUF_SEED <- 2026

# --- expected severity per event (compound Bernoulli-Beta mixture) -----------
# E[Z] = p + (1-p) * a/(a+b)   (manuscript Eq expected_severity)
expected_severity <- function(p_sr) {
  a <- .k("partial_severity_a"); b <- .k("partial_severity_b")
  p_sr + (1 - p_sr) * a / (a + b)
}

# --- GPD peaks-over-threshold tail (MLE, no external package) ----------------
.gpd_nll <- function(par, excess) {
  sigma <- exp(par[1]); xi <- par[2]
  if (abs(xi) < 1e-8) return(length(excess) * log(sigma) + sum(excess) / sigma)
  z <- 1 + xi * excess / sigma
  if (any(z <= 0)) return(1e10)
  length(excess) * log(sigma) + (1 + 1 / xi) * sum(log(z))
}
# GPD-vs-empirical incidence counter. Falling back to the empirical quantile gives a
# materially different b, and b is in every published net_share, so the fallback must be
# countable rather than invisible. Reported by .gpd_report() at the end of this file.
.gpd_tally <- c(few_obs = 0L, few_excess = 0L, no_converge = 0L, fitted = 0L)
.gpd_bump <- function(w) .gpd_tally[[w]] <<- .gpd_tally[[w]] + 1L
fit_gpd <- function(x, threshold_q = 0.75) {
  x <- x[is.finite(x)]
  if (length(x) < 40) { .gpd_bump("few_obs"); return(NULL) }
  u <- as.numeric(quantile(x, threshold_q)); excess <- x[x > u] - u
  if (length(excess) < 20) { .gpd_bump("few_excess"); return(NULL) }
  fit <- tryCatch(optim(c(log(mean(excess)), 0.1), .gpd_nll, excess = excess,
                        method = "Nelder-Mead", control = list(maxit = 500)),
                  error = function(e) NULL)
  if (is.null(fit) || fit$convergence != 0) { .gpd_bump("no_converge"); return(NULL) }
  .gpd_bump("fitted")
  list(sigma = exp(fit$par[1]), xi = fit$par[2], threshold = u,
       exceedance_rate = length(excess) / length(x))
}
# TVaR (expected shortfall) above prob p, GPD tail above threshold else empirical
tvar_semi <- function(p, x, fit) {
  if (is.null(fit) || fit$xi >= 1) { v <- quantile(x, p, na.rm = TRUE); return(mean(x[x >= v])) }
  var_p <- if (1 - p >= fit$exceedance_rate) {
    as.numeric(quantile(x, p, na.rm = TRUE))
  } else {
    pc <- 1 - (1 - p) / fit$exceedance_rate
    if (abs(fit$xi) < 1e-8) fit$threshold + fit$sigma * (-log(1 - pc))
    else fit$threshold + fit$sigma / fit$xi * ((1 - pc)^(-fit$xi) - 1)
  }
  if (1 - p >= fit$exceedance_rate) return(mean(x[x >= var_p], na.rm = TRUE))
  (var_p + fit$sigma - fit$xi * fit$threshold) / (1 - fit$xi)
}

# VaR (quantile) at prob p — GPD tail above threshold, else empirical quantile.
# The 99th-percentile loss; TVaR99 (expected shortfall above it) is the headline.
var_semi <- function(p, x, fit) {
  if (is.null(fit) || fit$xi >= 1 || 1 - p >= fit$exceedance_rate)
    return(as.numeric(quantile(x, p, na.rm = TRUE)))
  pc <- 1 - (1 - p) / fit$exceedance_rate
  if (abs(fit$xi) < 1e-8) fit$threshold + fit$sigma * (-log(1 - pc))
  else fit$threshold + fit$sigma / fit$xi * ((1 - pc)^(-fit$xi) - 1)
}

# --- per-country inputs: EFDA series, severity, climate uplift, dominant biome
.efda_dir <- .need("data/processed/efda_country_rates")
bmap <- read.csv(.need("data/country_biome_map.csv"), stringsAsFactors = FALSE)
file2country <- setNames(bmap$country, bmap$efda_filename)

# whole-country files only (exclude France/Italy sub-zone splits)
files <- list.files(.efda_dir, "\\.rds$", full.names = TRUE)
files <- files[!grepl("_(Temperate|Mediterranean)\\.rds$", files)]

# severity + U + dominant biome per country (area-weighted across any sub-zones)
efda_sum <- as.data.frame(readRDS(.need("data/processed/efda_country_summary.rds")))
# forest_kha weights the dominant-biome choice below, which fixes c and R for a whole
# country bootstrap. An NA would silently become zero weight, so require completeness.
if (anyNA(efda_sum$forest_kha)) stop("efda_country_summary: NA forest_kha")
gru_c    <- as.data.frame(readRDS(.need("data/processed/gruenig_country_uplift_factors.rds")))
sev_by_country <- tapply(seq_len(nrow(efda_sum)), efda_sum$country_root, function(ix)
  sum(efda_sum$forest_kha[ix] * efda_sum$severity[ix]) / sum(efda_sum$forest_kha[ix]))
gru45 <- gru_c[gru_c$scen == "RCP4.5", ]
U50_by_country <- tapply(gru45$U_50, gru45$country, mean)   # avg if >1 row
# Dominant biome per country (by forest area) selects the single c and R for the
# whole-country bootstrap — matches biome_of() in 26_empirical_buffer.R.
.cb <- aggregate(forest_kha ~ country_root + biome, efda_sum, sum)
.cb <- .cb[order(.cb$country_root, -.cb$forest_kha), ]
dom_biome <- setNames(.cb$biome[!duplicated(.cb$country_root)],
                      .cb$country_root[!duplicated(.cb$country_root)])

# --- bootstrap one (country, forest_type, horizon): correlation-limited pool --
# Returns list(b, se): b = TVaR99 buffer rate; se = batch-means standard error of
# that estimate (split the n_mc draws into K_BATCH batches, recompute TVaR99 per
# batch, se = sd(batch)/sqrt(K_BATCH)). The SE is the sampling uncertainty of the
# buffer RATE — the Monte Carlo layer (07) perturbs the buffer by this real SE.
# Mirrors analysis/R/26_empirical_buffer.R (the published b_se).
K_BATCH <- 20L
bootstrap_buffer <- function(series, severity, U_50, R_mult, c_corr, H, n_mc = BUF_N_MC,
                             uplift_vec = NULL) {
  if (length(series) < 10) return(list(b = NA_real_, var99 = NA_real_, se = NA_real_))
  E_Z    <- expected_severity(severity)
  # Default (headline, issued-today): climate ramps 5% -> U_50 over the H-yr window.
  # The fig4 trajectory (09) supplies uplift_vec to model the CONTEMPORANEOUS
  # calendar-year climate sustained over the window instead.
  uplift <- if (is.null(uplift_vec)) 0.05 + (seq_len(H) / H) * (U_50 - 0.05) else uplift_vec
  K  <- max(1L, round(1 / c_corr))                    # effective decorrelated cells
  ab <- (1 - c_corr) / c_corr                         # Beta concentration (a + b)
  cum_loss <- vapply(seq_len(n_mc), function(i) {
    mu   <- pmin(sample(series, H, replace = TRUE) * (1 + uplift) * R_mult, 0.999)
    A    <- pmax(rep(mu, times = K) * ab, 1e-9)        # H*K Beta shape-a (K cells)
    cell <- matrix(rbeta(H * K, A, ab - A), nrow = H)
    1 - prod(1 - rowMeans(cell) * E_Z)                 # saturating pool loss
  }, numeric(1))
  fit <- fit_gpd(cum_loss)
  b     <- min(tvar_semi(0.99, cum_loss, fit), 1.0)   # TVaR99 (headline)
  var99 <- min(var_semi(0.99, cum_loss, fit), 1.0)    # VaR99 (parametric_vs_empirical)
  bsz <- floor(length(cum_loss) / K_BATCH)
  batch <- vapply(seq_len(K_BATCH), function(j) {
    seg <- cum_loss[((j - 1) * bsz + 1):(j * bsz)]
    min(tvar_semi(0.99, seg, fit_gpd(seg)), 1.0)
  }, numeric(1))
  # b_se is the width of the MC buffer perturbation, hence of the published NCV
  # interval. Dropping a failed batch while still dividing by sqrt(K_BATCH) would
  # understate it, so require all batches to have produced a value.
  if (anyNA(batch)) stop("batch-means SE: ", sum(is.na(batch)), " of ", K_BATCH,
                         " batches failed")
  list(b = b, var99 = var99, se = stats::sd(batch) / sqrt(K_BATCH))
}

# --- grid: country x forest_type x horizon -----------------------------------
set.seed(BUF_SEED)
H_default <- 40
H_perm    <- .k("H_perm")
countries <- intersect(names(sev_by_country), names(U50_by_country))

# Per-country natural-disturbance series, kept so the practice-level buffer helper
# (practice_buffer_rate, below) can re-bootstrap with practice-specific multipliers.
series_by_country <- list()

clean_buffer <- do.call(rbind, lapply(files, function(f) {
  ann  <- as.data.frame(readRDS(f))
  fkey <- unique(ann$country)
  cn   <- file2country[[fkey]]
  if (is.null(cn) || is.na(cn) || !cn %in% countries) return(NULL)
  bm <- dom_biome[[cn]]
  if (is.null(bm) || is.na(bm)) stop("no dominant biome for ", cn)
  cc <- c_by_biome[[bm]]
  if (is.null(cc) || is.na(cc)) stop("no correlation c for biome ", bm)
  series <- ann$lambda_natural[ann$year >= 1986]
  series_by_country[[cn]] <<- series
  sev <- sev_by_country[[cn]]; U50 <- U50_by_country[[cn]]
  if (is.na(sev) || is.na(U50)) stop("missing severity/U_50 for ", cn)
  do.call(rbind, lapply(c("broadleaf", "conifer"), function(ft) {
    R <- R_of(bm, ft)
    h40  <- bootstrap_buffer(series, sev, U50, R, cc, H_default)
    h100 <- bootstrap_buffer(series, sev, U50, R, cc, H_perm)
    data.frame(country = cn, forest_type = ft,
               b_TVaR99_H40  = h40$b,  b_VaR99_H40  = h40$var99,  se_TVaR99_H40  = h40$se,
               b_TVaR99_H100 = h100$b, b_VaR99_H100 = h100$var99, se_TVaR99_H100 = h100$se,
               stringsAsFactors = FALSE)
  }))
}))
rownames(clean_buffer) <- NULL
dir.create("engine/output", showWarnings = FALSE, recursive = TRUE)
write.csv(clean_buffer, "engine/output/clean_buffer_rates.csv", row.names = FALSE)
cat(sprintf("[03_buffer] OK — %d country x forest-type rows (corr-limited N_eff=1/c, n_mc=%d, seed=%d)\n",
            nrow(clean_buffer), BUF_N_MC, BUF_SEED))
.gpd_report <- function(stage) {
  n <- sum(.gpd_tally); fb <- n - .gpd_tally[["fitted"]]
  cat(sprintf("[03_buffer] GPD tail %s: %d of %d fits succeeded, %d fell back to the empirical quantile (few_obs %d, few_excess %d, no_converge %d)\n",
              stage, .gpd_tally[["fitted"]], n, fb, .gpd_tally[["few_obs"]],
              .gpd_tally[["few_excess"]], .gpd_tally[["no_converge"]]))
}
.gpd_report("after country grid")

# =============================================================================
# practice_buffer_rate() — practice-level empirical TVaR99 buffer
# =============================================================================
# Applies the practice-specific risk multipliers (R_mult, lambda_mult, c_mult;
# practices.csv / manuscript Supp Table mgmt) on top of the biome base parameters,
# then re-runs the SAME empirical TVaR99 bootstrap as the biome buffer above:
#   R_mult      scales species/structural vulnerability  -> R = R_of(biome,ft)*R_mult
#   lambda_mult scales event frequency                   -> series * lambda_mult
#   c_mult      scales the spatial-correlation loading   -> c = c_biome*c_mult
# Because R_mult and lambda_mult both scale the per-year disturbed fraction mu
# multiplicatively, and c_mult only re-sizes N_eff=round(1/c) and the Beta pooling,
# the practice buffer is the biome bootstrap evaluated at the practice's hazard.
# Result is the forest-area-weighted mean over the biome's EFDA zones (matching
# biome_buffer() in 04). Cached by argument signature; deterministic per key.
PRACTICE_BUF_N_MC <- 8000L    # per-country depth (interp-free; batch SE reported)
.pbuf_cache <- new.env(parent = emptyenv())

# whole-country buffer with practice multipliers folded in (dominant-biome base)
.country_buf_mult <- function(cn, ft, H, R_mult, lambda_mult, c_mult, uplift_vec, n_mc) {
  bm  <- dom_biome[[cn]]
  R   <- R_of(bm, ft) * R_mult
  cc  <- c_by_biome[[bm]] * c_mult
  ser <- series_by_country[[cn]] * lambda_mult          # lambda_mult scales the rate
  bootstrap_buffer(ser, sev_by_country[[cn]], U50_by_country[[cn]], R, cc, H,
                   n_mc = n_mc, uplift_vec = uplift_vec)
}

# forest-area-weighted practice buffer for a biome x forest_type at horizon H.
# uplift_vec = NULL -> headline ramp (0.05 -> U_50); pass rep(U_end, H) for a
# sustained climate level (e.g. end-of-century RCP8.5, for the buffer-range report).
practice_buffer_rate <- function(biome, ft, H, R_mult = 1, lambda_mult = 1, c_mult = 1,
                                 uplift_vec = NULL, n_mc = PRACTICE_BUF_N_MC) {
  key <- paste("pb", biome, ft, H, R_mult, lambda_mult, c_mult,
               if (is.null(uplift_vec)) "ramp" else paste0("flat", round(uplift_vec[1], 5)),
               n_mc, sep = "|")
  if (!is.null(.pbuf_cache[[key]])) return(.pbuf_cache[[key]])
  zb <- efda_sum[efda_sum$biome == biome & !is.na(efda_sum$forest_kha) &
                 efda_sum$forest_kha > 0, c("country_root", "forest_kha")]
  zb <- aggregate(forest_kha ~ country_root, zb, sum)
  zb <- zb[zb$country_root %in% names(series_by_country), ]
  if (nrow(zb) == 0) stop("practice_buffer_rate: no EFDA countries for biome ", biome)
  set.seed(2026L + (sum(utf8ToInt(key)) %% 100000L))     # reproducible per signature
  bs <- lapply(zb$country_root, .country_buf_mult, ft = ft, H = H, R_mult = R_mult,
               lambda_mult = lambda_mult, c_mult = c_mult, uplift_vec = uplift_vec, n_mc = n_mc)
  w  <- zb$forest_kha / sum(zb$forest_kha)
  res <- list(b  = sum(w * vapply(bs, function(x) x$b,  numeric(1))),
              se = sum(w * vapply(bs, function(x) x$se, numeric(1))))
  .pbuf_cache[[key]] <- res
  res
}
