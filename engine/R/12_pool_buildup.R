# =============================================================================
# 12_pool_buildup.R  —  buffer diversification vs pool size (ed_fig_pool_buildup)
# =============================================================================
# How far the required buffer falls toward the fully-diversified level as the pool
# spans more countries. Builds a per-country cumulative-loss matrix (n_mc draws)
# with SHARED resampled years (preserves cross-country climate correlation), same
# corr-limited Beta-pool model as 03_buffer. Then:
#   div_ratio(K) = TVaR99(K-country pool loss) / (K-pool per-project avg buffer)
# swept K=1..nC, random enrolment (mean over 300 subsets) vs largest-forest-first.
# ~1.0 at K=1 (per-project, fig4a) down to ~0.53 at full pool (fig4b diversified).
# Reuses 03_buffer globals (files, file2country, dom_biome, c_by_biome,
# sev_by_country, U50_by_country, expected_severity) + clean_buffer_rates (per-
# country b) + scenario_practices (biome weights). Assumes 03,04,08 have run.
# =============================================================================
cat("[12_pool_buildup] buffer diversification vs pool size...\n")

POOL_N_MC <- 10000L; POOL_H <- 40L; POOL_SEED <- 2026L
R_EFF <- (0.85 + 1.37) / 2          # avg broadleaf/conifer R (representative pool)
YRS <- 1986:2023

# per-country buffer b (mean TVaR99_H40 over forest types)
cbr <- read.csv("engine/output/clean_buffer_rates.csv", stringsAsFactors = FALSE)
b_by_country <- tapply(cbr$b_TVaR99_H40, cbr$country, mean, na.rm = TRUE)

# biome weights for the mixed_reducing scenario (CRCF area per biome)
scp <- read.csv("engine/output/scenario_practices.csv", stringsAsFactors = FALSE)
scp <- scp[scp$scenario == "mixed_reducing" & scp$eu_area_ha > 0, ]
bw_area <- tapply(scp$eu_area_ha, scp$biome, sum)
biome_weights <- bw_area / sum(bw_area)

# assemble per-country inputs (whole-country files from 03_buffer globals)
ser <- list(); meta <- list()
for (f in files) {
  cn <- file2country[[unique(as.data.frame(readRDS(f))$country)]]
  if (is.null(cn) || is.na(cn)) next
  bm <- dom_biome[[cn]]; if (is.null(bm) || is.na(bm)) next
  if (is.null(b_by_country[[cn]]) || is.na(b_by_country[[cn]])) next
  if (is.null(sev_by_country[[cn]]) || is.null(U50_by_country[[cn]])) next
  d <- as.data.frame(readRDS(f)); d <- d[match(YRS, d$year), ]
  ser[[cn]] <- ifelse(is.na(d$lambda_natural), 0, d$lambda_natural)
  meta[[cn]] <- list(biome = bm, cc = c_by_biome[[bm]], sev = sev_by_country[[cn]],
                     U50 = U50_by_country[[cn]], b = b_by_country[[cn]],
                     fkha = sum(efda_sum$forest_kha[efda_sum$country_root == cn], na.rm = TRUE))
}
cn_all <- names(ser); nC <- length(cn_all)

# per-country cumulative-loss matrix (n_mc x nC), shared year indices
set.seed(POOL_SEED)
idx <- matrix(sample(seq_along(YRS), POOL_N_MC * POOL_H, replace = TRUE), POOL_N_MC, POOL_H)
loss <- matrix(0, POOL_N_MC, nC)
for (j in seq_len(nC)) {
  m <- meta[[cn_all[j]]]; s <- ser[[cn_all[j]]]
  ez <- expected_severity(m$sev)                          # 03_buffer
  up <- 0.05 + (seq_len(POOL_H) / POOL_H) * (m$U50 - 0.05)
  mu <- pmin(matrix(s[idx], POOL_N_MC, POOL_H) * rep(1 + up, each = POOL_N_MC) * R_EFF, 0.999)
  ab <- (1 - m$cc) / m$cc; A <- pmax(mu * ab, 1e-9); K <- max(1L, round(1 / m$cc))
  ff <- matrix(0, POOL_N_MC, POOL_H)
  for (k in seq_len(K))
    ff <- ff + matrix(rbeta(POOL_N_MC * POOL_H, as.vector(A), ab - as.vector(A)), POOL_N_MC, POOL_H)
  ff <- ff / K
  loss[, j] <- 1 - exp(rowSums(log1p(-ff * ez)))
}

# country weights: biome weight x within-biome forest-area share
biomes_c <- vapply(cn_all, function(c) meta[[c]]$biome, character(1))
fkha_c   <- vapply(cn_all, function(c) meta[[c]]$fkha, numeric(1))
b_c      <- vapply(cn_all, function(c) meta[[c]]$b, numeric(1))
w <- numeric(nC)
for (bm in names(biome_weights)) { inb <- biomes_c == bm
  if (any(inb)) w[inb] <- biome_weights[[bm]] * fkha_c[inb] / sum(fkha_c[inb]) }
w <- w / sum(w)

tv99 <- function(x) mean(x[x >= quantile(x, 0.99)])
div_ratio_K <- function(S) { wS <- w[S]; if (sum(wS) <= 0) return(NA_real_)
  wS <- wS / sum(wS); tv99(as.vector(loss[, S, drop = FALSE] %*% wS)) / sum(wS * b_c[S]) }

set.seed(7); n_draw <- 300
rand <- vapply(seq_len(nC), function(K) mean(replicate(if (K == nC) 1 else n_draw,
  div_ratio_K(sample(nC, K))), na.rm = TRUE), numeric(1))
ordg <- order(-w)
greedy <- vapply(seq_len(nC), function(K) div_ratio_K(ordg[seq_len(K)]), numeric(1))

buildup <- rbind(
  data.frame(K = seq_len(nC), div_ratio = rand,   ordering = "Random enrolment"),
  data.frame(K = seq_len(nC), div_ratio = greedy, ordering = "Largest forest nations first"))
write.csv(buildup, "engine/output/pool_buildup.csv", row.names = FALSE)

asym <- rand[nC]
cat(sprintf("[12_pool_buildup] OK — %d countries, n_mc=%d. full-pool div_ratio=%.2f; within 10%% at K=%d (random)/%d (largest-first)\n",
            nC, POOL_N_MC, asym, which(rand <= asym * 1.10)[1], which(greedy <= asym * 1.10)[1]))
