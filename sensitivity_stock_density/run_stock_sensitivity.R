# =============================================================================
# run_stock_sensitivity.R  —  STANDALONE, REMOVABLE sensitivity branch
# =============================================================================
# PURPOSE
#   Quantify the effect of ACTIVATING the stock-density hazard term (S/S_ref)^beta
#   in the buffer, which the canonical engine (engine/R/03_buffer.R) does NOT
#   apply. Manuscript Eq (4) writes lambda_adj = gamma * lambda * R^alpha *
#   (S/S_ref)^beta, but the headline empirical TVaR99 bootstrap uses only
#   lambda * (1+uplift) * R_mult. This script re-runs that bootstrap with the
#   extra multiplicative factor s = (S_bar / S_ref_biome)^beta per practice and
#   reports the deltas on per-practice buffer b and net climate value.
#
# ISOLATION (by design — easy to delete)
#   * Lives entirely under analysis/sensitivity_stock_density/.
#   * Sources NOTHING from engine/R; touches no engine file.
#   * Reads engine inputs/outputs READ-ONLY (primary data + committed CSVs).
#   * Writes ONLY to analysis/sensitivity_stock_density/output/.
#   * To remove this experiment entirely: delete the folder. Nothing else
#     references it.
#
# RUN (from analysis/):
#   Rscript sensitivity_stock_density/run_stock_sensitivity.R
#
# METHOD
#   The bootstrap helpers below are a VENDORED COPY of engine/R/03_buffer.R
#   (GPD tail + correlation-limited Beta pool + TVaR99), with ONE addition:
#   an s_factor argument multiplying the per-year disturbance mean `mu`.
#   For each anchor practice we compute, paired on the same RNG seed:
#       b_recomp_base  : s_factor = 1            (should reproduce engine b)
#       b_recomp_stock : s_factor = (S_bar/S_ref)^beta
#   The paired multiplicative effect m = b_recomp_stock / b_recomp_base is then
#   applied to the CANONICAL engine baseline b (engine/output/clean_headline.csv)
#   so the reported stock buffer is free of this script's MC bias:
#       b_stock = b_engine * m
#   NCV uses the engine's own L and T:  net = (1-L)(1-T)(1-b).
# =============================================================================

stopifnot(basename(getwd()) == "analysis")
SELF <- "sensitivity_stock_density"
OUT  <- file.path(SELF, "output")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
cat("=== STOCK-DENSITY (S/S_ref)^beta SENSITIVITY (standalone) ===\n")

N_MC <- 5000L          # bootstrap draws (manuscript-stated count; engine uses 20000)
SEED_BASE <- 7000L

# --- constants ---------------------------------------------------------------
mcv <- setNames(read.csv("engine/params/model_constants.csv")$value,
                read.csv("engine/params/model_constants.csv")$name)
k   <- function(n) { v <- mcv[[n]]; if (is.null(v) || is.na(v)) stop("missing const ", n); unname(v) }
BETA  <- k("beta")
A_SEV <- k("partial_severity_a")
B_SEV <- k("partial_severity_b")

# biome reference stock S_ref (engine-derived) + correlation c + forest-type R
bp     <- read.csv("engine/output/derived_biome_params.csv", stringsAsFactors = FALSE)
S_ref  <- setNames(bp$S_ref, bp$biome)
ftR    <- read.csv("engine/params/forest_type_R.csv", stringsAsFactors = FALSE)
R_of   <- function(biome, ft) { v <- ftR$R[ftR$biome == biome & ftR$forest_type == ft]
  if (length(v) != 1) stop("no R for ", biome, "/", ft); v }
ccdf   <- read.csv("engine/params/biome_correlation.csv", stringsAsFactors = FALSE)
c_by_biome <- setNames(ccdf$c, ccdf$biome)

# --- vendored bootstrap (engine/R/03_buffer.R) + s_factor --------------------
expected_severity <- function(p_sr) p_sr + (1 - p_sr) * A_SEV / (A_SEV + B_SEV)

.gpd_nll <- function(par, excess) {
  sigma <- exp(par[1]); xi <- par[2]
  if (abs(xi) < 1e-8) return(length(excess) * log(sigma) + sum(excess) / sigma)
  z <- 1 + xi * excess / sigma
  if (any(z <= 0)) return(1e10)
  length(excess) * log(sigma) + (1 + 1 / xi) * sum(log(z))
}
fit_gpd <- function(x, threshold_q = 0.75) {
  x <- x[is.finite(x)]; if (length(x) < 40) return(NULL)
  u <- as.numeric(quantile(x, threshold_q)); excess <- x[x > u] - u
  if (length(excess) < 20) return(NULL)
  fit <- tryCatch(optim(c(log(mean(excess)), 0.1), .gpd_nll, excess = excess,
                        method = "Nelder-Mead", control = list(maxit = 500)),
                  error = function(e) NULL)
  if (is.null(fit) || fit$convergence != 0) return(NULL)
  list(sigma = exp(fit$par[1]), xi = fit$par[2], threshold = u,
       exceedance_rate = length(excess) / length(x))
}
tvar_semi <- function(p, x, fit) {
  if (is.null(fit) || fit$xi >= 1) { v <- quantile(x, p, na.rm = TRUE); return(mean(x[x >= v])) }
  var_p <- if (1 - p >= fit$exceedance_rate) as.numeric(quantile(x, p, na.rm = TRUE)) else {
    pc <- 1 - (1 - p) / fit$exceedance_rate
    if (abs(fit$xi) < 1e-8) fit$threshold + fit$sigma * (-log(1 - pc))
    else fit$threshold + fit$sigma / fit$xi * ((1 - pc)^(-fit$xi) - 1)
  }
  if (1 - p >= fit$exceedance_rate) return(mean(x[x >= var_p], na.rm = TRUE))
  (var_p + fit$sigma - fit$xi * fit$threshold) / (1 - fit$xi)
}
# ONLY DIFFERENCE vs engine: `s_factor` multiplies mu (the (S/S_ref)^beta term).
bootstrap_buffer <- function(series, severity, U_50, R_mult, c_corr, H,
                             s_factor = 1, n_mc = N_MC, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(series) < 10) return(NA_real_)
  E_Z    <- expected_severity(severity)
  uplift <- 0.05 + (seq_len(H) / H) * (U_50 - 0.05)
  K  <- max(1L, round(1 / c_corr))
  ab <- (1 - c_corr) / c_corr
  cum_loss <- vapply(seq_len(n_mc), function(i) {
    mu   <- pmin(sample(series, H, replace = TRUE) * (1 + uplift) * R_mult * s_factor, 0.999)
    A    <- pmax(rep(mu, times = K) * ab, 1e-9)
    cell <- matrix(rbeta(H * K, A, ab - A), nrow = H)
    1 - prod(1 - rowMeans(cell) * E_Z)
  }, numeric(1))
  min(tvar_semi(0.99, cum_loss, fit_gpd(cum_loss)), 1.0)
}

# --- per-country EFDA inputs (mirror engine/R/03_buffer.R loading) -----------
efda_sum <- as.data.frame(readRDS("data/processed/efda_country_summary.rds"))
bmap     <- read.csv("data/country_biome_map.csv", stringsAsFactors = FALSE)
gru_c    <- as.data.frame(readRDS("data/processed/gruenig_country_uplift_factors.rds"))
file2country <- setNames(bmap$country, bmap$efda_filename)

files <- list.files("data/processed/efda_country_rates", "\\.rds$", full.names = TRUE)
files <- files[!grepl("_(Temperate|Mediterranean)\\.rds$", files)]

sev_by_country <- tapply(seq_len(nrow(efda_sum)), efda_sum$country_root, function(ix)
  sum(efda_sum$forest_kha[ix] * efda_sum$severity[ix]) / sum(efda_sum$forest_kha[ix]))
gru45 <- gru_c[gru_c$scen == "RCP4.5", ]
U50_by_country <- tapply(gru45$U_50, gru45$country, mean)
.cb <- aggregate(forest_kha ~ country_root + biome, efda_sum, sum, na.rm = TRUE)
.cb <- .cb[order(.cb$country_root, -.cb$forest_kha), ]
dom_biome <- setNames(.cb$biome[!duplicated(.cb$country_root)],
                      .cb$country_root[!duplicated(.cb$country_root)])

# per-country buffer for a given (forest_type, H, s_factor); paired seed per country
country_buffers <- function(ft, H, s_factor) {
  res <- lapply(files, function(f) {
    ann  <- as.data.frame(readRDS(f))
    fkey <- unique(ann$country)
    cn   <- file2country[[fkey]]
    if (is.null(cn) || is.na(cn)) return(NULL)
    bm <- dom_biome[[cn]]; cc <- c_by_biome[[bm]]
    if (is.null(bm) || is.na(bm) || is.null(cc) || is.na(cc)) return(NULL)
    sev <- sev_by_country[[cn]]; U50 <- U50_by_country[[cn]]
    if (is.null(sev) || is.na(sev) || is.null(U50) || is.na(U50)) return(NULL)
    series <- ann$lambda_natural[ann$year >= 1986]
    seed_i <- SEED_BASE + sum(utf8ToInt(cn))      # deterministic per-country seed
    b <- bootstrap_buffer(series, sev, U50, R_of(bm, ft), cc, H, s_factor, seed = seed_i)
    data.frame(country = cn, b = b, stringsAsFactors = FALSE)
  })
  do.call(rbind, res)
}

# biome-level buffer = forest-area-weighted mean of per-country buffers
# (mirrors engine/R/04_headline.R biome_buffer via the zone merge)
zone_tpl <- efda_sum[, c("country_root", "biome", "forest_kha")]
biome_buffer <- function(biome, ft, H, s_factor) {
  cb <- country_buffers(ft, H, s_factor)
  z  <- merge(zone_tpl, cb, by.x = "country_root", by.y = "country")
  z  <- z[z$biome == biome, ]
  if (nrow(z) == 0) stop("no zones for biome ", biome)
  sum(z$forest_kha * z$b) / sum(z$forest_kha)
}

# --- assemble per anchor practice --------------------------------------------
practices <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
headline  <- read.csv("engine/output/clean_headline.csv", stringsAsFactors = FALSE)
anchors   <- practices[as.logical(practices$is_anchor), ]

cat(sprintf("[stock-sens] %d anchor practices; n_mc=%d; beta=%.2f\n",
            nrow(anchors), N_MC, BETA))

rows <- lapply(seq_len(nrow(anchors)), function(i) {
  r  <- anchors[i, ]
  protected <- isTRUE(as.logical(r$legally_protected))
  H  <- if (protected) k("H_perm") else 40
  sref <- S_ref[[r$biome]]; if (is.null(sref) || is.na(sref)) stop("no S_ref for ", r$biome)
  s_factor <- (r$S_bar / sref)^BETA

  b_base  <- biome_buffer(r$biome, r$forest_type, H, 1)
  b_stock <- biome_buffer(r$biome, r$forest_type, H, s_factor)
  m       <- b_stock / b_base                       # paired multiplicative effect

  h <- headline[headline$practice == r$practice & headline$biome == r$biome &
                headline$species == r$species, ]
  if (nrow(h) != 1) { cat("  ! no headline match:", r$practice, r$biome, "\n"); return(NULL) }
  L <- h$L; T <- h$T; b_eng <- h$b
  b_stock_applied <- min(max(b_eng * m, 0), 1)
  net_base  <- (1 - L) * (1 - T) * (1 - b_eng)
  net_stock <- (1 - L) * (1 - T) * (1 - b_stock_applied)

  data.frame(practice = r$practice, biome = r$biome, forest_type = r$forest_type,
             S_bar = r$S_bar, S_ref = round(sref, 0), S_ratio = round(r$S_bar / sref, 3),
             s_factor = round(s_factor, 3),
             b_engine = round(b_eng, 4), b_recomp_base = round(b_base, 4),
             b_stock = round(b_stock_applied, 4), mult_effect = round(m, 3),
             L = round(L, 4), T = round(T, 4),
             net_base = round(net_base, 4), net_stock = round(net_stock, 4),
             d_net_pp = round(100 * (net_stock - net_base), 2),
             stringsAsFactors = FALSE)
})
tab <- do.call(rbind, rows)
tab <- tab[order(tab$d_net_pp), ]
write.csv(tab, file.path(OUT, "stock_sensitivity_practice.csv"), row.names = FALSE)

# --- summary -----------------------------------------------------------------
val_err <- 100 * abs(tab$b_recomp_base - tab$b_engine) / pmax(tab$b_engine, 1e-6)
spr <- suppressWarnings(cor(tab$net_base, tab$net_stock, method = "spearman"))
cat("\n--- per-practice results (sorted by NCV change) ---\n")
print(tab[, c("practice","biome","S_ratio","s_factor","b_engine","b_stock",
              "net_base","net_stock","d_net_pp")], row.names = FALSE)
cat(sprintf("\nVALIDATION  b_recomp_base vs engine b: median abs err %.1f%% (max %.1f%%)\n",
            median(val_err), max(val_err)))
cat(sprintf("HEADLINE    median NCV  base=%.1f%%  ->  stock=%.1f%%  (delta %.1f pp)\n",
            100*median(tab$net_base), 100*median(tab$net_stock),
            100*(median(tab$net_stock) - median(tab$net_base))))
cat(sprintf("RANKING     Spearman(net_base, net_stock) = %.4f\n", spr))
cat(sprintf("BUFFER      mult_effect range %.2f - %.2f  (1 = no change)\n",
            min(tab$mult_effect), max(tab$mult_effect)))
cat(sprintf("\nWrote %s\n", file.path(OUT, "stock_sensitivity_practice.csv")))
cat("=== DONE (delete the sensitivity_stock_density/ folder to remove) ===\n")
