# =============================================================================
# sens_within_country_correlation.R  —  estimate c, and test it for drift
# =============================================================================
# Standalone SI analysis (not part of run_engine.R). Run from analysis/:
#   Rscript engine/R/sens_within_country_correlation.R
#
# Scope. The engine's c is a WITHIN-country parameter: 03_buffer sets K = round(1/c)
# effective decorrelated cells inside one country's bootstrap, so c sets the country-level
# buffer rate and therefore the headline NCV. This script estimates c at that scale and tests
# it for drift. The companion sens_correlation_drift.R measures BETWEEN-country co-movement,
# which drives the pooling claim (Fig 4b, ED Fig 4) but is a different quantity and is not
# comparable to engine/params/biome_correlation.csv.
#
# Data. Per-hexagon annual natural-disturbance rates on the JRC 35 km grid, extracted from the
# EFDA 30 m rasters by scripts/efda_offline/extract_hexagon_series.R; the committed country
# files are aggregates and cannot support this. Hexagons are the grain the JRC risk model uses,
# so the estimate is comparable to that product. Runs on a partial extraction and reports the
# coverage.
#
# Design. Per country, mean pairwise correlation across its hexagons, on two bases: raw levels,
# and after removing each hexagon's own linear trend. Levels conflate a shared trend with the
# synchronised shocks a bad pool year consists of. Drift is a paired comparison across
# countries, each contributing one early and one late value, which keeps within- and
# between-country variation separate. Inference is by permutation of the year labels; the
# pooled null applies the same year permutation to every country, preserving the cross-country
# dependence that would otherwise make the pooled test anticonservative. Two year windows: the
# full record, and excluding 2017-2023, which are author-constructed in EFDA and fall inside
# the late window.
#
# Interpretation of the benchmark. c is not the pairwise correlation of the engine's own cells:
# 03_buffer draws them conditionally independently given the country-year mean, so that process
# at c = 0.15-0.25 yields cells correlated about 0.05, and about 0 with the country mean held
# flat. The construction is a correlation-limited equivalent pool — K = round(1/c) independent
# cells standing in for a large correlated pool — which holds because the effective number of
# independent units in a pool of N units with mean pairwise correlation rho tends to 1/rho.
# The comparable quantity is therefore the observed mean pairwise correlation among
# sub-national units. A second estimator (realised variance reduction) is reported alongside,
# because that identity assumes equal variances: true of the engine's cells, not of real
# hexagons.
#
# Unit. Hexagons are grouped by whole country, matching 03_buffer's bootstrap unit: it globs
# efda_country_rates/ and drops the *_Temperate / *_Mediterranean split files, so France and
# Italy enter as single countries there too (31 files). No bioregion split is applied here.
#
# Emits: engine/output/sens_within_country_c.csv        (per-country estimates + drift)
#        engine/output/sens_within_country_c_biome.csv  (pooled vs the assumed c)
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
cat("[sens_within_country_c] estimating within-country spatial correlation\n")

MIN_HEX      <- 8L      # fewer hexagons than this gives an unstable pairwise mean
MIN_NONZERO  <- 8L      # hexagon must have >= this many non-zero years to correlate
N_PERM       <- 2000L
PERM_SEED    <- 909L
CONSTRUCTED  <- 2017:2023   # author-constructed EFDA years (see docs/efda_extraction.md)

.need <- function(p) { if (!file.exists(p)) stop("MISSING input: ", p); p }
hexf <- "data/processed/efda_hex_rates.rds"
if (!file.exists(hexf))
  stop("missing ", hexf, "\n  run: cd ~/efda_scratch && ./process_hex_all.sh && Rscript combine_hex_rates.R")
H <- as.data.frame(readRDS(hexf))
if (anyNA(H$lambda_natural)) stop("NA lambda_natural in the hexagon series")
cat(sprintf("[sens_within_country_c] %d countries, %d hexagons, %d-%d\n",
            length(unique(H$country)), length(unique(H$hex_id)),
            min(H$year), max(H$year)))

# --- estimators ------------------------------------------------------------------
mean_pair_cor <- function(M) {
  sdev <- apply(M, 2, sd)
  M <- M[, sdev > 0, drop = FALSE]          # a hexagon with no variance carries no information
  if (ncol(M) < 3L) return(NA_real_)
  R <- cor(M)
  mean(R[upper.tri(R)])
}
detrend <- function(M) {
  tt <- seq_len(nrow(M))
  apply(M, 2, function(v) residuals(lm(v ~ tt)))
}
# Second, independent estimator of the effective pool size: the realised variance
# reduction from pooling. 03_buffer represents a large correlated pool by K = round(1/c)
# INDEPENDENT cells, which is valid only if 1/c is the effective number of independent
# units. For N units of equal variance and mean pairwise correlation rho, Var(mean) =
# sigma^2[1+(N-1)rho]/N, so Var_single/Var_pooled -> 1/rho. Reported alongside 1/rho
# because real hexagons are heteroskedastic while the engine's cells are not: the
# variance ratio weights high-variance hexagons more, so the two bracket the answer
# rather than coincide. Bounded above by n_hex, so it is only informative when
# n_hex >> 1/rho.
n_eff_var_ratio <- function(M) {
  sdev <- apply(M, 2, sd); M <- M[, sdev > 0, drop = FALSE]
  if (ncol(M) < 3L) return(NA_real_)
  vp <- var(rowMeans(M))
  if (!is.finite(vp) || vp <= 0) return(NA_real_)
  mean(apply(M, 2, var)) / vp
}

# --- per-country hexagon x year matrices ----------------------------------------
build_mats <- function(years) {
  mats <- list(); dropped <- list()
  for (cn in sort(unique(H$country))) {
    d <- H[H$country == cn & H$year %in% years, ]
    M <- tapply(d$lambda_natural, list(as.character(d$year), as.character(d$hex_id)), identity)
    if (anyNA(M)) stop("incomplete hexagon panel for ", cn)
    # A hexagon that is almost always zero cannot be correlated meaningfully.
    ok <- colSums(M > 0) >= MIN_NONZERO
    dropped[[cn]] <- sum(!ok)
    M <- M[, ok, drop = FALSE]
    if (ncol(M) >= MIN_HEX) mats[[cn]] <- list(M = M, dropped = dropped[[cn]])
  }
  mats
}

# --- one year window: per-country estimates + permutation nulls ------------------
run_window <- function(years, label) {
  mats <- build_mats(years)
  cat(sprintf("[sens_within_country_c] %-14s usable countries (>=%d active hexagons): %d of %d\n",
              label, MIN_HEX, length(mats), length(unique(H$country))))
  if (!length(mats)) stop("no country has enough active hexagons in window ", label)
  nyr <- unique(vapply(mats, function(z) nrow(z$M), integer(1)))
  if (length(nyr) != 1L)
    stop("countries have different year counts in window ", label,
         " — the pooled null requires a common year grid")

  rows <- list(); nulls <- list()
  for (cn in names(mats)) {
    M <- mats[[cn]]$M
    yy <- as.integer(rownames(M)); mid <- floor(median(yy)); ne <- sum(yy <= mid)
    # Detrend ONCE on the observed series. Detrending inside the permutation loop would fit
    # a trend to randomly reordered years, which is meaningless. Permuting the residuals'
    # year labels is the correct null.
    bases <- list(levels = M, detrended = detrend(M))
    # Same seed for every country => the SAME year permutation is applied throughout, so
    # pooling across countries below preserves their shared-year dependence.
    set.seed(PERM_SEED)
    perm_idx <- lapply(seq_len(N_PERM), function(i) sample(nrow(M)))
    for (b in c("levels", "detrended")) {
      B <- bases[[b]]
      full <- mean_pair_cor(B)
      oe <- mean_pair_cor(B[seq_len(ne), , drop = FALSE])
      ol <- mean_pair_cor(B[(ne + 1L):nrow(B), , drop = FALSE])
      pn <- vapply(perm_idx, function(idx)
        mean_pair_cor(B[idx[(ne + 1L):nrow(B)], , drop = FALSE]) -
          mean_pair_cor(B[idx[seq_len(ne)], , drop = FALSE]), numeric(1))
      nulls[[paste(b, cn)]] <- pn
      rows[[length(rows) + 1L]] <- data.frame(
        year_window = label, country = cn, basis = b,
        n_hex = ncol(M), n_hex_dropped = mats[[cn]]$dropped,
        n_years_early = ne, n_years_late = nrow(M) - ne,
        c_full = full, c_early = oe, c_late = ol, difference = ol - oe,
        n_eff_inv_rho = if (is.na(full) || full <= 0) NA_real_ else 1 / full,
        n_eff_var_ratio = n_eff_var_ratio(B),
        mean_hazard = mean(M),
        p_permutation = if (is.na(ol - oe) || !length(pn[!is.na(pn)])) NA_real_ else
          mean(abs(pn[!is.na(pn)]) >= abs(ol - oe)),
        # A country can span several JRC bioregions (France, Italy, Spain), so report the
        # modal one AND how many it spans. Taking the first match would silently label a
        # multi-bioregion country by whichever hexagon happened to sort first.
        bioregion_modal = names(sort(table(H$bioregion[H$country == cn]),
                                     decreasing = TRUE))[1],
        n_bioregions = length(unique(H$bioregion[H$country == cn])),
        stringsAsFactors = FALSE)
    }
  }
  list(per_country = do.call(rbind, rows), nulls = nulls)
}

FULL_YEARS <- sort(unique(H$year))
res <- list(run_window(FULL_YEARS, "full"),
            run_window(setdiff(FULL_YEARS, CONSTRUCTED), "excl_2017_2023"))
per_country <- do.call(rbind, lapply(res, `[[`, "per_country"))
rownames(per_country) <- NULL

# --- map each country to the biome the engine assigns it -------------------------
# Dominant biome by forest area, mirroring 03_buffer.R, so the comparison uses the same
# assignment as the buffer does.
esum <- as.data.frame(readRDS(.need("data/processed/efda_country_summary.rds")))
cb <- aggregate(forest_kha ~ country_root + biome, esum, sum)
cb <- cb[order(cb$country_root, -cb$forest_kha), ]
dom <- setNames(cb$biome[!duplicated(cb$country_root)], cb$country_root[!duplicated(cb$country_root)])
bmap <- read.csv(.need("data/country_biome_map.csv"), stringsAsFactors = FALSE)
f2c <- setNames(bmap$country, bmap$efda_filename)
per_country$engine_biome <- unname(dom[f2c[per_country$country]])
if (anyNA(per_country$engine_biome))
  stop("no engine biome for: ",
       paste(unique(per_country$country[is.na(per_country$engine_biome)]), collapse = ", "))
write.csv(per_country, "engine/output/sens_within_country_c.csv", row.names = FALSE)

# --- pooled: observed c vs assumed, and a paired drift test ----------------------
cpar <- read.csv(.need("engine/params/biome_correlation.csv"), stringsAsFactors = FALSE)
# Not every biome in biome_correlation.csv reaches the published results: the 4-biome
# variant Temperate_UK (Ireland, United Kingdom) has no rows in mc_summary, so its c
# cannot move a published NCV. Flag this per biome so a discrepancy there is not read as
# affecting the headline.
published_biomes <- unique(read.csv(.need("engine/output/mc_summary.csv"),
                                   stringsAsFactors = FALSE)$biome)

pooled_p <- function(nl, countries, basis, obs) {
  # Average the SAME permutation replicate across countries -> null for the mean difference.
  keys <- paste(basis, countries)
  if (!all(keys %in% names(nl)) || is.na(obs)) return(NA_real_)
  m <- do.call(rbind, nl[keys])
  d <- colMeans(m, na.rm = TRUE)
  d <- d[!is.na(d)]
  if (!length(d)) return(NA_real_)
  mean(abs(d) >= abs(obs))
}

biome_tbl <- do.call(rbind, lapply(seq_along(res), function(w) {
  label <- if (w == 1L) "full" else "excl_2017_2023"
  nl <- res[[w]]$nulls
  do.call(rbind, lapply(c("levels", "detrended"), function(b) {
    d0 <- per_country[per_country$year_window == label & per_country$basis == b &
                        !is.na(per_country$c_full), ]
    grp <- c(sort(unique(d0$engine_biome)), "ALL")
    do.call(rbind, lapply(grp, function(bm) {
      x <- if (bm == "ALL") d0 else d0[d0$engine_biome == bm, ]
      assumed <- if (bm == "ALL") NA_real_ else cpar$c[cpar$biome == bm]
      xd <- x[!is.na(x$difference), ]
      md <- if (nrow(xd)) mean(xd$difference) else NA_real_
      nr <- if (nrow(xd)) sum(xd$difference > 0) else NA_integer_
      data.frame(
        year_window = label, biome = bm, basis = b, n_countries = nrow(x),
        in_published_mc = if (bm == "ALL") NA else bm %in% published_biomes,
        c_observed_mean = mean(x$c_full),
        c_observed_min = min(x$c_full), c_observed_max = max(x$c_full),
        c_assumed = if (length(assumed) == 1) assumed else NA_real_,
        n_eff_observed = round(1 / max(mean(x$c_full), 1e-6)),
        # Second estimator, median across countries (see n_eff_var_ratio()).
        n_eff_var_ratio_median = median(x$n_eff_var_ratio, na.rm = TRUE),
        n_eff_assumed = if (length(assumed) == 1 && !is.na(assumed)) round(1 / assumed) else NA_real_,
        mean_late_minus_early = md,
        n_countries_rising = nr, n_countries_paired = nrow(xd),
        p_pooled_permutation = pooled_p(nl, xd$country, b, md),
        p_sign_test = if (nrow(xd) >= 2)
          binom.test(nr, nrow(xd), 0.5)$p.value else NA_real_,
        stringsAsFactors = FALSE)
    }))
  }))
}))
rownames(biome_tbl) <- NULL
write.csv(biome_tbl, "engine/output/sens_within_country_c_biome.csv", row.names = FALSE)

# --- report --------------------------------------------------------------------
cat("\n--- per-country within-country correlation (detrended, full record) ---\n")
pc <- per_country[per_country$basis == "detrended" & per_country$year_window == "full" &
                    !is.na(per_country$c_full), ]
for (i in order(-pc$c_full)) with(pc[i, ],
  cat(sprintf("  %-14s %-14s n_hex=%3d  c=%.3f  N_eff %4.1f (1/rho) / %4.1f (var-ratio)  (early %.3f -> late %.3f, diff %+.3f, p=%.3f)\n",
              country, engine_biome, n_hex, c_full, n_eff_inv_rho, n_eff_var_ratio,
              c_early, c_late, difference, p_permutation)))

cat("\n--- observed c vs assumed c (SAME scale: within-country) ---\n")
for (i in seq_len(nrow(biome_tbl))) with(biome_tbl[i, ], {
  cat(sprintf("  %-14s %-13s%-2s %-9s n=%2d  observed %.3f [%.3f-%.3f] N_eff %2d",
              year_window, biome,
              if (!is.na(in_published_mc) && !in_published_mc) " *" else "",
              basis, n_countries, c_observed_mean,
              c_observed_min, c_observed_max, n_eff_observed))
  if (is.na(c_assumed)) cat("  |  assumed    n/a        ") else
    cat(sprintf("  |  assumed %.2f N_eff %2d", c_assumed, n_eff_assumed))
  cat(sprintf("  |  late-early %+.3f  %d/%d rising  p_pool=%.3f  p_sign=%.3f\n",
              mean_late_minus_early, n_countries_rising, n_countries_paired,
              p_pooled_permutation, p_sign_test))
})
# --- is the estimate an artefact of the activity filter? -------------------------
# MIN_NONZERO keeps the more disturbance-active hexagons, which could inflate rho. Sweep it
# and report, so the reader can see the estimate is not manufactured by the threshold.
cat(sprintf("\n--- sensitivity to MIN_NONZERO (detrended; baseline %d) ---\n", MIN_NONZERO))
for (k in c(2L, 4L, 8L, 12L, 16L, 20L)) {
  rho <- c(); dfull <- c(); dexcl <- c()
  for (cn in sort(unique(H$country))) {
    d <- H[H$country == cn, ]
    M0 <- tapply(d$lambda_natural, list(as.character(d$year), as.character(d$hex_id)), identity)
    ok <- colSums(M0 > 0) >= k
    if (sum(ok) < MIN_HEX) next
    yy0 <- as.integer(rownames(M0))
    for (win in c("full", "excl")) {
      ky <- if (win == "full") rep(TRUE, length(yy0)) else !(yy0 %in% CONSTRUCTED)
      B <- detrend(M0[ky, ok, drop = FALSE])
      y2 <- yy0[ky]; ne <- sum(y2 <= floor(median(y2)))
      dd <- mean_pair_cor(B[(ne + 1L):nrow(B), , drop = FALSE]) -
        mean_pair_cor(B[seq_len(ne), , drop = FALSE])
      if (win == "full") { rho <- c(rho, mean_pair_cor(B)); dfull <- c(dfull, dd) }
      else dexcl <- c(dexcl, dd)
    }
  }
  cat(sprintf("  MIN_NONZERO=%2d  n=%2d  mean rho %.3f  median %.3f  drift %+.3f (full) %+.3f (excl)\n",
              k, length(rho), mean(rho, na.rm = TRUE), median(rho, na.rm = TRUE),
              mean(dfull, na.rm = TRUE), mean(dexcl, na.rm = TRUE)))
}

# --- does correlation rise with hazard? (the mechanism the objection needs) ------
# If a warmer, more-disturbed forest is also a more SYNCHRONISED one, then applying an RCP
# uplift to the mean rate while holding c fixed would understate end-century pool risk.
# Tested across countries, at the within-country scale. sens_correlation_drift.R runs the
# same check between countries (R^2 0.15 in levels, 0.01 detrended).
cat("\n--- correlation vs hazard, across countries (within-country scale) ---\n")
for (b in c("levels", "detrended")) for (w in c("full", "excl_2017_2023")) {
  z <- per_country[per_country$basis == b & per_country$year_window == w &
                     !is.na(per_country$c_full) & !is.na(per_country$mean_hazard), ]
  if (nrow(z) < 4L) next
  cat(sprintf("  %-9s %-14s n=%2d  Pearson %+.3f  Spearman %+.3f  (R^2 %.3f)\n",
              b, w, nrow(z), cor(z$mean_hazard, z$c_full),
              cor(z$mean_hazard, z$c_full, method = "spearman"),
              cor(z$mean_hazard, z$c_full)^2))
}

cat("\n  * biome has no rows in mc_summary, so its c cannot move a published NCV\n")
cat("\n[sens_within_country_c] wrote 2 tables to engine/output/\n")
