# =============================================================================
# sens_correlation_drift.R  —  is cross-country disturbance correlation drifting?
# =============================================================================
# Standalone SI analysis (not part of run_engine.R). Run from analysis/:
#   Rscript engine/R/sens_correlation_drift.R
#
# WHY. The buffer treats spatial correlation c as fixed per biome (engine/params/
# biome_correlation.csv, sourced from Anderegg 2020 rather than fitted here), while only
# the mean hazard rate gets an RCP uplift. Migliavacca's review point: the mechanism most
# likely to raise c under climate change — synchronised continental droughts driving
# simultaneous outbreaks — is exactly why correlation matters, so a fixed c would overstate
# end-of-century diversification and make the Fig 4 country rates a lower bound rather than
# a central estimate. c enters the pool only through N_eff = round(1/c), so a drift in c is
# a drift in how much the pool can diversify.
#
# SCALE — read this before comparing anything here to c. The engine's c is a WITHIN-country
# parameter: 03_buffer sets K = round(1/c) effective decorrelated CELLS inside one country's
# pool. CROSS-country co-movement is not parameterised by c at all — 12_pool_buildup induces
# it empirically by drawing SHARED resampled years, so all countries see the same year. What
# this script measures is that cross-country co-movement. It is therefore the right quantity
# for the pooling/diversification claim (Fig 4b, ED Fig 4), but it is NOT an estimate of c
# and must not be substituted for one. For c itself, see the companion script
# sens_within_country_correlation.R, which estimates the within-country correlation from
# per-hexagon EFDA series — that one IS comparable to biome_correlation.csv, and it is the
# script that answers Migliavacca's objection at the scale his objection names.
#
# WHAT THIS DOES. Estimates the observed cross-country correlation of annual natural
# disturbance rates from the EFDA record and tests whether it has drifted, three ways:
#   (1) early vs late half, non-overlapping, with a year-block bootstrap CI on the
#       difference — this is the inferential test;
#   (2) a moving-window series, descriptive only (overlapping windows share years, so the
#       slope's nominal p-value would be anticonservative and is deliberately not reported);
#   (3) correlation against contemporaneous hazard intensity — Migliavacca's mechanism,
#       i.e. does co-movement rise in high-hazard periods rather than with calendar time.
#
# Reported on two bases, because the choice matters and should not be silent:
#   levels     — correlation of annual rates. Includes any common trend, so a shared
#                upward trend alone can produce positive correlation.
#   detrended  — each country's own linear time trend removed first, isolating
#                synchronised SHOCKS, which is what a bad pool year actually is.
#
# ROBUSTNESS. 2017-2023 rates are author-constructed (see the manuscript's disturbance-data
# limitation), so every statistic is recomputed on 1986-2016 alone. If a drift appears only
# with the constructed years it is an artefact of them, and the script says so.
#
# Emits: engine/output/sens_correlation_drift_summary.csv   (headline statistics)
#        engine/output/sens_correlation_drift_windows.csv   (moving-window series)
#        engine/output/sens_correlation_drift_biome.csv     (observed vs assumed c)
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
cat("[sens_correlation_drift] cross-country disturbance correlation over time\n")

WINDOW      <- 15L     # moving-window width in years (descriptive series only)
N_BOOT      <- 2000L   # year-block bootstrap replicates for the early/late difference
BOOT_SEED   <- 4242L
CONSTRUCTED <- 2017L   # first author-constructed year; robustness cut is < this

.need <- function(p) { if (!file.exists(p)) stop("MISSING input: ", p); p }

# --- country x year matrix of annual natural-disturbance rates -------------------
# Same file filter as 03_buffer.R: whole-country files only, France/Italy sub-zone
# splits excluded so no country is represented twice.
efda_dir <- .need("data/processed/efda_country_rates")
files <- list.files(efda_dir, "\\.rds$", full.names = TRUE)
files <- files[!grepl("_(Temperate|Mediterranean)\\.rds$", files)]
if (!length(files)) stop("no EFDA country files found in ", efda_dir)

series <- lapply(files, function(f) {
  d <- as.data.frame(readRDS(f))
  stopifnot(all(c("country", "year", "lambda_natural") %in% names(d)))
  if (length(unique(d$country)) != 1) stop("multiple countries in ", basename(f))
  d[, c("country", "year", "lambda_natural")]
})
long <- do.call(rbind, series)
if (anyNA(long$lambda_natural)) stop("NA lambda_natural in the EFDA series")

years     <- sort(unique(long$year))
countries <- sort(unique(long$country))
X <- matrix(NA_real_, length(years), length(countries),
            dimnames = list(as.character(years), countries))
X[cbind(as.character(long$year), long$country)] <- long$lambda_natural
# A hole here would silently drop pairs from the correlation, so require completeness.
if (anyNA(X)) stop("incomplete country x year panel: ", sum(is.na(X)), " missing cells")
cat(sprintf("[sens_correlation_drift] panel %d years x %d countries (%d-%d)\n",
            nrow(X), ncol(X), min(years), max(years)))

# --- estimators -----------------------------------------------------------------
# Mean pairwise Pearson correlation over the upper triangle. Columns with no variance
# would give an undefined correlation, so they are an error rather than an NA to skip.
mean_pair_cor <- function(M) {
  if (nrow(M) < 3L) stop("need >= 3 years to correlate; got ", nrow(M))
  sdev <- apply(M, 2, sd)
  if (any(sdev <= 0)) stop("zero-variance country in window: ",
                           paste(colnames(M)[sdev <= 0], collapse = ", "))
  R <- cor(M)
  mean(R[upper.tri(R)])
}
# Remove each country's own linear time trend, leaving synchronised shocks.
detrend <- function(M) {
  tt <- seq_len(nrow(M))
  apply(M, 2, function(v) residuals(lm(v ~ tt)))
}
# Both bases for one block of years.
both_bases <- function(M) c(levels = mean_pair_cor(M), detrended = mean_pair_cor(detrend(M)))

# --- (1) early vs late half, with a year-block bootstrap ------------------------
# Non-overlapping halves, so the comparison is clean. The bootstrap resamples YEARS within
# each half (that is the sampling variability of interest: which years we happened to
# observe), not countries.
split_test <- function(M, label) {
  yy <- as.integer(rownames(M))
  mid <- floor(median(yy))
  e <- M[yy <= mid, , drop = FALSE]; l <- M[yy > mid, , drop = FALSE]
  obs_e <- both_bases(e); obs_l <- both_bases(l)
  set.seed(BOOT_SEED)
  bd <- vapply(seq_len(N_BOOT), function(i) {
    be <- e[sample(nrow(e), nrow(e), replace = TRUE), , drop = FALSE]
    bl <- l[sample(nrow(l), nrow(l), replace = TRUE), , drop = FALSE]
    tryCatch(both_bases(bl) - both_bases(be), error = function(err) c(NA_real_, NA_real_))
  }, numeric(2))
  keep <- !is.na(bd[1, ]) & !is.na(bd[2, ])
  if (sum(keep) < N_BOOT * 0.9)
    stop("bootstrap: only ", sum(keep), " of ", N_BOOT, " replicates usable")
  # Permutation test: reassign year labels between the two periods and recompute the
  # difference. This is the proper null for "does period membership matter", and unlike the
  # bootstrap CI it needs no assumption about the estimator's centring. Years are mildly
  # autocorrelated (AR(1) rho ~ 0.3), which makes permutation slightly ANTIconservative --
  # so a null result here is a robust null.
  perm <- vapply(seq_len(N_BOOT), function(i) {
    idx <- sample(nrow(M))
    pe <- M[idx[seq_len(nrow(e))], , drop = FALSE]
    pl <- M[idx[(nrow(e) + 1L):nrow(M)], , drop = FALSE]
    tryCatch(both_bases(pl) - both_bases(pe), error = function(err) c(NA_real_, NA_real_))
  }, numeric(2))
  do.call(rbind, lapply(c("levels", "detrended"), function(b) {
    d <- bd[b, keep]
    pnull <- perm[b, !is.na(perm[b, ])]
    obs_d <- obs_l[[b]] - obs_e[[b]]
    data.frame(subset = label, basis = b,
               early_period = paste0(min(yy), "-", mid),
               late_period  = paste0(mid + 1, "-", max(yy)),
               cor_early = obs_e[[b]], cor_late = obs_l[[b]],
               difference = obs_l[[b]] - obs_e[[b]],
               ci_lo = unname(quantile(d, 0.025)), ci_hi = unname(quantile(d, 0.975)),
               # share of replicates with the opposite sign = two-sided bootstrap p
               p_bootstrap = 2 * min(mean(d <= 0), mean(d >= 0)),
               p_permutation = mean(abs(pnull) >= abs(obs_d)),
               n_boot_used = sum(keep), stringsAsFactors = FALSE)
  }))
}

# --- (2) moving-window series (descriptive) + (3) correlation vs hazard ---------
windows_of <- function(M, label) {
  yy <- as.integer(rownames(M))
  starts <- seq_len(nrow(M) - WINDOW + 1L)
  do.call(rbind, lapply(starts, function(s) {
    w <- M[s:(s + WINDOW - 1L), , drop = FALSE]
    bb <- both_bases(w)
    data.frame(subset = label, mid_year = mean(yy[s:(s + WINDOW - 1L)]),
               cor_levels = bb[["levels"]], cor_detrended = bb[["detrended"]],
               mean_hazard = mean(w), stringsAsFactors = FALSE)
  }))
}
# Slope per decade, on both time and hazard. No p-values: the windows overlap.
slopes_of <- function(W, label) {
  do.call(rbind, lapply(c("cor_levels", "cor_detrended"), function(b) {
    v <- W[[b]]
    data.frame(subset = label, basis = sub("^cor_", "", b),
               slope_per_decade_time = unname(coef(lm(v ~ W$mid_year))[2]) * 10,
               slope_per_hazard_pp   = unname(coef(lm(v ~ W$mean_hazard))[2]) / 100,
               r2_vs_hazard = summary(lm(v ~ W$mean_hazard))$r.squared,
               n_windows = nrow(W), stringsAsFactors = FALSE)
  }))
}

# --- run on the full record and on the pre-constructed segment ------------------
subsets <- list(full = X, `pre-2017` = X[as.integer(rownames(X)) < CONSTRUCTED, , drop = FALSE])
split_tbl  <- do.call(rbind, lapply(names(subsets), function(n) split_test(subsets[[n]], n)))
window_tbl <- do.call(rbind, lapply(names(subsets), function(n) windows_of(subsets[[n]], n)))
slope_tbl  <- do.call(rbind, lapply(names(subsets),
                                    function(n) slopes_of(window_tbl[window_tbl$subset == n, ], n)))

summary_tbl <- merge(split_tbl, slope_tbl, by = c("subset", "basis"))
rownames(summary_tbl) <- NULL
write.csv(summary_tbl, "engine/output/sens_correlation_drift_summary.csv", row.names = FALSE)
write.csv(window_tbl,  "engine/output/sens_correlation_drift_windows.csv", row.names = FALSE)

# --- cross-country correlation within each biome -------------------------------
# NOT an estimate of c (see SCALE above): c is within-country, this is between-country.
# Reported per biome because that is the grain at which the pool diversifies, with the
# assumed c alongside purely for order-of-magnitude context. Country -> dominant biome by
# forest area, mirroring 03_buffer.R.
esum <- as.data.frame(readRDS(.need("data/processed/efda_country_summary.rds")))
if (anyNA(esum$forest_kha)) stop("efda_country_summary: NA forest_kha")
cb <- aggregate(forest_kha ~ country_root + biome, esum, sum)
cb <- cb[order(cb$country_root, -cb$forest_kha), ]
dom <- setNames(cb$biome[!duplicated(cb$country_root)], cb$country_root[!duplicated(cb$country_root)])
bmap <- read.csv(.need("data/country_biome_map.csv"), stringsAsFactors = FALSE)
file2country <- setNames(bmap$country, bmap$efda_filename)
# efda_filename is the lowercase key that matches the rds `country` field (as in 03_buffer),
# not a filename with an extension.
col_country <- unname(file2country[colnames(X)])
if (all(is.na(col_country)))
  stop("country_biome_map.csv: efda_filename matched no panel column")
cpar <- read.csv(.need("engine/params/biome_correlation.csv"), stringsAsFactors = FALSE)

biome_rows <- lapply(unique(na.omit(dom[col_country])), function(bm) {
  cols <- which(!is.na(col_country) & dom[col_country] == bm)
  if (length(cols) < 3L) return(NULL)          # <3 countries: no meaningful pairwise mean
  Mb <- X[, cols, drop = FALSE]
  bb <- both_bases(Mb)
  assumed <- cpar$c[cpar$biome == bm]
  data.frame(biome = bm, n_countries = length(cols),
             xcountry_cor_levels = bb[["levels"]],
             xcountry_cor_detrended = bb[["detrended"]],
             c_withincountry_assumed = if (length(assumed) == 1) assumed else NA_real_,
             scale_note = "xcountry_* is between-country; c is within-country - not comparable",
             stringsAsFactors = FALSE)
})
biome_tbl <- do.call(rbind, biome_rows)
if (is.null(biome_tbl)) stop("no biome had >= 3 countries; cannot compare against c")
rownames(biome_tbl) <- NULL
write.csv(biome_tbl, "engine/output/sens_correlation_drift_biome.csv", row.names = FALSE)

# --- report --------------------------------------------------------------------
cat("\n--- early vs late half (bootstrap CI on the difference) ---\n")
for (i in seq_len(nrow(summary_tbl))) with(summary_tbl[i, ],
  cat(sprintf("  %-8s %-9s %s %.3f -> %s %.3f | diff %+.3f [%+.3f, %+.3f]  p_perm=%.3f p_boot=%.3f%s\n",
              subset, basis, early_period, cor_early, late_period, cor_late,
              difference, ci_lo, ci_hi, p_permutation, p_bootstrap,
              if (p_permutation < 0.05) "  *" else "")))
cat("\n--- moving-window slopes (descriptive; windows overlap, so no p-values) ---\n")
for (i in seq_len(nrow(slope_tbl))) with(slope_tbl[i, ],
  cat(sprintf("  %-8s %-9s %+.4f /decade | %+.4f per pp hazard (R2=%.2f, %d windows)\n",
              subset, basis, slope_per_decade_time, slope_per_hazard_pp,
              r2_vs_hazard, n_windows)))
cat("\n--- BETWEEN-country correlation per biome (not c; c is within-country) ---\n")
for (i in seq_len(nrow(biome_tbl))) with(biome_tbl[i, ],
  cat(sprintf("  %-14s n=%2d  between-country: detrended %.3f, levels %.3f   (within-country c assumed %.2f, different scale)\n",
              biome, n_countries, xcountry_cor_detrended, xcountry_cor_levels,
              c_withincountry_assumed)))
cat("\n[sens_correlation_drift] wrote 3 tables to engine/output/\n")
