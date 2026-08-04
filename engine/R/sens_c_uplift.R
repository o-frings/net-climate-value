# =============================================================================
# sens_c_uplift.R  —  how much does the headline move if c is wrong?
# =============================================================================
# Standalone SI sensitivity (not part of run_engine.R). Run from analysis/, after
# sens_within_country_correlation.R and sens_c_trend.R:
#   Rscript engine/R/sens_c_uplift.R
#
# Purpose. A bounded sensitivity on the within-country spatial correlation c, which is held
# fixed per biome while only the mean hazard gets an RCP uplift. Because
# sens_within_country_correlation.R estimates c from the per-hexagon EFDA record, the sweep is
# bounded by a measured range rather than an arbitrary one.
#
# What it isolates. c enters the NCV only through the buffer b: net_share = (1-L)(1-T)(1-b),
# and neither L nor T is a function of c. Rescaling c and recomputing b via the engine's own
# practice_buffer_rate() therefore gives the exact c-effect with no other parameter moving.
# Modes:
#   uniform_*  one global multiplier on every biome's c, i.e. the precautionary case in which
#              correlation rises with warming, and its mirror below 1
#   observed   c set to the value measured per biome, a multiplier of c_obs/c_assumed
#   c_070_K1   c = 0.70 everywhere, the threshold at which K = round(1/c) reaches 1, so no
#              within-country diversification. K is floored at 1, so the response saturates
#              here and this bounds the effect. c = 1 is inadmissible: it makes the Beta
#              concentration zero.
#   proj_*     c replaced by the value projected in sens_c_trend.R, for the RCP8.5 2100
#              hazard-elasticity route and for the scenario-blind calendar extrapolation to
#              2050 and 2100
# The practice-specific c_mult in practices.csv is preserved and the sweep multiplies on top of
# it, as 04_headline.R applies it.
#
# Basis. Deterministic, matching clean_headline.csv, so this is a sensitivity of the closed-form
# central case and not comparable to the MC-median headline that is the paper's reporting
# convention. The reported quantity is the shift in b and in net_share; levels are labelled
# deterministic throughout.
#
# Emits: engine/output/sens_c_uplift.csv         (per practice x multiplier)
#        engine/output/sens_c_uplift_summary.csv (per multiplier, headline shift)
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
cat("[sens_c_uplift] bounded sensitivity of the headline to the correlation c\n")

source("engine/R/_utils.R")
source("engine/R/01_data.R")
source("engine/R/02_model.R")
source("engine/R/03_buffer.R")
# 04_headline.R is deliberately NOT sourced: it would rewrite the canonical headline
# outputs. Read the same practice grid it reads instead.
practices <- read.csv(.need("engine/params/practices.csv"), stringsAsFactors = FALSE)

# --- the empirically measured c, if the hexagon analysis has been run ------------
OBSF <- "engine/output/sens_within_country_c_biome.csv"
if (!file.exists(OBSF))
  stop("missing ", OBSF, "\n  run engine/R/sens_within_country_correlation.R first; the ",
       "observed-c mode must not fall back to a guessed range")
obs <- read.csv(OBSF, stringsAsFactors = FALSE)
# Preferred specification: detrended (levels conflate a shared trend with synchronised
# shocks) and excluding the author-constructed EFDA years 2017-2023.
obs <- obs[obs$basis == "detrended" & obs$year_window == "excl_2017_2023" &
             obs$biome != "ALL" & !is.na(obs$c_assumed), ]
if (!nrow(obs)) stop("no usable rows in ", OBSF)
obs$mult <- obs$c_observed_mean / obs$c_assumed
obs_mult <- setNames(obs$mult, obs$biome)
cat("[sens_c_uplift] measured c/assumed c:",
    paste(sprintf("%s %.2f", obs$biome, obs$mult), collapse = "  "), "\n")

# --- headline recomputation at a given set of biome multipliers ------------------
# Mirrors 04_headline.R's per-practice loop exactly; only c moves.
H_buf <- function(protected) if (protected) H_perm else 40
recompute <- function(mult_of_biome, tag) {
  do.call(rbind, lapply(seq_len(nrow(practices)), function(i) {
    row <- practices[i, ]
    s <- mult_of_biome[[row$biome]]
    if (is.null(s) || is.na(s))
      stop("no c multiplier for biome ", row$biome, " in mode ", tag)
    protected <- isTRUE(as.logical(row$legally_protected))
    x <- resolve_x(row)
    L <- leakage_L(row$practice, row$biome, x)
    T <- temporality_T(tau_2_temporality(row))
    pb <- practice_buffer_rate(row$biome, row$forest_type, H_buf(protected),
                               row$R_mult, row$lambda_mult, row$c_mult * s)
    data.frame(mode = tag, c_mult_applied = s, practice = row$practice,
               biome = row$biome, species = row$species,
               is_anchor = as.logical(row$is_anchor),
               L = L, T = T, b = pb$b, net_share = net_share(L, T, pb$b),
               stringsAsFactors = FALSE)
  }))
}

# Headline statistic, mirroring the deterministic clean_headline convention: average over
# species within a practice, then the median across practices.
headline_stat <- function(d) {
  per_practice <- aggregate(cbind(b, net_share) ~ practice + biome, d, mean)
  c(b = median(per_practice$b), net = median(per_practice$net_share))
}

BIOMES <- unique(practices$biome)
one <- function(s) setNames(as.list(rep(s, length(BIOMES))), BIOMES)

# Uniform grid: 0.7 brackets the measured low end, 2.0 a precautionary doubling of c.
# The response is stepwise, not smooth, because the pool size K = round(1/c) is an integer.
# K falls monotonically across the grid: Boreal (c 0.15) 10,8,7,5,4,3 and Temperate (0.20)
# 7,6,5,4,3,2. Mediterranean (0.25) gives 6,5,4,3,3,2 — the 1.25 and 1.50 points share K = 3
# and so return an identical buffer, which is unavoidable rather than a bug: over this range
# 1/c spans only five integers for c = 0.25, so six grid points must repeat one.
GRID <- c(0.70, 0.85, 1.00, 1.25, 1.50, 2.00)
runs <- lapply(GRID, function(s) recompute(one(s), sprintf("uniform_%.2f", s)))
# Observed mode: biome-specific, from the hexagon estimate. Biomes absent from the estimate
# would silently keep c unchanged, so require every modelled biome to have been measured.
miss <- setdiff(BIOMES, names(obs_mult))
if (length(miss))
  stop("observed-c mode: no measurement for biome(s) ", paste(miss, collapse = ", "),
       "; re-run sens_within_country_correlation.R with those countries extracted")
runs[[length(runs) + 1L]] <- recompute(as.list(obs_mult[BIOMES]), "observed")

# --- the structural bound: no within-country diversification at all ----------------
# K = round(1/c) is floored at 1, so the buffer's response to c SATURATES: once the pool is a
# single cell, more correlation cannot remove any further spatial averaging. That makes the
# worst case computable rather than open-ended.
#
# c = 1 cannot be used for it. 03_buffer sets the Beta concentration ab = (1-c)/c, so at c = 1
# ab = 0 and rbeta() receives a negative second shape, making every draw NaN; the batch-means
# assertion in bootstrap_buffer stops the run. c = 2/3 is the threshold where round(1/c)
# reaches 1, so c = 0.70 is the mildest configuration with zero spatial averaging and a
# well-defined Beta. Raising c further only adds within-cell dispersion; as c approaches 1 the
# cell becomes near-Bernoulli and the buffer degenerates towards total loss, which is not a
# useful bound.
C_NO_DIVERSIFICATION <- 0.70
runs[[length(runs) + 1L]] <- recompute(
  setNames(lapply(BIOMES, function(bm) {
    cb <- c_by_biome[[bm]]
    if (is.null(cb) || is.na(cb)) stop("no assumed c for biome ", bm)
    C_NO_DIVERSIFICATION / cb
  }), BIOMES), "c_070_K1")

# --- modes taken from the projected c (sens_c_trend.R) -----------------------------
# Uses c_mult_recalibrated: replace the assumed c with the projected one. Selected scenarios
# only, because each mode costs a full practice-grid re-bootstrap.
PROJF <- "engine/output/sens_c_trend_projection.csv"
if (!file.exists(PROJF))
  stop("missing ", PROJF, "\n  run engine/R/sens_c_trend.R first; the projected modes must ",
       "not fall back to a guessed multiplier")
pj <- read.csv(PROJF, stringsAsFactors = FALSE)
pj <- pj[pj$year_window == "excl_2017_2023", ]
WANT <- c("rcp85_2100", "calendar_2050", "calendar_2100")
for (sc in WANT) {
  x <- pj[pj$scenario == sc, ]
  m <- setNames(x$c_mult_recalibrated, x$biome)
  miss <- setdiff(BIOMES, names(m))
  if (length(miss))
    stop("projected mode ", sc, ": no multiplier for biome(s) ", paste(miss, collapse = ", "))
  runs[[length(runs) + 1L]] <- recompute(as.list(m[BIOMES]), paste0("proj_", sc))
}

det <- do.call(rbind, runs)
rownames(det) <- NULL

# The c_mult = 1.00 arm re-derives the canonical deterministic headline, so it must reproduce
# clean_headline.csv exactly. If it does not, this script is no longer measuring a pure
# c-effect (a changed parameter, a drifted helper, or an RNG-order dependence in the buffer),
# and every delta below would be contaminated.
chk <- merge(det[det$mode == "uniform_1.00", ],
             read.csv(.need("engine/output/clean_headline.csv"), stringsAsFactors = FALSE),
             by = c("practice", "biome", "species"), suffixes = c("_s", "_h"))
if (nrow(chk) != sum(det$mode == "uniform_1.00"))
  stop("baseline check: ", nrow(chk), " rows matched clean_headline but the baseline arm has ",
       sum(det$mode == "uniform_1.00"))
for (v in c("L", "T", "b", "net_share")) {
  dv <- max(abs(chk[[paste0(v, "_s")]] - chk[[paste0(v, "_h")]]))
  if (dv > 1e-12)
    stop("baseline arm does not reproduce clean_headline: max |d", v, "| = ", signif(dv, 3),
         " — the reported c-sensitivity would not be a pure c-effect")
}
cat("[sens_c_uplift] baseline arm reproduces clean_headline exactly (L, T, b, net_share)\n")

write.csv(det, "engine/output/sens_c_uplift.csv", row.names = FALSE)

base <- headline_stat(det[det$mode == "uniform_1.00", ])
summ <- do.call(rbind, lapply(unique(det$mode), function(m) {
  d <- det[det$mode == m, ]
  st <- headline_stat(d)
  data.frame(mode = m,
             c_mult_min = min(d$c_mult_applied), c_mult_max = max(d$c_mult_applied),
             median_b = st[["b"]], median_net_share = st[["net"]],
             delta_b_pp = 100 * (st[["b"]] - base[["b"]]),
             delta_net_share_pp = 100 * (st[["net"]] - base[["net"]]),
             b_min = min(d$b), b_max = max(d$b), stringsAsFactors = FALSE)
}))
write.csv(summ, "engine/output/sens_c_uplift_summary.csv", row.names = FALSE)

cat("\n--- deterministic headline vs the correlation multiplier (baseline = uniform_1.00) ---\n")
for (i in seq_len(nrow(summ))) with(summ[i, ],
  cat(sprintf("  %-14s c x %.2f-%.2f  median b %.3f (%+.2f pp)  median NCV %.3f (%+.2f pp)\n",
              mode, c_mult_min, c_mult_max, median_b, delta_b_pp,
              median_net_share, delta_net_share_pp)))
cat("\n[sens_c_uplift] wrote 2 tables to engine/output/\n")
