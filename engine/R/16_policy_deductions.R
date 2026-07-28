# =============================================================================
# 16_policy_deductions.R — per-practice x biome deduction rates for policy use
# =============================================================================
# Requested by Mirco Migliavacca (JRC) review, 2026-07-28: supply the numbers a
# methodology could apply as deductions per practice and biome — in particular the
# market-leakage discount, which the adopted CRCF carbon-farming methodology does
# not quantify — analogous to how the uncertainty deduction discounts CR-RC_bas.
#
# Basis: MC medians (n = 10,000), the reporting convention used throughout the
# manuscript, because NCV distributions are right-skewed. Per-channel medians come
# from 07_montecarlo.R (p50_L / p50_T / p50_b). Medians of the individual channels
# do NOT multiply exactly to the median net share (the median is not a linear
# operator), so ncv_pct is the median net share as computed, not a product of columns.
#
# Emits: output/policy_deductions.csv (one row per practice x biome x forest type)
#        output/policy_deductions_leakage.csv (leakage only, collapsed to practice)
# Assumes 04_headline.R and 07_montecarlo.R have run.
# =============================================================================
cat("[16] policy deduction table\n")

.pd <- read.csv("engine/output/mc_summary.csv", stringsAsFactors = FALSE)
.dt <- read.csv("engine/output/clean_headline.csv", stringsAsFactors = FALSE)

stopifnot(all(c("p50_L", "p50_T", "p50_b", "p50_share") %in% names(.pd)))

# Both bases are reported side by side because they differ materially and the
# choice is a policy decision, not a modelling one. The MC median of kappa is
# 0.709 (triangular on [0.33, 1.27], mode 0.60), not 0.60, so MC-median leakage
# runs ~7 pp above the deterministic central value the main text quotes as
# L in [0.15, 0.37]. A methodology quoting one basis must not cite the other's range.
.keys <- c("practice", "biome", "species")
.n_before <- nrow(.pd)
.pd <- merge(.pd, .dt[, c(.keys, "L", "T", "b", "net_share")], by = .keys)
# Inner join on purpose: a missing deterministic counterpart is a pipeline fault, not
# a case to fill with NA. Fail here rather than emit a policy table with holes in it.
stopifnot(nrow(.pd) == .n_before,
          !anyNA(.pd[, c("p50_L", "p50_T", "p50_b", "L", "T", "b", "net_share")]))

policy_deductions <- data.frame(
  practice        = .pd$practice,
  biome           = .pd$biome,
  forest_type     = .pd$species,
  is_anchor_biome = .pd$is_anchor,
  leakage_pct     = round(100 * .pd$p50_L, 1),
  temporality_pct = round(100 * .pd$p50_T, 1),
  buffer_pct      = round(100 * .pd$p50_b, 1),
  ncv_pct         = round(100 * .pd$p50_share, 1),
  leakage_pct_central     = round(100 * .pd$L, 1),
  temporality_pct_central = round(100 * .pd$T, 1),
  buffer_pct_central      = round(100 * .pd$b, 1),
  ncv_pct_central         = round(100 * .pd$net_share, 1),
  stringsAsFactors = FALSE
)
policy_deductions <- policy_deductions[order(-policy_deductions$ncv_pct), ]
rownames(policy_deductions) <- NULL

write.csv(policy_deductions, "engine/output/policy_deductions.csv", row.names = FALSE)

# --- leakage-only view: the channel the CRCF methodology leaves unquantified ---
# L = kappa * rho^rep * x varies by biome, because rho^rep is built from biome-specific
# elasticities (up to ~11 pp across biomes for extended rotation), so no single number
# represents a practice. The headline figure is the ANCHOR-biome value — the manuscript's
# convention for per-practice reporting — with the cross-biome range beside it. An
# unweighted mean across rows would silently weight Temperate double wherever a practice
# carries both a conifer and a broadleaf variant, so it is deliberately not reported.
.agg <- do.call(rbind, by(policy_deductions, policy_deductions$practice, function(d) {
  anchor <- d[d$is_anchor_biome, ]
  # A practice may carry two anchor rows (conifer + broadleaf). Leakage does not depend
  # on forest type by construction, so collapsing them is legitimate -- but enforce that
  # invariant on the deterministic values rather than trusting it, and bound the MC
  # sampling noise that makes the median rows differ slightly.
  stopifnot(nrow(anchor) >= 1,
            length(unique(anchor$leakage_pct_central)) == 1,
            diff(range(anchor$leakage_pct)) < 1)
  data.frame(
    practice          = d$practice[1],
    anchor_biome      = anchor$biome[1],
    leakage_pct       = round(mean(anchor$leakage_pct), 1),
    leakage_pct_central = anchor$leakage_pct_central[1],
    leakage_min       = min(d$leakage_pct),
    leakage_max       = max(d$leakage_pct),
    n_biomes          = length(unique(d$biome)),
    stringsAsFactors  = FALSE)
}))
.agg <- .agg[order(-.agg$leakage_pct), ]
rownames(.agg) <- NULL
write.csv(.agg, "engine/output/policy_deductions_leakage.csv", row.names = FALSE)

cat(sprintf("     %d practice-biome rows; leakage span %.1f%% to %.1f%%\n",
            nrow(policy_deductions), min(.agg$leakage_min), max(.agg$leakage_max)))
