# =============================================================================
# sens_additive.R  —  SI robustness: multiplicative vs additive issuance rule
# =============================================================================
# Filewod review A1: is the sequential multiplicative rule net_share =
# (1-L)(1-T)(1-b) defensible against a simultaneous/additive alternative that
# subtracts the three rates from one? This script applies the additive rule
#   sigma_add = max(0, 1 - L - T - b)
# to the SAME per-draw L, T, b the canonical engine already produced, so the two
# rules differ only in how the three deductions combine. It is a pure post-
# processing step (reads engine/output/, writes only sens_additive_*.csv; the
# canonical multiplicative headline is untouched). Reports (i) the headline level
# shift, (ii) practice-ranking invariance (Spearman between the two rules), (iii)
# scheme integrity-gap sign invariance, and (iv) how often the additive floor at
# zero binds (the pathology that motivates the multiplicative form). Run from
# analysis/:  Rscript engine/R/sens_additive.R
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
need <- c("engine/output/mc_results.rds", "engine/output/scheme_gaps.csv",
          "engine/params/schemes.csv", "engine/output/clean_headline.csv")
miss <- need[!file.exists(need)]
if (length(miss)) stop("run the engine first; missing: ", paste(miss, collapse = ", "))

mc <- as.data.frame(readRDS("engine/output/mc_results.rds"))
mc$add_share <- pmax(0, 1 - mc$L - mc$T - mc$b)          # additive rule, floored at 0

# --- (i) headline level shift + (iv) floor incidence --------------------------
# Mirror the canonical headline stat (sens_leak_rev.R): per practice x biome x
# species take the MC median, keep anchors, average over species within practice,
# then the median across the 16 headline practices.
cell <- aggregate(cbind(net_share, add_share) ~ practice + biome + species + is_anchor,
                  mc, median)
anc  <- cell[cell$is_anchor, ]
pp   <- merge(aggregate(net_share ~ practice, anc, mean),
              aggregate(add_share ~ practice, anc, mean), by = "practice")
ncv16_mult <- median(pp$net_share)
ncv16_add  <- median(pp$add_share)

# floor incidence: share of anchor-practice draws with 1 - L - T - b < 0
manc <- mc[mc$is_anchor, ]
manc$below0 <- (1 - manc$L - manc$T - manc$b) < 0
floor_overall <- mean(manc$below0)
floor_by_practice <- aggregate(below0 ~ practice, manc, mean)
floor_by_practice <- floor_by_practice[order(-floor_by_practice$below0), ]

# --- (ii) practice-ranking invariance -----------------------------------------
sp16 <- cor(pp$net_share, pp$add_share, method = "spearman")               # 16 headline
cell37 <- merge(aggregate(net_share ~ practice + biome, cell, mean),
                aggregate(add_share ~ practice + biome, cell, mean),
                by = c("practice", "biome"))
sp37 <- cor(cell37$net_share, cell37$add_share, method = "spearman")       # 37 combos

# --- (iii) scheme integrity-gap sign invariance -------------------------------
# Recompute each scheme's gap under the additive rule from the components the
# engine stored (10_schemes.R), keeping T_scheme = 0 (no scheme prices time) and
# the CA_USFP buffer exclusion. Multiplicative gap comes straight from the engine.
sg <- read.csv("engine/output/scheme_gaps.csv", stringsAsFactors = FALSE)
sch <- read.csv("engine/params/schemes.csv", stringsAsFactors = FALSE)
sg <- merge(sg, sch[, c("scheme_id", "leakage_rate", "buffer_rate", "exclude_buffer")],
            by.x = "scheme", by.y = "scheme_id", all.x = TRUE)
eb <- as.logical(sg$exclude_buffer)
scheme_ns_add <- ifelse(eb, pmax(0, 1 - sg$leakage_rate),
                             pmax(0, 1 - sg$leakage_rate - sg$buffer_rate))
prop_ns_add   <- ifelse(eb, pmax(0, 1 - sg$L_prop - sg$T_prop),
                             pmax(0, 1 - sg$L_prop - sg$T_prop - sg$b_prop))
sg$gap_add  <- (scheme_ns_add - prop_ns_add) / scheme_ns_add
sg$gap_mult <- sg$integrity_gap_pct

source("engine/R/_utils.R")   # shared wmean
scheme_cmp <- do.call(rbind, lapply(split(sg, sg$scheme), function(d) data.frame(
  scheme = d$scheme[1], scheme_name = d$scheme_name[1],
  is_figure_scheme = !isTRUE(d$exclude_figures[1]),
  gap_mult = wmean(d$gap_mult, d$joint_weight),
  gap_add  = wmean(d$gap_add,  d$joint_weight), stringsAsFactors = FALSE)))
scheme_cmp <- scheme_cmp[order(-scheme_cmp$gap_mult), ]
rownames(scheme_cmp) <- NULL

# --- write + report -----------------------------------------------------------
summary_df <- data.frame(
  stat = c("headline_ncv_median16", "practice_ranking_spearman_16",
           "practice_ranking_spearman_37", "additive_floor_incidence_overall"),
  multiplicative = c(ncv16_mult, NA, NA, NA),
  additive = c(ncv16_add, NA, NA, floor_overall),
  spearman_mult_vs_add = c(NA, sp16, sp37, NA))
write.csv(summary_df, "engine/output/sens_additive_summary.csv", row.names = FALSE)
write.csv(pp[order(-pp$net_share), ], "engine/output/sens_additive_practices.csv", row.names = FALSE)
write.csv(scheme_cmp, "engine/output/sens_additive_schemes.csv", row.names = FALSE)
write.csv(floor_by_practice, "engine/output/sens_additive_floor.csv", row.names = FALSE)

cat(sprintf("[sens_additive] headline NCV: multiplicative %.1f%% vs additive %.1f%% (%.1f pp lower)\n",
            100 * ncv16_mult, 100 * ncv16_add, 100 * (ncv16_mult - ncv16_add)))
cat(sprintf("[sens_additive] practice-ranking Spearman (mult vs add): %.3f (16 practices), %.3f (37 combos)\n",
            sp16, sp37))
cat(sprintf("[sens_additive] additive floor at 0 binds in %.1f%% of anchor draws; top practices:\n",
            100 * floor_overall))
for (i in seq_len(min(4, nrow(floor_by_practice))))
  cat(sprintf("               %-28s %.1f%%\n", floor_by_practice$practice[i],
              100 * floor_by_practice$below0[i]))
cat("[sens_additive] scheme integrity gaps (multiplicative -> additive):\n")
for (i in seq_len(nrow(scheme_cmp)))
  cat(sprintf("               %-22s %+5.0f%% -> %+5.0f%%%s\n",
              scheme_cmp$scheme_name[i], 100 * scheme_cmp$gap_mult[i],
              100 * scheme_cmp$gap_add[i],
              if (scheme_cmp$is_figure_scheme[i]) "" else "  (text-only)"))
sign_ok <- all(sign(scheme_cmp$gap_mult) == sign(scheme_cmp$gap_add))
cat(sprintf("[sens_additive] all scheme gap signs invariant across the two rules: %s\n",
            if (sign_ok) "YES" else "NO"))
