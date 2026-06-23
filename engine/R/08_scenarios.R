# =============================================================================
# 08_scenarios.R  —  CRCF deployment scenarios + NCV-adjusted area (fig5)
# =============================================================================
# Six EU-wide CRCF deployment scenarios (Regulation EU 2024/3012), each targeting
# CRCF_TARGET_MT MtCO2/yr: 3 pure pathways (one harvest class) + 3 mixed (60/25/15).
# Faithful port of build_crcf_scenario (analysis/R/03_parameters.R): scale area per
# harvest class to hit the target, split across each practice's plausible biomes by
# forest-area share. Then the NCV-adjusted area required TODAY = sum_i(area_i / NCV_i)
# aggregated at the Monte Carlo iteration level (Jensen-correct; 1/x is convex),
# using the engine's per-practice net_share draws (07_montecarlo). Feeds fig5 panel a.
# The over-time trajectory (fig5 panel b) and fig4 need the climate buffer trajectory
# (P2b, engine/09). Assumes 02_model.R + 07_montecarlo.R have run.
# =============================================================================
cat("[08_scenarios] CRCF deployment scenarios + NCV-adjusted area today...\n")

CRCF_TARGET_MT <- 100   # LULUCF shortfall target (MtCO2/yr)

base       <- read.csv("engine/params/crcf_base_practices.csv", stringsAsFactors = FALSE)
practices  <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
mc         <- readRDS("engine/output/mc_results.rds")

# Sync CRCF base rates to the anchor rates in practices.csv (post-Chiti override)
anchors <- practices[as.logical(practices$is_anchor), ]
for (i in seq_len(nrow(base))) {
  a <- anchors[anchors$practice == base$practice[i], ]
  if (nrow(a) == 1 && a$rate != base$rate[i]) base$rate[i] <- a$rate
}

# EU forest area shares per biome (Senf & Seidl; exclude Temperate_UK)
.senf <- as.data.frame(readRDS("data/processed/senf_biome_rates.rds"))
.senf <- .senf[.senf$biome != "Temperate_UK", ]
BIOME_FOREST_SHARES <- setNames(.senf$forest_kha, .senf$biome) / sum(.senf$forest_kha)

# practice -> plausible biomes (from practices.csv, exclude Temperate_UK)
PRACTICE_BIOMES <- tapply(practices$biome, practices$practice,
                          function(b) unique(b[b != "Temperate_UK"]))

# --- factory: scale area per harvest class to target, split across biomes -------
build_crcf_scenario <- function(target_mt, class_weights) {
  out <- base
  for (hc in c("Harvest-reducing", "Harvest-neutral", "Harvest-increasing")) {
    key <- c("Harvest-reducing" = "reducing", "Harvest-neutral" = "neutral",
             "Harvest-increasing" = "increasing")[[hc]]
    idx <- out$harvest_class == hc
    cur <- sum(out$rate[idx] * out$eu_area_ha[idx] / 1e6)
    if (cur > 0 && class_weights[[key]] > 0) {
      out$eu_area_ha[idx] <- out$eu_area_ha[idx] * (target_mt * class_weights[[key]] / cur)
    } else if (class_weights[[key]] == 0) {
      out$eu_area_ha[idx] <- 0
    }
  }
  expanded <- do.call(rbind, lapply(seq_len(nrow(out)), function(i) {
    row <- out[i, ]
    bs <- PRACTICE_BIOMES[[row$practice]]; if (is.null(bs)) bs <- "Temperate"
    sh <- BIOME_FOREST_SHARES[bs]; sh <- sh / sum(sh)
    do.call(rbind, lapply(seq_along(bs), function(j) {
      r <- row; r$biome <- bs[j]; r$eu_area_ha <- row$eu_area_ha * sh[j]; r
    }))
  }))
  expanded$eu_annual_MtCO2 <- expanded$rate * expanded$eu_area_ha / 1e6
  expanded
}

SCEN_WEIGHTS <- list(
  reducing_only    = c(reducing = 1.00, neutral = 0.00, increasing = 0.00),
  neutral_only     = c(reducing = 0.00, neutral = 1.00, increasing = 0.00),
  increasing_only  = c(reducing = 0.00, neutral = 0.00, increasing = 1.00),
  mixed_reducing   = c(reducing = 0.60, neutral = 0.25, increasing = 0.15),
  mixed_neutral    = c(reducing = 0.25, neutral = 0.60, increasing = 0.15),
  mixed_increasing = c(reducing = 0.25, neutral = 0.15, increasing = 0.60))
scenarios <- lapply(SCEN_WEIGHTS, function(w) build_crcf_scenario(CRCF_TARGET_MT, w))

# write the expanded practice x biome scenario table (long)
scen_tbl <- do.call(rbind, lapply(names(scenarios), function(nm) {
  d <- scenarios[[nm]]; d$scenario <- nm; d }))
write.csv(scen_tbl, "engine/output/scenario_practices.csv", row.names = FALSE)

# --- NCV-adjusted area required today (iteration-level MC aggregation) ----------
# anchor net_share draws per practice (10k-vector ordered by iteration)
anc_mc <- mc[mc$is_anchor, ]
anc_biome <- setNames(anchors$biome, anchors$practice)   # anchor biome per practice
ns_draws <- function(practice) {
  bm <- anc_biome[[practice]]; if (is.null(bm)) return(NULL)
  d <- anc_mc[anc_mc$practice == practice & anc_mc$biome == bm, ]
  if (nrow(d) == 0) return(NULL)
  d <- d[order(d$iteration), ]
  # if multiple anchor species share the practice/biome, use the first species' stream
  sp <- d$species[1]; d$net_share[d$species == sp]
}

area_today <- do.call(rbind, lapply(names(scenarios), function(nm) {
  s <- scenarios[[nm]]; s <- s[s$eu_area_ha > 0, ]
  area_mc <- NULL
  for (p in unique(s$practice)) {
    ns <- ns_draws(p); if (is.null(ns)) next
    area_p <- sum(s$eu_area_ha[s$practice == p])      # total area for practice (all biomes)
    contrib <- area_p / pmax(ns, 0.02)
    area_mc <- if (is.null(area_mc)) contrib else area_mc + contrib
  }
  area_mc <- area_mc / 1e6                            # -> Mha
  data.frame(scenario = nm, area_mean = median(area_mc),
             area_p5 = quantile(area_mc, .05), area_p25 = quantile(area_mc, .25),
             area_p75 = quantile(area_mc, .75), area_p95 = quantile(area_mc, .95),
             area_mean_arith = mean(area_mc),       # fig5a mean dot; median drives row order
             gross_area_Mha = sum(s$eu_area_ha) / 1e6,
             annual_MtCO2 = sum(s$eu_annual_MtCO2), stringsAsFactors = FALSE)
}))
rownames(area_today) <- NULL
write.csv(area_today, "engine/output/scenario_area_today.csv", row.names = FALSE)
cat(sprintf("[08_scenarios] OK — 6 scenarios; NCV-adjusted area today %.0f-%.0f Mha (target %.0f MtCO2/yr)\n",
            min(area_today$area_mean), max(area_today$area_mean), CRCF_TARGET_MT))
