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

# EU forest area shares per biome (Senf & Seidl; exclude Temperate_UK)
.senf <- as.data.frame(readRDS("data/processed/senf_biome_rates.rds"))
.senf <- .senf[.senf$biome != "Temperate_UK", ]
BIOME_FOREST_SHARES <- setNames(.senf$forest_kha, .senf$biome) / sum(.senf$forest_kha)

# --- cell definitions, all read from practices.csv (no literals) ----------------
# A scenario cell is practice x biome x forest_type, the same grain as the MC draws and
# the buffer trajectories. The old code collapsed both of the latter two: it kept a
# practice-level rate (the sync was skipped whenever a practice had two anchor rows) and
# left forest type to be guessed downstream by a first-row pick. Every cell here instead
# carries its own sourced rate.
.pc <- practices[practices$biome != "Temperate_UK", ]
.cellkey <- function(p, b, f) paste(p, b, f, sep = "\r")
if (anyDuplicated(.cellkey(.pc$practice, .pc$biome, .pc$forest_type)))
  stop("practices.csv: duplicate practice x biome x forest_type cell")
CELL_RATE <- setNames(.pc$rate, .cellkey(.pc$practice, .pc$biome, .pc$forest_type))
# forest types that actually exist for a given practice x biome (varies by biome: e.g.
# extended rotation is conifer-only in Boreal and Mediterranean but both in Temperate)
PRACTICE_BIOME_FTS <- tapply(.pc$forest_type,
                             list(.pc$practice, .pc$biome), function(x) unique(x))
PRACTICE_BIOMES <- tapply(.pc$biome, .pc$practice, unique)
# conifer share per biome, derived in 01_data.R from the sourced SoEF areas in
# engine/params/forest_type_area_shares.csv
CONIFER_SHARE <- setNames(forest_type_shares$conifer_share, forest_type_shares$biome)

# --- factory: expand to cells, then scale area per harvest class to target -------
# Scaling happens AFTER expansion because the rate is now per cell, so the class tonnage
# is sum(rate_cell * area_cell) and cannot be evaluated at practice level.
build_crcf_scenario <- function(target_mt, class_weights) {
  expanded <- do.call(rbind, lapply(seq_len(nrow(base)), function(i) {
    row <- base[i, ]
    bs <- PRACTICE_BIOMES[[row$practice]]
    if (is.null(bs)) stop("no biomes in practices.csv for: ", row$practice)
    bsh <- BIOME_FOREST_SHARES[bs]; bsh <- bsh / sum(bsh)
    do.call(rbind, lapply(seq_along(bs), function(j) {
      bm  <- bs[j]
      fts <- PRACTICE_BIOME_FTS[[row$practice, bm]]
      if (is.null(fts)) stop("no forest type for ", row$practice, " / ", bm)
      cs <- CONIFER_SHARE[[bm]]
      if (is.na(cs)) stop("no conifer share for biome ", bm)
      # split the biome's area between the forest types present, by the biome composition;
      # normalising means a practice available as only one type takes that biome's whole
      # area rather than silently losing the other type's share.
      fw <- ifelse(fts == "conifer", cs, 1 - cs); fw <- fw / sum(fw)
      do.call(rbind, lapply(seq_along(fts), function(k) {
        key <- .cellkey(row$practice, bm, fts[k])
        if (!key %in% names(CELL_RATE)) stop("no rate for cell ", gsub("\r", "/", key))
        r <- row
        r$biome       <- bm
        r$forest_type <- fts[k]
        r$rate        <- CELL_RATE[[key]]
        r$eu_area_ha  <- row$eu_area_ha * bsh[j] * fw[k]
        r
      }))
    }))
  }))
  for (hc in c("Harvest-reducing", "Harvest-neutral", "Harvest-increasing")) {
    key <- c("Harvest-reducing" = "reducing", "Harvest-neutral" = "neutral",
             "Harvest-increasing" = "increasing")[[hc]]
    idx <- expanded$harvest_class == hc
    cur <- sum(expanded$rate[idx] * expanded$eu_area_ha[idx] / 1e6)
    if (cur > 0 && class_weights[[key]] > 0) {
      expanded$eu_area_ha[idx] <- expanded$eu_area_ha[idx] *
        (target_mt * class_weights[[key]] / cur)
    } else if (class_weights[[key]] == 0) {
      expanded$eu_area_ha[idx] <- 0
    }
  }
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

# Realised area-weighted conifer share per scenario. The Methods quote these six figures;
# previously they reflected a first-row forest-type pick per practice, now they are the
# actual area shares implied by the practice mix and the sourced biome composition.
scen_conifer <- do.call(rbind, lapply(names(scenarios), function(nm) {
  d <- scenarios[[nm]]; d <- d[d$eu_area_ha > 0, ]
  data.frame(scenario = nm,
             conifer_share = sum(d$eu_area_ha[d$forest_type == "conifer"]) / sum(d$eu_area_ha),
             stringsAsFactors = FALSE)
}))
write.csv(scen_conifer, "engine/output/scenario_conifer_share.csv", row.names = FALSE)
cat(sprintf("[08_scenarios] area-weighted conifer share by scenario: %s\n",
            paste(sprintf("%s %.0f%%", scen_conifer$scenario,
                          100 * scen_conifer$conifer_share), collapse = ", ")))

# --- NCV-adjusted area required today (iteration-level MC aggregation) ----------
# net_share draws for a scenario CELL (practice x biome), as a 10k-vector ordered by
# iteration. Area is spread across a practice's biomes by forest-area share, so the
# divisor must be that biome's own NCV: the previous version summed area over ALL biomes
# but divided by the ANCHOR biome's first species, and because 1/NCV is convex that
# understates the requirement wherever NCV is biome-heterogeneous (most for
# harvest-reducing practices, whose Mediterranean cells sit far below their Temperate
# anchors). Same Jensen argument this file already applies across MC iterations.
#
# Forest type is now a scenario dimension too, so a cell resolves to one forest type; any
# remaining multiple species within (practice, biome, forest_type) are averaged per draw.
.SPECIES_FT <- setNames(practices$forest_type, practices$species)[!duplicated(practices$species)]
ns_draws_cell <- function(practice, biome, forest_type) {
  d <- mc[mc$practice == practice & mc$biome == biome, ]
  d <- d[.SPECIES_FT[d$species] == forest_type, ]
  if (nrow(d) == 0)
    stop("no MC draws for scenario cell: ", practice, " / ", biome, " / ", forest_type)
  sp <- unique(d$species)
  m <- vapply(sp, function(s) {
    v <- d[d$species == s, ]
    v$net_share[order(v$iteration)]
  }, numeric(max(d$iteration)))
  if (length(sp) > 1) rowMeans(m) else as.vector(m)
}

.floor_hits <- 0L; .floor_n <- 0L
area_today <- do.call(rbind, lapply(names(scenarios), function(nm) {
  s <- scenarios[[nm]]; s <- s[s$eu_area_ha > 0, ]
  area_mc <- NULL
  for (i in seq_len(nrow(s))) {                       # one row per practice x biome
    ns <- ns_draws_cell(s$practice[i], s$biome[i], s$forest_type[i])
    # The 0.02 floor regularises the 1/x singularity, so it truncates the right tail
    # of the area distribution. Count how often it binds: a reader otherwise cannot
    # tell whether area_p95 is data or the floor.
    .floor_hits <<- .floor_hits + sum(ns < 0.02); .floor_n <<- .floor_n + length(ns)
    contrib <- s$eu_area_ha[i] / pmax(ns, 0.02)
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
cat(sprintf("[08_scenarios] 2%% net-share floor bound on %d of %d practice-draws (%.3f%%); above zero it truncates the upper area tail\n",
            .floor_hits, .floor_n, 100 * .floor_hits / max(.floor_n, 1)))

# --- Foregone-harvest / market-saturation check (Murray phi at EU scale) --------
# The per-project leakage rate is the marginal (phi->0) MAXIMUM (02_model); a
# scale-dependent (phi>0) treatment would attenuate it. As an upper bound we instead
# report the aggregate foregone harvest at the NCV-adjusted deployment area (same ns
# draws as the area above), summed over harvest-reducing practices (x>0), against the
# EU-27 roundwood removals 460 Mm3 (2023), 510 Mm3 (2022) [Eurostat] x ~0.9 tCO2/m3
# (basic wood density ~0.5 t/m3, 50% C, x 44/12; IPCC 2006 AFOLU) ~= 450 MtCO2/yr
# (conservative high end of the ~410-460 range; value unchanged, provenance tightened).
# When this approaches/exceeds 100%, harvest reduction saturates the timber market
# before the land requirement is met -> the scenario is infeasible on market grounds.
EU_ROUNDWOOD_MTCO2 <- 450
x_by_practice <- tapply(practices$harvest_displacement, practices$practice, function(v) v[1])
foregone_tbl <- do.call(rbind, lapply(names(scenarios), function(nm) {
  s <- scenarios[[nm]]; s <- s[s$eu_area_ha > 0, ]
  fore_mc <- 0
  for (i in seq_len(nrow(s))) {                             # per practice x biome, as above
    p  <- s$practice[i]
    if (!p %in% names(x_by_practice))
      stop("no harvest displacement x for scenario practice: ", p)
    xp <- x_by_practice[[p]]
    if (is.na(xp) || xp <= 0) next                          # harvest-reducing withdrawal only
    ns <- ns_draws_cell(p, s$biome[i], s$forest_type[i])
    G_p <- s$eu_annual_MtCO2[i]                             # face gross contribution (MtCO2/yr)
    fore_mc <- fore_mc + xp * G_p / pmax(ns, 0.02)          # foregone harvest at NCV-adjusted deployment
  }
  data.frame(scenario = nm,
             foregone_MtCO2 = median(fore_mc), foregone_p5 = quantile(fore_mc, .05),
             foregone_p95 = quantile(fore_mc, .95),
             pct_EU_roundwood = 100 * median(fore_mc) / EU_ROUNDWOOD_MTCO2,
             stringsAsFactors = FALSE)
}))
rownames(foregone_tbl) <- NULL
write.csv(foregone_tbl, "engine/output/scenario_leakage_scale.csv", row.names = FALSE)
cat(sprintf("[08_scenarios] foregone harvest at deployment (harvest-reducing withdrawal): %.0f-%.0f%% of EU roundwood\n",
            min(foregone_tbl$pct_EU_roundwood), max(foregone_tbl$pct_EU_roundwood)))
