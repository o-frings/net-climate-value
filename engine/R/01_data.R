# =============================================================================
# clean_01_data.R  —  data-calibration layer (recomputed from primary files)
# =============================================================================
# Independently recomputes the data-DERIVED biome parameters from the committed
# primary data. Nothing here is hardcoded; every value comes from a real file.
# Fail-loud: any missing file or empty result stops the run.
#
# Produces data.frame `biome_data` with one row per biome:
#   lambda_obs : forest-area-weighted mean NATURAL disturbance rate (harvest
#                excluded), EFDA 1986-2023  [from efda_country_summary]
#   severity   : Senf & Seidl (2021) mean disturbance severity  [senf_biome_rates]
#   S_ref      : UNECE/Forest Europe reference above-ground stock, tCO2/ha [sref_biome]
#   U_50/U_100/U_peak (RCP4.5 + RCP8.5) : Grünig climate uplift [gruenig_uplift_factors]
# =============================================================================
cat("[clean_01_data] recomputing biome calibration from primary files...\n")

.need <- function(p) { if (!file.exists(p)) stop("MISSING primary file: ", p,
                       " (no fallback — clean run requires real data)"); p }

# --- lambda_obs: area-weighted EFDA natural rate per biome --------------------
efda <- as.data.frame(readRDS(.need("data/processed/efda_country_summary.rds")))
stopifnot(all(c("biome","lambda_full","forest_kha") %in% names(efda)))
ok <- !is.na(efda$lambda_full) & !is.na(efda$forest_kha)
if (!any(ok)) stop("efda_country_summary has no usable rows")
lambda_obs <- tapply(seq_len(nrow(efda))[ok], efda$biome[ok], function(ix)
  sum(efda$forest_kha[ix] * efda$lambda_full[ix]) / sum(efda$forest_kha[ix]))

# --- severity: Senf & Seidl per biome ----------------------------------------
senf <- as.data.frame(readRDS(.need("data/processed/senf_biome_rates.rds")))
stopifnot(all(c("biome","severity") %in% names(senf)))
severity <- setNames(senf$severity, senf$biome)

# --- S_ref: UNECE reference stock per biome ----------------------------------
sref <- as.data.frame(readRDS(.need("data/processed/sref_biome.rds")))
stopifnot(all(c("biome","S_ref") %in% names(sref)))
S_ref <- setNames(sref$S_ref, sref$biome)

# --- climate uplift U: Grünig per biome x RCP --------------------------------
# Grünig covers Boreal/Temperate/Mediterranean only. Temperate_UK uplift is a
# DERIVED value: Temperate x oceanic-buffer factor (no primary file). The factor
# is read from the sourced param CSV (NOT hardcoded here).
gru <- as.data.frame(readRDS(.need("data/processed/gruenig_uplift_factors.rds")))
stopifnot(all(c("biome_3","scen","U_50","U_100","U_peak") %in% names(gru)))
.mc <- read.csv(.need("engine/params/model_constants.csv"), stringsAsFactors = FALSE)
uk_factor <- .mc$value[.mc$name == "uk_oceanic_uplift_factor"]
if (length(uk_factor) != 1) stop("uk_oceanic_uplift_factor missing from model_constants.csv")
.uplift_for <- function(b, scen) {
  src_biome <- if (b == "Temperate_UK") "Temperate" else b
  g <- gru[gru$biome_3 == src_biome & gru$scen == scen, ]
  if (nrow(g) != 1) stop("Grünig uplift not uniquely found for ", src_biome, " / ", scen)
  mult <- if (b == "Temperate_UK") uk_factor else 1
  list(U_50 = g$U_50 * mult, U_100 = g$U_100 * mult, U_peak = g$U_peak * mult)
}

# --- assemble (biomes present in all sources; fail if a biome is missing one) -
biomes <- sort(unique(efda$biome))
biome_data <- do.call(rbind, lapply(biomes, function(b) {
  if (is.na(lambda_obs[b])) stop("no lambda_obs for biome ", b)
  if (!b %in% names(severity)) stop("no Senf severity for biome ", b)
  if (!b %in% names(S_ref))    stop("no S_ref for biome ", b)
  g45 <- .uplift_for(b, "RCP4.5")
  g85 <- .uplift_for(b, "RCP8.5")
  data.frame(biome = b,
             lambda_obs = round(unname(lambda_obs[b]), 6),
             severity   = round(unname(severity[b]), 4),
             S_ref      = round(unname(S_ref[b]), 2),
             U_50 = g45$U_50, U_100 = g45$U_100, U_peak = g45$U_peak,
             U_50_rcp85 = g85$U_50, U_100_rcp85 = g85$U_100, U_peak_rcp85 = g85$U_peak,
             stringsAsFactors = FALSE)
}))
rownames(biome_data) <- NULL

dir.create("engine/output", showWarnings = FALSE, recursive = TRUE)
write.csv(biome_data, "engine/output/derived_biome_params.csv", row.names = FALSE)
cat(sprintf("[clean_01_data] OK — %d biomes:\n", nrow(biome_data)))
print(biome_data[, c("biome","lambda_obs","severity","S_ref","U_50","U_50_rcp85")])

# --- forest-type composition per biome ----------------------------------------
# Derived here from the sourced per-country areas in
# engine/params/forest_type_area_shares.csv (Forest Europe 2025 SoEF annex, Table 9)
# rather than stated as a literal, so the parameter file stays the single source.
#
# "Mixed" is allocated proportionally to the two dominated classes, which reduces
# exactly to conifer_share = C / (C + B): distributing M in ratio C:B gives
# [C + M*C/(C+B)] / (C+B+M) = C/(C+B). The mixed column therefore does not enter,
# and countries whose mixed cell is blank (Bulgaria, United Kingdom) stay usable.
#
# Country -> biome and the aggregation weight both come from the EFDA summary, so this
# matches how biome buffers are weighted elsewhere in the engine.
.fts <- read.csv(.need("engine/params/forest_type_area_shares.csv"), stringsAsFactors = FALSE)
.fts$conifer_share <- .fts$area_conifer_kha / (.fts$area_conifer_kha + .fts$area_broadleaf_kha)
.efda_ft <- aggregate(forest_kha ~ country_root + biome, efda, sum)
.ft <- merge(.efda_ft, .fts[, c("country", "conifer_share")],
             by.x = "country_root", by.y = "country", all.x = TRUE)
forest_type_shares <- do.call(rbind, lapply(sort(unique(.ft$biome)), function(b) {
  d <- .ft[.ft$biome == b, ]
  g <- d[!is.na(d$conifer_share), ]
  if (!nrow(g)) stop("no forest-type composition for any country in biome ", b)
  data.frame(biome = b,
             conifer_share = sum(g$conifer_share * g$forest_kha) / sum(g$forest_kha),
             area_coverage = sum(g$forest_kha) / sum(d$forest_kha),
             n_countries = nrow(g), n_total = nrow(d),
             missing = paste(d$country_root[is.na(d$conifer_share)], collapse = ";"),
             stringsAsFactors = FALSE)
}))
write.csv(forest_type_shares, "engine/output/forest_type_shares.csv", row.names = FALSE)
# Report the coverage gap rather than letting an incomplete weighted mean pass silently.
for (i in seq_len(nrow(forest_type_shares))) with(forest_type_shares[i, ],
  cat(sprintf("[clean_01_data] forest type %-14s conifer %.1f%% (%d/%d countries, %.0f%% of area)%s\n",
              biome, 100 * conifer_share, n_countries, n_total, 100 * area_coverage,
              if (nzchar(missing)) paste0(" — no data: ", missing) else "")))
