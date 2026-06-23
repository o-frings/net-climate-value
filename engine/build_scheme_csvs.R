# =============================================================================
# build_scheme_csvs.R  —  ONE-TIME extractor (build tool, not part of engine run)
# =============================================================================
# Dumps SCHEME_PARAMS (analysis/R/03_parameters.R) to sourced CSVs under
# engine/params/, the same pattern as clean/build_param_csvs.R. Sources 03 ONCE
# to read the audited values (so the engine reproduces them exactly without any
# hand-transcription); the ENGINE RUNTIME (engine/R/*) never sources analysis/R.
# Regional override / buffer-exclusion (defined in 09_scheme_comparison.R) are
# small, stable facts encoded here explicitly.
#   Rscript engine/build_scheme_csvs.R
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
ACTIVE_SCENARIO <- "mixed_reducing"; CRCF_TARGET_MT <- 100
QUICK_MODE <- TRUE; N_ITERATIONS <- 1000; REEXTRACT_DATA <- FALSE
suppressWarnings(suppressMessages(source("prep/01_setup.R")))
invisible(capture.output({
  source("prep/03_parameters.R"); source("prep/02b_efda_biome_timeseries.R"); source("prep/04_functions.R")
}))

REGIONAL <- list(WCC = "Temperate_UK")          # from 09_scheme_comparison.R
EXCLUDE_BUFFER  <- c("CA_USFP")
EXCLUDE_FIGURES <- c("CA_USFP")

sp <- SCHEME_PARAMS
schemes <- do.call(rbind, lapply(names(sp), function(id) {
  s <- sp[[id]]
  data.frame(
    scheme_id = id, name = s$name, country = s$country,
    buffer_rate = s$buffer_rate, leakage_rate = s$leakage_rate,
    liability_years = s$liability_years,
    liability_years_min = if (is.null(s$liability_years_min)) s$liability_years else s$liability_years_min,
    regional_override = if (is.null(REGIONAL[[id]])) "" else REGIONAL[[id]],
    exclude_buffer  = id %in% EXCLUDE_BUFFER,
    exclude_figures = id %in% EXCLUDE_FIGURES,
    notes = if (is.null(s$notes)) "" else s$notes,   # scheme rulebook / version provenance
    stringsAsFactors = FALSE)
}))

# Documented provenance of the practice / forest-type weights (from the SCHEME_PARAMS
# comment blocks in 03_parameters.R — comments are not extractable as data, so the
# sourced strings are carried here so the CSV is self-describing).
WEIGHTS_SOURCE <- c(
  LBC = "Credit shares: I4CE / Observatoire de la foret & resoilag bilan LBC 2023 (Boisement 44%, Reconstitution 56%); BL/CF: WWF 2021 + IGN inventory",
  WCC = "Carbon-share weighted: Forest Research Forestry Statistics 2025 (Scotland=83% of WCC carbon) + Scottish Govt EIR FOI/202400398009; England FC woodland-creation stats",
  PLC = "Single covered practice (peatland rewetting); no cross-practice/forest-type weighting",
  WKS = "German post-bark-beetle/storm reconstruction, CF-dominated (Waldumbau ~70% CF)",
  KSF = "Danish national forest ~50/50; KSF climate-adaptation skew toward broadleaf",
  CA_USFP = "US PNW/SE IFM conifer-dominated (~85% CF); CARB protocol coverage")

# coverage + weights: one row per (scheme, covered practice)
cov <- do.call(rbind, lapply(names(sp), function(id) {
  s <- sp[[id]]
  do.call(rbind, lapply(s$covered_practices, function(p) {
    pw <- if (is.null(s$practice_weights)) NA_real_ else {
      v <- s$practice_weights[p]; if (is.na(v)) NA_real_ else as.numeric(v) }
    ftw <- if (is.null(s$forest_type_weights)) NULL else s$forest_type_weights[[p]]
    data.frame(scheme_id = id, practice = p,
               practice_weight = pw,
               ft_broadleaf = if (is.null(ftw)) NA_real_ else as.numeric(ftw["broadleaf"]),
               ft_conifer   = if (is.null(ftw)) NA_real_ else as.numeric(ftw["conifer"]),
               source = if (is.null(WEIGHTS_SOURCE[[id]])) "" else WEIGHTS_SOURCE[[id]],
               stringsAsFactors = FALSE)
  }))
}))

# H_ref sensitivity grid (for integrity-gap href tables; constants from 06_baseline_issuance.R)
href <- data.frame(label = c("100yr", "1000yr", "Inf"), H_ref = c(100, 1000, Inf),
                   stringsAsFactors = FALSE)

dir.create("engine/params", showWarnings = FALSE, recursive = TRUE)
write.csv(schemes, "engine/params/schemes.csv", row.names = FALSE)
write.csv(cov,     "engine/params/scheme_coverage.csv", row.names = FALSE)
write.csv(href,    "engine/params/href_values.csv", row.names = FALSE)
cat(sprintf("wrote schemes.csv (%d), scheme_coverage.csv (%d), href_values.csv (%d)\n",
            nrow(schemes), nrow(cov), nrow(href)))
