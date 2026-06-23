# =============================================================================
# 02b_efda_biome_timeseries.R - Observed per-year natural-disturbance series
# =============================================================================
# Builds a biome x year forest-area-weighted NATURAL disturbance-rate series
# from the committed EFDA per-country files (European Forest Disturbance Atlas
# v2.1.1, Viana-Soto & Senf 2025; codes 1=wind/beetle, 2=fire ONLY - harvest
# code 3 excluded; see docs/efda_extraction.md). This is the canonical
# OBSERVED series consumed by:
#   - 10_buffer_backtest.R       (historical backtest + disturbance_timeseries)
#   - 11_disturbance_analysis.R  (parameter-validation benchmark)
#
# It replaces (a) the synthetic disturbance_data.rds (base_rate+noise+event)
# and (b) the hand-built SENF_SEIDL_DISTURBANCE tribble ("visual extraction"
# + author estimates) that were previously plotted/validated as "observed".
#
# No silent fallback: stops if the EFDA per-country files or the biome map
# are missing. Output: data/processed/efda_biome_timeseries.rds + .csv
# Schema: biome <chr>, year <int>, lambda_natural <dbl>, n_zones <int>,
#         forest_kha <dbl> (total weighting area)
# =============================================================================

cat("Building EFDA observed biome x year disturbance series...\n")

local({
  efda_dir <- file.path(PATHS$data_processed, "efda_country_rates")
  map_csv  <- "data/country_biome_map.csv"
  if (!dir.exists(efda_dir) ||
      length(list.files(efda_dir, "\\.rds$")) == 0)
    stop("EFDA per-country files missing at ", efda_dir,
         " (see docs/efda_extraction.md). No synthetic fallback.")
  if (!file.exists(map_csv))
    stop("country_biome_map.csv missing at ", map_csv)

  bmap <- read.csv(map_csv, stringsAsFactors = FALSE)
  file2biome <- setNames(bmap$biome_4, bmap$efda_filename)

  files <- list.files(efda_dir, "\\.rds$", full.names = TRUE)
  stems <- sub("\\.rds$", "", basename(files))

  # Identify split parents (e.g. france, italy) that also have sub-zone files
  # (france_Temperate, italy_Mediterranean, ...); drop the whole-country file
  # to avoid double-counting, mirroring 02_data_extraction.R Part E.
  split_parents <- unique(sub("_.*$", "", stems[grepl("_", stems)]))
  keep <- !(stems %in% split_parents)

  rows <- list()
  for (i in which(keep)) {
    stem <- stems[i]
    d <- as.data.frame(readRDS(files[i]))
    if (grepl("_", stem)) {
      # split zone: biome is the suffix (Temperate / Mediterranean)
      biome <- sub("^[^_]*_", "", stem)
    } else {
      biome <- file2biome[[stem]]
    }
    if (is.null(biome) || is.na(biome)) {
      warning("No biome mapping for EFDA file '", stem, "' - skipped")
      next
    }
    d <- d[d$year >= 1986, ]   # match lambda_full window (02_data_extraction.R)
    rows[[stem]] <- data.frame(
      biome = biome, year = as.integer(d$year),
      lambda_natural = as.numeric(d$lambda_natural),
      forest_ha = as.numeric(d$forest_ha),
      stringsAsFactors = FALSE)
  }
  long <- do.call(rbind, rows)
  if (is.null(long) || nrow(long) == 0)
    stop("EFDA biome series build produced no rows")

  # Forest-area-weighted natural rate per biome x year
  agg <- do.call(rbind, lapply(
    split(long, list(long$biome, long$year), drop = TRUE),
    function(g) data.frame(
      biome          = g$biome[1],
      year           = g$year[1],
      lambda_natural = sum(g$forest_ha * g$lambda_natural) / sum(g$forest_ha),
      n_zones        = nrow(g),
      forest_kha     = sum(g$forest_ha) / 1000,
      stringsAsFactors = FALSE)))
  agg <- agg[order(agg$biome, agg$year), ]
  rownames(agg) <- NULL

  saveRDS(agg, file.path(PATHS$data_processed, "efda_biome_timeseries.rds"))
  write.csv(agg, file.path(PATHS$data_processed, "efda_biome_timeseries.csv"),
            row.names = FALSE)

  bm <- aggregate(lambda_natural ~ biome, agg, function(x)
    c(mean = mean(x), p90 = quantile(x, 0.9), max = max(x)))
  cat("  Saved efda_biome_timeseries.rds (", nrow(agg), " biome-years)\n", sep = "")
  cat("  Biome natural-disturbance summary (1986-2023):\n")
  for (i in seq_len(nrow(bm)))
    cat(sprintf("    %-14s mean=%.4f  p90=%.4f  max=%.4f\n",
                bm$biome[i], bm$lambda_natural[i, "mean"],
                bm$lambda_natural[i, "p90.90%"], bm$lambda_natural[i, "max"]))
})
