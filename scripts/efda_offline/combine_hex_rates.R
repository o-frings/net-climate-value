# Combine per-country hexagon series into one committed artefact the engine can read.
# Usage (from ~/efda_scratch): Rscript combine_hex_rates.R [out_path]
# Safe to run on a PARTIAL set — it reports how many countries are included.
args <- commandArgs(trailingOnly = TRUE)
# Destination must be given explicitly (argument or NCV_ANALYSIS_DIR), so this never writes
# to a hardcoded absolute path from someone else's machine.
out <- if (length(args) >= 1) args[1] else {
  d <- Sys.getenv("NCV_ANALYSIS_DIR", unset = "")
  if (!nzchar(d))
    stop("no destination given. Pass the output path as the 1st argument, or set ",
         "NCV_ANALYSIS_DIR to the repo's analysis/ directory.", call. = FALSE)
  file.path(d, "data/processed/efda_hex_rates.rds")
}
if (!dir.exists(dirname(out)))
  stop("output directory does not exist: ", dirname(out), call. = FALSE)
f <- list.files("hex_rates", pattern = "[.]rds$", full.names = TRUE)
if (!length(f)) stop("no hex_rates/*.rds yet")
d <- do.call(rbind, lapply(f, readRDS))
need <- c("hex_id","year","natural_pix","forest_pix","bioregion","country","lambda_natural")
if (!all(need %in% names(d))) stop("unexpected columns: ", paste(names(d), collapse = ","))
if (anyNA(d$lambda_natural)) stop("NA lambda_natural in combined hex series")
saveRDS(d, out)
cat(sprintf("combined %d countries, %d hexagons, %d rows -> %s\n",
            length(unique(d$country)), length(unique(d$hex_id)), nrow(d), basename(out)))
