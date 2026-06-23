# =============================================================================
# figures/_setup_legacy.R — legacy helpers + engine-data bridge
# =============================================================================
# Sources the validated legacy helpers (theme_nature, practice_full_label,
# is_secondary_variant, NATURE_* colours, SCHEME_PARAMS, forest-type/biome
# abbreviations) from prep/, then points figure output at figures/output and
# loads the engine's MC results. The figure scripts are faithful ports of the
# legacy aesthetic (17/19/22/27) fed by engine outputs — same look, engine
# numbers. Run from analysis/.
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
# Globals the 03_parameters monolith expects (same as build_scheme_csvs.R).
ACTIVE_SCENARIO <- "mixed_reducing"; CRCF_TARGET_MT <- 100
QUICK_MODE <- TRUE; N_ITERATIONS <- 1000; REEXTRACT_DATA <- FALSE
suppressWarnings(suppressMessages({
  source("prep/01_setup.R")
  invisible(capture.output({
    source("prep/03_parameters.R"); source("prep/04_functions.R")
  }))
}))
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr)
  library(patchwork); library(ggnewscale); library(ggtext)
})

FIG_OUT <- "figures/output"
dir.create(FIG_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FIG_OUT, "tables"), recursive = TRUE, showWarnings = FALSE)

.has_cairo <- tryCatch({
  f <- tempfile(fileext = ".pdf")
  ggsave(f, ggplot() + geom_blank(), device = grDevices::cairo_pdf, width = 1, height = 1)
  unlink(f); TRUE
}, error = function(e) FALSE, warning = function(w) FALSE)

# Override the legacy save_figure to write into figures/output (not the prep
# scratch output/figures), keeping the same cairo_pdf device for font embedding.
save_figure <- function(plot, name, width = 180, height = 120) {
  pdf_path <- file.path(FIG_OUT, paste0(name, ".pdf"))
  png_path <- file.path(FIG_OUT, paste0(name, ".png"))
  tryCatch(suppressWarnings(ggsave(pdf_path, plot, width = width, height = height,
           units = "mm", device = if (.has_cairo) grDevices::cairo_pdf else "pdf")),
           error = function(e) cat(sprintf("  PDF fail (%s): %s\n", name, conditionMessage(e))))
  ok <- tryCatch({ ggsave(png_path, plot, width = width, height = height, units = "mm", dpi = 300); TRUE },
           error = function(e) { cat(sprintf("  PNG fail (%s): %s\n", name, conditionMessage(e))); FALSE })
  if (ok) cat(sprintf("  saved %s (%gx%g mm)\n", name, width, height))
}

eng <- function(f) read.csv(file.path("engine/output", f), stringsAsFactors = FALSE)
# plot-ready table reader: figures read ONLY these engine-computed tables and draw.
fd <- function(name) read.csv(file.path("engine/output/figdata", paste0(name, ".csv")),
                              stringsAsFactors = FALSE)
