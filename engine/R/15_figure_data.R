# =============================================================================
# 15_figure_data.R  —  plot-ready tables for every figure
# =============================================================================
# The engine is the single source of truth: ALL figure calculations (medians,
# quantiles, decomposition, smoothing, scaling, ordering, labels) happen HERE and
# are written to engine/output/figdata/. Figure scripts only read these and draw.
# Runs last in run_engine.R (after all analysis modules). Each per-figure fragment
# in engine/R/figdata/ may assume in scope: mc_results, eng(), wfd(), the label
# helpers from _labels.R, and dplyr/tidyr.
# =============================================================================
suppressWarnings(suppressMessages({ library(dplyr); library(tidyr) }))
source("engine/R/_labels.R")
cat("[15_figure_data] building plot-ready tables...\n")

mc_results <- readRDS("engine/output/mc_results.rds")
eng <- function(f) read.csv(file.path("engine/output", f), stringsAsFactors = FALSE)
FD  <- "engine/output/figdata"
dir.create(FD, recursive = TRUE, showWarnings = FALSE)
wfd <- function(df, name) write.csv(df, file.path(FD, paste0(name, ".csv")), row.names = FALSE)

for (f in sort(list.files("engine/R/figdata", pattern = "\\.R$", full.names = TRUE)))
  source(f)

cat(sprintf("[15_figure_data] OK — %d plot-ready tables in %s/\n",
            length(list.files(FD, pattern = "\\.csv$")), FD))
