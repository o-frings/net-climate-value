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

# --- median-draw decomposition (shared by fig3 and ed_all_practices) ----------
# Per draw the decomposition is exact by construction:
#   delta_leak + delta_temp + delta_buf = L + (1-L)T + (1-L)(1-T)b = 1 - net_share.
# Summarising each component by its own median destroys that identity, because the
# median is not linear: the componentwise-median bar neither sums to 1 - net_share nor
# equals the MC median net_share reported in the text (it was out by up to 14 pp).
# Taking one real draw -- the one whose net_share is closest to the cell median --
# restores both properties: the segments add up exactly and the bar total is the
# reported median. Deterministic (with_ties = FALSE over a fixed row order).
median_draw <- function(d, keys) {
  d %>%
    group_by(across(all_of(keys))) %>%
    mutate(.med = median(net_share)) %>%
    slice_min(abs(net_share - .med), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-.med)
}
# Enforce the adding-up identity wherever a decomposition is emitted.
assert_adds_up <- function(d, what) {
  err <- max(abs((d$delta_leak + d$delta_temp + d$delta_buf) - (1 - d$net_share)))
  if (err > 1e-12) stop(what, ": decomposition does not sum to 1 - net_share (", err, ")")
  invisible(err)
}

for (f in sort(list.files("engine/R/figdata", pattern = "\\.R$", full.names = TRUE)))
  source(f)

cat(sprintf("[15_figure_data] OK — %d plot-ready tables in %s/\n",
            length(list.files(FD, pattern = "\\.csv$")), FD))
