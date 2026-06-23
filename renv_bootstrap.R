#!/usr/bin/env Rscript
# =============================================================================
# renv_bootstrap.R — pin the package environment for reproducible runs
# =============================================================================
# Run ONCE (from analysis/) to create a committed renv.lock that records the
# exact package versions used to produce the published numbers. Thereafter any
# third party reproduces the environment with `renv::restore()` before running
# R/00_master.R. This closes the NCC editorial reproducibility requirement that
# the current pipeline fails (no version pinning; runtime install.packages()).
#
#   Rscript renv_bootstrap.R          # init + snapshot -> renv.lock
#
# After this, commit renv.lock (and the renv/ activate scaffolding).
# =============================================================================
# Run from this script's own directory (analysis/), so it works whether invoked
# as `Rscript renv_bootstrap.R` (from analysis/) or `Rscript
# analysis/renv_bootstrap.R` (from the project root). renv is initialised here,
# the same directory R/00_master.R runs from.
.file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (length(.file) == 1 && nzchar(.file)) {
  setwd(dirname(normalizePath(.file)))
} else if (requireNamespace("this.path", quietly = TRUE)) {
  setwd(dirname(this.path::this.path()))
}
if (basename(getwd()) != "analysis")
  stop("Run from the 'analysis/' directory (auto-locate failed). ",
       "cd into analysis/ and retry: Rscript renv_bootstrap.R")

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org/")
}

# Packages the pipeline depends on (mirror of 01_setup.R + extraction/optional).
pkgs <- c(
  # core (01_setup.R)  [viridis intentionally excluded: not used by the pipeline]
  "tidyverse", "data.table", "scales", "patchwork", "ggrepel",
  "knitr", "glue", "lubridate", "ggtext", "cowplot",
  # used across the pipeline
  "dplyr", "tidyr", "forcats", "ggplot2", "readr", "tibble", "purrr",
  # data re-extraction (REEXTRACT_DATA = TRUE only) + offline EFDA scripts
  "sf", "rnaturalearth", "rnaturalearthdata", "terra", "readxl"
)

# Initialise renv for the project (idempotent if already initialised), then
# HYDRATE: renv scans the pipeline's R code for its dependencies and copies the
# already-installed versions from your user/system libraries into the project
# library (no re-download). hydrate() reads your REAL libraries even when the
# (possibly empty) renv library is active — which is why we rely on it rather
# than checking requireNamespace() after init (that sees only the empty renv
# library and would pin nothing). snapshot() then writes them into renv.lock.
renv::init(bare = TRUE, restart = FALSE)
renv::hydrate()
renv::snapshot(prompt = FALSE)

# Sanity: with the project library now populated, report coverage of the
# expected pipeline packages so an empty/partial lock is obvious.
.have <- pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
cat(sprintf("\nProject library now holds %d/%d expected pipeline packages.\n",
            length(.have), length(pkgs)))
.miss <- setdiff(pkgs, .have)
if (length(.miss) > 0)
  message("Not locked (install + re-run if the pipeline needs them; ",
          "sf/terra/rnaturalearth/rnaturalearthdata/readxl are REEXTRACT_DATA=",
          "TRUE / offline-extraction only): ", paste(.miss, collapse = ", "))

cat("\nrenv.lock written. Commit it. Reproduce with: renv::restore()\n")
cat("R version recorded:", R.version.string, "\n")
