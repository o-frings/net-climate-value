# =============================================================================
# 01_setup.R - Package Installation and Configuration
# =============================================================================
cat("Setting up R environment...\n")

# NB: viridis (the wrapper pkg) was removed — it is not used anywhere in the
# pipeline (figures use hardcoded palettes + viridisLite via ggplot2). Keeping
# it here would make the fail-loud check below block users who lack it.
required_packages <- c("tidyverse", "data.table", "scales",
                       "patchwork", "ggrepel", "knitr", "glue", "lubridate",
                       "ggtext", "cowplot")

# Fail loud on a missing package rather than silently installing CRAN HEAD at
# run time (which would make the run depend on the unpinned package versions
# available on the run date — an irreproducibility a journal check would flag).
# Reproducible installs are managed by renv (see analysis/renv_bootstrap.R and
# the committed renv.lock). Only the listed packages are checked here.
.missing_pkgs <- required_packages[!vapply(
  required_packages, function(p) requireNamespace(p, quietly = TRUE), logical(1))]
if (length(.missing_pkgs) > 0) {
  stop("Missing required package(s): ", paste(.missing_pkgs, collapse = ", "),
       ".\n  Reproducible: run renv::restore() to install the PINNED versions ",
       "(see analysis/renv.lock / renv_bootstrap.R).\n  Unpinned (not ",
       "recommended for the canonical run): install.packages(c(",
       paste(sprintf('"%s"', .missing_pkgs), collapse = ", "), ")).",
       call. = FALSE)
}
rm(.missing_pkgs)

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(scales)
})

theme_carbon <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
}
theme_set(theme_carbon())

# BIOME_COLOURS and PATHS are defined in 03_parameters.R (canonical source)
fmt_pct <- function(x, digits = 1) paste0(round(x * 100, digits), "%")
fmt_num <- function(x, digits = 0) format(round(x, digits), big.mark = ",", scientific = FALSE)
clip <- function(x, lower, upper) pmin(pmax(x, lower), upper)

set.seed(42)
cat("Setup complete\n")
