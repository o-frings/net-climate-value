# =============================================================================
# run_figures.R  —  render all manuscript figures + tables from engine outputs
# =============================================================================
# Presentation layer: consumes engine/output/*.csv and writes figures/output/
# (PDF+PNG) + figures/output/tables/ (.tex). Run from analysis/ AFTER the engine:
#   Rscript engine/R/run_engine.R
#   Rscript figures/run_figures.R
# Each figure is one self-contained script following the same legacy aesthetic,
# fed by engine data via _setup_legacy.R (legacy helpers + eng()/save_figure()).
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
cat("=== RENDERING FIGURES + TABLES (from engine/output) ===\n")

source("figures/_setup_legacy.R")   # legacy helpers + engine bridge (theme, colours, eng, save_figure)

figures <- c(
  "fig3.R",                       # headline (4-panel butterfly + MC boxplots + tornado)
  "fig4.R",                       # buffer solvency under climate (2-panel trajectory)
  "fig5.R",                       # CRCF scenario area (boxplots + forward trajectory)
  "ed_sensitivity.R",             # ED1  PRCC heatmap + marginal bar
  "ed_all_practices.R",           # ED2  all-practice cross-biome decomposition
  "ed_practice_spread.R",         # ED   practice buffer spread + disturbance
  "ed_country_disturbance.R",     # ED   country disturbance choropleths
  "ed_jrc.R",                     # ED   per-country JRC comparison
  "ed_parametric.R",              # ED   parametric vs empirical buffer
  "ed_pool_buildup.R",            # ED   buffer diversification vs pool size
  "ed_scheme_gap.R")              # ED   scheme integrity-gap decomposition
for (f in figures) source(file.path("figures", f))

source("figures/tables.R")          # 9 LaTeX tables (self-contained; uses eng())
cat("=== DONE — figures/output/ (PDF+PNG) + figures/output/tables/ (.tex) ===\n")
