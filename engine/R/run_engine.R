# =============================================================================
# run_engine.R  —  orchestrator for the rebuilt computational engine
# =============================================================================
# The single source of truth for every number the manuscript reports. Reads ONLY
# committed primary data (data/processed/) + sourced parameter CSVs (engine/params/);
# never sources the legacy analysis/R pipeline. Linear, top-to-bottom, fully seeded
# (re-running yields byte-identical engine/output/). Run from analysis/:
#   Rscript engine/R/run_engine.R
#
# Lineage: promoted from analysis/clean/R (the verified clean-room reimplementation,
# which reproduces the published headline within ~2-4%). Extended phase by phase per
# REBUILD_BLUEPRINT.md (P1 MC+parametric, P2 scenarios, P3 schemes+country).
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
cat("=== ENGINE PIPELINE ===\n")
source("engine/R/01_data.R")                 # biome calibration (lambda, severity, S_ref, U)
source("engine/R/02_model.R")                # equations: leakage L, temporality T, x
source("engine/R/03_buffer.R")               # empirical TVaR99 buffer (bootstrap)
source("engine/R/04_headline.R")             # net_share = (1-L)(1-T)(1-b) per practice
source("engine/R/05_temporality_variants.R") # omega: burden vs warming vs benchmark
source("engine/R/06_sensitivity.R")          # deterministic kappa + x robustness sweeps
source("engine/R/07_montecarlo.R")           # Monte Carlo uncertainty + PRCC
source("engine/R/08_scenarios.R")            # CRCF deployment scenarios + NCV area
source("engine/R/09_buffer_trajectory.R")    # empirical TVaR99 buffer trajectory (fig4)
source("engine/R/10_schemes.R")              # cross-scheme integrity gaps (fig3c)
source("engine/R/11_country.R")              # country-level disturbance summary (ED)
source("engine/R/12_pool_buildup.R")         # buffer diversification vs pool size (ED)
source("engine/R/13_jrc.R")                  # per-country comparison vs JRC/Marinelli (ED)
source("engine/R/14_href_sensitivity.R")     # H_ref sensitivity of NCV + scheme gaps (tables)
source("engine/R/15_figure_data.R")          # plot-ready tables (engine computes; figs only plot)
cat("=== ENGINE PIPELINE COMPLETE — outputs in engine/output/ ===\n")
