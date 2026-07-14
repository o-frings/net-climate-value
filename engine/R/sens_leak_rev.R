# =============================================================================
# sens_leak_rev.R  —  SI sensitivity: NCV vs leakage-reversal copula correlation
# =============================================================================
# Re-runs the Monte Carlo core (07) across a grid of Gaussian-copula correlations
# LEAK_REV_RHO between the leakage driver (kappa) and the reversal driver
# (lambda_mult). rho = 0 is the independence baseline the headline uses; off-
# baseline runs leave canonical outputs untouched (WRITE_CANONICAL guard in 07).
# Writes engine/output/sens_leak_rev_corr.csv. Run from analysis/:
#   Rscript engine/R/sens_leak_rev.R
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
source("engine/R/01_data.R")     # biome calibration
source("engine/R/02_model.R")    # leakage/temporality equations, resolve_x, PPW, LE
source("engine/R/03_buffer.R")   # empirical buffer
source("engine/R/04_headline.R") # clean_headline.csv, biome_buffer.csv

rhos <- c(-0.5, -0.3, 0, 0.3, 0.5)
rows <- lapply(rhos, function(rho) {
  Sys.setenv(MC_LEAK_REV_RHO = as.character(rho))
  source("engine/R/07_montecarlo.R")          # defines mc_summary for this rho
  a <- mc_summary[as.logical(mc_summary$is_anchor), ]
  pp <- aggregate(p50_share ~ practice, a, mean) # collapse biome/species per practice
  data.frame(rho = rho, ncv_median16 = median(pp$p50_share)) # manuscript headline stat
})
out <- do.call(rbind, rows)
out$delta_vs_indep <- out$ncv_median16 - out$ncv_median16[out$rho == 0]
write.csv(out, "engine/output/sens_leak_rev_corr.csv", row.names = FALSE)
cat("[sens_leak_rev] baseline headline NCV", round(100 * out$ncv_median16[out$rho == 0], 2),
    "%; max shift", round(100 * max(abs(out$delta_vs_indep)), 2), "pp\n")
