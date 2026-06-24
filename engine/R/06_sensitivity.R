# =============================================================================
# 06_sensitivity.R  —  deterministic leakage (kappa) and harvest (x) sweeps
# =============================================================================
# Robustness sweeps that feed the manuscript leakage- and x-sensitivity tables.
# Both reuse the engine's already-validated deterministic core (rho_rep from
# 02_model; temporality T and buffer b from the headline) and vary ONE lever:
#   kappa sweep : L(kappa) = clip(kappa * rho_rep * x, -ell_max, 1)  [x<=0 -> cap]
#   x sweep     : scale the harvest-displacement x; static x scaled directly,
#                 dynamic (afforestation) x scaled via its harvest fraction.
# net_share = (1-L)(1-T)(1-b) recomputed at each level. T and b are held at their
# headline (central) values per practice row (joined from clean_headline.csv).
# Assumes 02_model.R and 04_headline.R have run (run_engine.R sources in order).
# =============================================================================
cat("[06_sensitivity] kappa + x deterministic sweeps...\n")

practices <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
headline  <- read.csv("engine/output/clean_headline.csv", stringsAsFactors = FALSE)
sweeps    <- read.csv("engine/params/sensitivity_sweeps.csv", stringsAsFactors = FALSE)
ell_max   <- .const("ell_max")                       # from 02_model.R
kappa_levels <- sweeps$level[sweeps$sweep == "kappa"]
x_levels     <- sweeps$level[sweeps$sweep == "x"]

# headline T and b per (practice, biome, species) — the central values held fixed
key <- function(p, b, s) paste(p, b, s, sep = "\r")
h_T <- setNames(headline$T, key(headline$practice, headline$biome, headline$species))
h_b <- setNames(headline$b, key(headline$practice, headline$biome, headline$species))

# leakage at an arbitrary kappa, reusing the engine rho_rep + the clip/cap rule
leakage_at <- function(practice, biome, x, kappa) {
  L <- min(max(kappa * rho_rep(practice, biome) * x, -ell_max), 1)  # 02_model rho_rep
  if (x <= 0) L <- min(L, ell_max)
  L
}

# --- kappa sweep --------------------------------------------------------------
kappa_sweep <- do.call(rbind, lapply(seq_len(nrow(practices)), function(i) {
  row <- practices[i, ]
  k   <- key(row$practice, row$biome, row$species)
  Tval <- h_T[[k]]; bval <- h_b[[k]]
  if (is.null(Tval) || is.null(bval)) stop("no headline T/b for ", k)
  x <- resolve_x(row)                                # 02_model (dynamic for afforestation)
  do.call(rbind, lapply(kappa_levels, function(kp) {
    L <- leakage_at(row$practice, row$biome, x, kp)
    data.frame(practice = row$practice, biome = row$biome, species = row$species,
               is_anchor = as.logical(row$is_anchor), kappa = kp,
               L = L, net_share = net_share(L, Tval, bval), stringsAsFactors = FALSE)
  }))
}))
rownames(kappa_sweep) <- NULL

# range summary per practice x biome (central = manuscript central kappa = 0.60)
kc <- 0.60
leak_range <- do.call(rbind, by(kappa_sweep, list(kappa_sweep$practice, kappa_sweep$biome), function(d) {
  if (nrow(d) == 0) return(NULL)
  d1 <- d[!duplicated(d$kappa), ]                    # collapse species variants
  data.frame(practice = d1$practice[1], biome = d1$biome[1],
             is_anchor = d1$is_anchor[1],
             L_low = min(d1$L), L_central = d1$L[d1$kappa == kc], L_high = max(d1$L),
             ns_low = min(d1$net_share), ns_central = d1$net_share[d1$kappa == kc],
             ns_high = max(d1$net_share), ns_range = max(d1$net_share) - min(d1$net_share),
             stringsAsFactors = FALSE)
}))
rownames(leak_range) <- NULL
leak_range <- leak_range[order(-leak_range$ns_range), ]

write.csv(kappa_sweep, "engine/output/leakage_sensitivity_sweep.csv", row.names = FALSE)
write.csv(leak_range,  "engine/output/leakage_sensitivity_range.csv", row.names = FALSE)
cat(sprintf("[06_sensitivity] kappa sweep: %d rows (%d practice-biome ranges)\n",
            nrow(kappa_sweep), nrow(leak_range)))

# --- rank robustness across the kappa range (manuscript: Sensitivity & robustness) ---
# Spearman rank correlation of practice net_share between the lowest and highest kappa
# in the sweep (general-equilibrium vs partial-equilibrium leakage). Reported for the
# 16 headline practices (anchor biome) and for all practice x biome combinations.
.ks_rank <- function(rows) {
  d <- aggregate(net_share ~ practice + biome + kappa, rows, mean)   # collapse species
  klo <- min(d$kappa); khi <- max(d$kappa)
  m <- merge(d[d$kappa == klo, c("practice", "biome", "net_share")],
             d[d$kappa == khi, c("practice", "biome", "net_share")],
             by = c("practice", "biome"), suffixes = c("_lo", "_hi"))
  list(rho = cor(m$net_share_lo, m$net_share_hi, method = "spearman"), n = nrow(m))
}
ra <- .ks_rank(kappa_sweep[kappa_sweep$is_anchor, ])
rb <- .ks_rank(kappa_sweep)
write.csv(data.frame(set = c("headline_practices", "all_practice_biome"),
                     n = c(ra$n, rb$n), spearman_kappa_lo_hi = c(ra$rho, rb$rho)),
          "engine/output/rank_robustness_kappa.csv", row.names = FALSE)
cat(sprintf("[06_sensitivity] kappa rank-robustness: %d headline practices rho=%.3f; %d combos rho=%.3f\n",
            ra$n, ra$rho, rb$n, rb$rho))

# --- x sweep ------------------------------------------------------------------
# Static x: scale harvest_displacement directly (clamped at 1). Dynamic
# (afforestation, T_rot_silv set): scale the harvest FRACTION f_h, then x=-f_h/(1-f_h).
x_at <- function(row, mult) {
  if (!is.na(row$T_rot_silv)) {
    x0 <- calc_x_afforestation(row$tau_2, row$T_thin_silv, row$T_rot_silv, row$f_thin_silv)
    f0 <- -x0 / (1 - x0)                             # invert x=-f/(1-f)
    f  <- min(f0 * mult, 0.99)
    return(-(f / (1 - f)))
  }
  hd <- row$harvest_displacement
  if (hd > 0) return(min(hd * mult, 1.0))
  if (hd < 0) return(hd * mult)
  0
}

x_sweep <- do.call(rbind, lapply(seq_len(nrow(practices)), function(i) {
  row <- practices[i, ]
  k   <- key(row$practice, row$biome, row$species)
  Tval <- h_T[[k]]; bval <- h_b[[k]]
  do.call(rbind, lapply(x_levels, function(m) {
    x <- x_at(row, m)
    L <- leakage_at(row$practice, row$biome, x, .const("kappa"))
    data.frame(practice = row$practice, biome = row$biome, species = row$species,
               is_anchor = as.logical(row$is_anchor), x_mult = m, x_effective = x,
               L = L, net_share = net_share(L, Tval, bval), stringsAsFactors = FALSE)
  }))
}))
rownames(x_sweep) <- NULL

x_range <- do.call(rbind, by(x_sweep, list(x_sweep$practice, x_sweep$biome), function(d) {
  if (nrow(d) == 0) return(NULL)
  d1 <- d[!duplicated(d$x_mult), ]
  data.frame(practice = d1$practice[1], biome = d1$biome[1], is_anchor = d1$is_anchor[1],
             x_base = d1$x_effective[d1$x_mult == 1.00],
             ns_low = min(d1$net_share), ns_central = d1$net_share[d1$x_mult == 1.00],
             ns_high = max(d1$net_share), ns_range = max(d1$net_share) - min(d1$net_share),
             stringsAsFactors = FALSE)
}))
rownames(x_range) <- NULL
x_range <- x_range[order(-x_range$ns_range), ]

write.csv(x_sweep, "engine/output/x_sensitivity_sweep.csv", row.names = FALSE)
write.csv(x_range, "engine/output/x_sensitivity_range.csv", row.names = FALSE)
cat(sprintf("[06_sensitivity] x sweep: %d rows (%d practice-biome ranges)\n",
            nrow(x_sweep), nrow(x_range)))
