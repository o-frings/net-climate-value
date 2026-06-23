# =============================================================================
# 05_temporality_variants.R  —  temporality weight under alternative metrics
# =============================================================================
# Tests whether the temporality weight omega = 1 - T (and hence the net_share
# ranking) depends on (a) the climate-impact metric (CO2 burden vs warming) and
# (b) the permanence benchmark — by computing omega three ways and re-ranking.
#
#   omega_disc(H)  = 1 - e^{-k0 H}                       (current; burden, H_ref->Inf)
#   omega_T(H)     = warming-based (Joos g * AR5 thermal R), discounted, numeric
#   omega_b(H,Href)= burden, finite permanence benchmark Href
#
# Reads only param CSVs + clean_headline. Assumes 01_data/02_model context not
# needed (self-contained). Run after run_clean.R (needs clean_headline.csv).
# =============================================================================
cat("[05_temporality] omega under burden / warming / benchmark variants...\n")

.mc <- read.csv("engine/params/model_constants.csv", stringsAsFactors = FALSE)
MC  <- setNames(.mc$value, .mc$name)
k0    <- MC[["r"]] - MC[["g"]]
Hperm <- MC[["H_perm"]]

# --- impulse responses (published constants) ---------------------------------
# CO2 airborne fraction after a pulse — Joos et al. 2013 (IPCC AR5 AGWP)
joos_g  <- function(s) 0.2173 + 0.2240*exp(-s/394.4) + 0.2824*exp(-s/36.54) + 0.2763*exp(-s/4.304)
# temperature response to unit radiative forcing — IPCC AR5 Table 8.SM.4
therm_R <- function(t) (0.631/8.4)*exp(-t/8.4) + (0.429/409.5)*exp(-t/409.5)

dt <- 0.5; tmax <- 2000; tg <- seq(0, tmax, by = dt)
gv <- joos_g(tg); Rv <- therm_R(tg)
# warming from a unit CO2 pulse at t=0: A(t) = int_0^t g(s) R(t-s) ds
A <- numeric(length(tg))
for (k in seq_along(tg)) A[k] <- dt * sum(gv[seq_len(k)] * Rv[rev(seq_len(k))])
disc <- exp(-k0 * tg)

omega_disc <- function(H) 1 - exp(-k0 * H)                       # burden, H_ref=Inf
omega_b <- function(H, Href = Inf) {                            # burden, finite Href
  den <- if (is.infinite(Href)) 1 else 1 - exp(-k0 * Href)
  min((1 - exp(-k0 * H)) / den, 1)
}
omega_T <- function(H) {                                        # warming-based, numeric
  shift <- round(H / dt)
  A_re  <- c(rep(0, shift), head(A, length(A) - shift))         # warming of pulse re-released at H
  Vperm <- sum(A * disc) * dt
  sum((A - A_re) * disc) * dt / Vperm
}
# Declining (gamma) discount rate — Weitzman 2001 certainty-equivalent factor
# D(t) = (1 + t/beta)^(-alpha), near-term rate alpha/beta = k0. Because it is
# NON-exponential, D(u+H) != D(u) D(H): the carbon decay g(u) no longer cancels,
# so this omega depends explicitly on the impulse response.
ddr_alpha <- 2
ddr_beta  <- ddr_alpha / k0                                     # near-term rate = k0
D_ddr     <- function(t) (1 + t / ddr_beta)^(-ddr_alpha)
omega_ddr <- function(H) 1 - sum(gv * D_ddr(tg + H)) * dt / (sum(gv * D_ddr(tg)) * dt)

# --- per anchor practice ------------------------------------------------------
pr <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
pr <- pr[as.logical(pr$is_anchor), ]
H_of <- function(row) if (isTRUE(as.logical(row$legally_protected))) Hperm else row$tau_2
res <- do.call(rbind, lapply(seq_len(nrow(pr)), function(i) {
  H <- H_of(pr[i, ])
  data.frame(practice = pr$practice[i], biome = pr$biome[i], species = pr$species[i], H = H,
             omega_disc = omega_disc(H), omega_T = omega_T(H), omega_ddr = omega_ddr(H),
             omega_b100 = omega_b(H, 100), omega_b_inf = omega_b(H, Inf),
             stringsAsFactors = FALSE)
}))
write.csv(res, "engine/output/temporality_variants.csv", row.names = FALSE)

cat(sprintf("  omega_T vs omega_disc: max|dev| = %.4f  (metric-invariance: burden vs warming)\n",
            max(abs(res$omega_T - res$omega_disc))))

# --- ranking robustness: net_share under each variant ------------------------
hl <- read.csv("engine/output/clean_headline.csv", stringsAsFactors = FALSE)
hl <- hl[as.logical(hl$is_anchor), c("practice","biome","species","L","b","net_share")]
m  <- merge(hl, res, by = c("practice","biome","species"))
ns <- function(om) (1 - m$L) * om * (1 - m$b)
cat(sprintf("  net_share check (disc vs published): max|dev| = %.4f\n",
            max(abs(ns(m$omega_disc) - m$net_share))))
cat(sprintf("  Spearman(net_share: warming vs burden)        = %.4f\n",
            cor(ns(m$omega_T),    ns(m$omega_disc), method = "spearman")))
cat(sprintf("  Spearman(net_share: H_ref=100yr vs Inf)       = %.4f\n",
            cor(ns(m$omega_b100), ns(m$omega_b_inf), method = "spearman")))
cat(sprintf("  declining-rate (Weitzman) omega vs exp: max|dev| = %.4f (IRF does NOT cancel)\n",
            max(abs(m$omega_ddr - m$omega_disc))))
cat(sprintf("  Spearman(net_share: declining-rate vs exp)    = %.4f\n",
            cor(ns(m$omega_ddr), ns(m$omega_disc), method = "spearman")))
# --- why discounting is required: the undiscounted CO2 impact does not vanish --
# A(t) = warming still present t years after a 1-tonne pulse. It does NOT decay
# to zero; it plateaus at g(Inf) * int_0^Inf R(t) dt -- the persistent airborne
# fraction times the thermal sensitivity. Hence int_0^Inf A(t) dt DIVERGES: an
# undiscounted, infinite-horizon temporality metric is ill-defined. Any finite
# accounting horizon TH (GWP/GTP) truncates this non-zero tail; discounting
# down-weights it without erasing it. The discount rate is therefore the complete
# regulariser, and H_ref -> Inf the only physical permanence benchmark.
a0    <- joos_g(1e6)                       # persistent airborne fraction g(Inf)
intR  <- sum(Rv) * dt                      # int_0^Inf thermal IRF (~1.06)
A_inf <- a0 * intR                         # warming plateau as t -> Inf
A_at  <- function(t) A[which.min(abs(tg - t))]
cat(sprintf("  persistent airborne fraction g(Inf) = %.4f; int R = %.3f -> A(Inf) = %.4f\n",
            a0, intR, A_inf))
for (t in c(100, 500, 1000, 2000))
  cat(sprintf("  warming A(%5dyr) = %.4f  (%.0f%% of A(100yr) -- never reaches 0)\n",
              t, A_at(t), 100 * A_at(t) / A_at(100)))
cat("  => int_0^Inf A(t) dt diverges; no discount-free permanence metric exists,\n     so the discount rate (not a finite horizon) is the proper regulariser.\n")
