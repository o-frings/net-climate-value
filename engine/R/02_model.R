# =============================================================================
# clean_02_model.R  —  core deduction model (manuscript equations)
# =============================================================================
# Pure functions re-coded directly from the manuscript Methods equations.
# Reads parameters only from the sourced CSVs in engine/params/ (no literals,
# no fallbacks). Equations:
#   Leakage   (Eq rho_rep, L_mkt, L_total):
#       rho = sum_p w_p * [u*es_dom + (1-u)*s*es_imp] / [|ed| + u*es_dom + (1-u)*s*es_imp]
#       L   = clip(kappa * rho * x, -ell_max, 1);  if x<=0: L = min(L, ell_max)
#   Temporality (Eq T_temp, H_ref->Inf benchmark):
#       T   = 1 - omega,  omega = [(1-e^{-rate*Δτ})/rate] / [(1-e^{-k0*H_ref})/k0],
#             rate = k0 + phi_add,  k0 = r - g
#   Issuance  (Eq issuance_rule):  net_share = (1-L)(1-T)(1-b)
# =============================================================================

# --- load sourced parameters --------------------------------------------------
.pp <- function(f) { p <- file.path("engine/params", f)
  if (!file.exists(p)) stop("MISSING param CSV: ", p); read.csv(p, stringsAsFactors = FALSE) }
.MC_df  <- .pp("model_constants.csv")
MC <- setNames(.MC_df$value, .MC_df$name)            # model constants
LE  <- .pp("leakage_elasticities.csv")               # biome x product elasticities
PPW <- .pp("practice_product_weights.csv")           # practice -> SL/PW/WF weights

.const <- function(nm) { v <- MC[[nm]]
  if (is.null(v) || is.na(v)) stop("model constant not found: ", nm); unname(v) }

# --- leakage ------------------------------------------------------------------
# Single product-class replacement ratio (manuscript Eq rho_rep).
rho_one <- function(u, s, eps_d, eps_s_dom, eps_s_imp) {
  num <- u * eps_s_dom + (1 - u) * s * eps_s_imp
  num / (abs(eps_d) + num)
}
# Practice replacement ratio = product-weighted average over SL/PW/WF.
rho_rep <- function(practice, biome) {
  w <- PPW[PPW$practice == practice, c("SL", "PW", "WF")]
  if (nrow(w) != 1) stop("no product weights for practice: ", practice)
  if (sum(w) == 0) return(0)                          # harvest-neutral practices
  tot <- 0
  for (pc in c("SL", "PW", "WF")) {
    if (w[[pc]] == 0) next
    e <- LE[LE$biome == biome & LE$product == pc, ]
    if (nrow(e) != 1) stop("no elasticities for ", biome, " x ", pc)
    tot <- tot + w[[pc]] * rho_one(e$u, e$s, e$eps_d, e$eps_s_dom, e$eps_s_imp)
  }
  tot
}
# Total leakage L (manuscript Eq L_mkt + L_total).
leakage_L <- function(practice, biome, x) {
  kappa <- .const("kappa"); ell_max <- .const("ell_max")
  L <- pmin(pmax(kappa * rho_rep(practice, biome) * x, -ell_max), 1)
  if (x <= 0) L <- min(L, ell_max)                    # afforestation cap (Murray 2004)
  L
}

# --- harvest displacement x ---------------------------------------------------
# Dynamic x = -H/Q for zero-baseline (afforestation/short-rotation) regimes
# (manuscript: x = -H/Q). Static x otherwise.
calc_x_afforestation <- function(T_project, T_thin, T_rot, f_thin) {
  if (T_project <= 0) return(0)
  thin_end   <- min(T_project, T_rot)
  thin_years <- max(thin_end - T_thin, 0)
  f_h <- f_thin * thin_years / T_project
  if (T_project > T_rot) {
    n_clearfell <- floor((T_project - 1) / T_rot)
    f_h <- f_h + (1 - f_thin) * n_clearfell * T_rot / T_project
  }
  f_h <- min(f_h, 0.99)
  -(f_h / (1 - f_h))
}
# Resolve x for a practice row from practices.csv.
resolve_x <- function(row) {
  if (!is.na(row$T_rot_silv)) {
    return(calc_x_afforestation(row$tau_2, row$T_thin_silv, row$T_rot_silv, row$f_thin_silv))
  }
  row$harvest_displacement
}

# --- temporality --------------------------------------------------------------
# T = 1 - omega, with k0 = r - g and the H_ref->Inf permanence benchmark
# (Inf is passed through exactly: exp(-k0*Inf) = 0). phi_add = 0 (headline).
temporality_T <- function(tau_2, phi_add = 0, H_ref = Inf) {
  k0 <- .const("r") - .const("g")
  if (k0 <= 0) k0 <- 0.001
  rate  <- k0 + phi_add
  tau_1 <- .const("tau_1")
  num   <- (1 - exp(-rate * (tau_2 - tau_1))) / rate
  denom <- (1 - exp(-k0 * H_ref)) / k0
  1 - pmin(pmax(num / denom, 0), 1)
}
# Storage horizon for temporality: legally-protected (afforestation) -> H_perm.
tau_2_temporality <- function(row) {
  if (isTRUE(as.logical(row$legally_protected))) .const("H_perm") else row$tau_2
}

# --- issuance -----------------------------------------------------------------
net_share <- function(L, T, b) (1 - L) * (1 - T) * (1 - b)

cat("[clean_02_model] equations loaded (leakage L, temporality T, issuance).\n")
