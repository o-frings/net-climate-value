# =============================================================================
# 10_schemes.R  —  cross-scheme integrity gaps (fig3c + scheme comparison table)
# =============================================================================
# For each anchor practice, compares each existing scheme's own fixed deductions
#   scheme_net_share = (1 - leakage_rate) * (1 - buffer_rate)
# against the framework's net_share recomputed at THAT scheme's storage horizon
# and region. Faithful port of analysis/R/09_scheme_comparison.R:
#   tau_2_eff = max(practice tau_2, scheme liability midpoint); legally-protected
#     practices use H_perm. WCC uses Temperate_UK biome. CA_USFP excludes the
#     buffer term on BOTH sides (US biome params not applicable).
#   integrity_gap_pct = (scheme_net_share - proposed_net_share) / scheme_net_share
# Per-scheme summary weights = practice_weight x forest_type_weight (scheme_coverage.csv;
# missing -> equal). CA_USFP excluded from the figure summary (text only).
# Reuses engine primitives (leakage_L, temporality_T, calc_x_afforestation from 02)
# and biome_buffer.csv (04). Assumes 02_model.R + 04_headline.R have run.
# =============================================================================
cat("[10_schemes] cross-scheme integrity gaps...\n")

schemes   <- read.csv("engine/params/schemes.csv", stringsAsFactors = FALSE)
coverage  <- read.csv("engine/params/scheme_coverage.csv", stringsAsFactors = FALSE)
practices <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
bbuf      <- read.csv("engine/output/biome_buffer.csv", stringsAsFactors = FALSE)
H_perm    <- .const("H_perm")
anchors   <- practices[as.logical(practices$is_anchor), ]

# biome buffer lookup for a practice row at a horizon (JRC-scope mature-disturbance
# buffer; the afforestation establishment-risk floor was removed — see 04_headline.R).
buffer_for <- function(practice, biome, ft, protected) {
  bb <- bbuf[bbuf$biome == biome & bbuf$forest_type == ft, ]
  if (nrow(bb) != 1) stop("no biome_buffer for ", biome, "/", ft)
  if (protected) bb$b_H100 else bb$b_H40
}

# framework deductions recomputed at a scheme's horizon + region (central params,
# k0 = r-g = 0.01). Returns L/T/b components + net_share (net_share excludes the
# buffer term when exclude_buffer, matching 09_scheme_comparison.R).
proposed_ns <- function(row, biome_ovr, liab_mid, exclude_buffer) {
  biome <- if (nzchar(biome_ovr)) biome_ovr else row$biome
  protected <- isTRUE(as.logical(row$legally_protected))
  tau2 <- if (protected) H_perm else max(row$tau_2, liab_mid)
  x <- if (!is.na(row$T_rot_silv))
         calc_x_afforestation(tau2, row$T_thin_silv, row$T_rot_silv, row$f_thin_silv)
       else row$harvest_displacement
  L <- leakage_L(row$practice, biome, x)
  T <- temporality_T(tau2)                       # H_ref = Inf (headline benchmark)
  b <- buffer_for(row$practice, biome, row$forest_type, protected)
  ns <- if (exclude_buffer) (1 - L) * (1 - T) else (1 - L) * (1 - T) * (1 - b)
  list(L = L, T = T, b = b, ns = ns)
}

# --- per (scheme, covered practice anchor) gap --------------------------------
rows <- list()
for (k in seq_len(nrow(coverage))) {
  cv <- coverage[k, ]; sc <- schemes[schemes$scheme_id == cv$scheme_id, ]
  apr <- anchors[anchors$practice == cv$practice, ]
  if (nrow(apr) == 0) next                       # scheme practice not an engine anchor
  liab_mid <- (sc$liability_years_min + sc$liability_years) / 2
  for (j in seq_len(nrow(apr))) {                # broadleaf/conifer anchor variants
    row <- apr[j, ]
    eb <- isTRUE(sc$exclude_buffer)
    # scheme own deductions (T_s = 0: no scheme prices temporality)
    L_s <- sc$leakage_rate; T_s <- 0; b_s <- sc$buffer_rate
    scheme_ns <- if (eb) (1 - L_s) else (1 - L_s) * (1 - b_s)
    pd <- proposed_ns(row, sc$regional_override, liab_mid, eb)
    prop_ns <- pd$ns
    # channel decomposition (sequential L -> T -> b); for exclude_buffer set
    # b_prop_eff = b_s so delta_b = 0. Deltas sum to (scheme_net - proposed_net).
    b_prop_eff <- if (eb) b_s else pd$b
    scheme_net   <- (1 - L_s) * (1 - T_s) * (1 - b_s)
    proposed_net <- (1 - pd$L) * (1 - pd$T) * (1 - b_prop_eff)
    delta_L <- scheme_net * (pd$L - L_s) / (1 - L_s)
    delta_T <- (1 - pd$L) * (1 - b_s) * (pd$T - T_s)
    delta_b <- (1 - pd$L) * (1 - pd$T) * (b_prop_eff - b_s)
    ft <- row$forest_type
    ftw <- if (ft == "broadleaf") cv$ft_broadleaf else cv$ft_conifer
    rows[[length(rows) + 1]] <- data.frame(
      scheme = cv$scheme_id, scheme_name = sc$name, practice = cv$practice,
      biome = row$biome, species = row$species, forest_type = ft,
      scheme_net_share = scheme_ns, proposed_net_share = prop_ns,
      integrity_gap_pp = scheme_ns - prop_ns,
      integrity_gap_pct = (scheme_ns - prop_ns) / scheme_ns,
      L_prop = pd$L, T_prop = pd$T, b_prop = pd$b,
      scheme_net = scheme_net, proposed_net = proposed_net,
      delta_L = delta_L, delta_T = delta_T, delta_b = delta_b,
      practice_weight = if (is.na(cv$practice_weight)) 1 else cv$practice_weight,
      ft_weight = if (is.na(ftw)) 1 else ftw,
      exclude_figures = isTRUE(sc$exclude_figures), stringsAsFactors = FALSE)
  }
}
scheme_gaps <- do.call(rbind, rows); rownames(scheme_gaps) <- NULL
scheme_gaps$joint_weight <- scheme_gaps$practice_weight * scheme_gaps$ft_weight
# decomposition identity: deltas sum to (scheme_net - proposed_net) per cell
.dchk <- with(scheme_gaps,
              max(abs((delta_L + delta_T + delta_b) - (scheme_net - proposed_net))))
if (.dchk > 1e-10) stop("scheme gap decomposition identity broken: ", .dchk)
write.csv(scheme_gaps, "engine/output/scheme_gaps.csv", row.names = FALSE)

# --- per-draw integrity-gap MC (restores fig3c boxplot distribution) ----------
# Legacy pert-adjustment method (15_integrity_gap_sensitivity.R): perturb each
# scheme's central proposed net by the practice's per-draw deviation from its
# headline central, then weight across covered cells. Engine MC draws (07) supply
# the per-practice deviations; scheme credited shares stay fixed.
.mc   <- readRDS("engine/output/mc_results.rds")
.hl   <- read.csv("engine/output/clean_headline.csv", stringsAsFactors = FALSE)
.ck   <- function(p, b, s) paste(p, b, s, sep = "\r")
.netc <- setNames(.hl$net_share, .ck(.hl$practice, .hl$biome, .hl$species))
.mc$cell <- .ck(.mc$practice, .mc$biome, .mc$species)
.draws <- lapply(split(.mc[, c("iteration", "net_share")], .mc$cell),
                 function(d) d$net_share[order(d$iteration)])
.niter <- max(.mc$iteration)
.fg <- scheme_gaps[!scheme_gaps$exclude_figures, ]
.fg$cell <- .ck(.fg$practice, .fg$biome, .fg$species)
scheme_gap_mc <- do.call(rbind, lapply(unique(.fg$scheme), function(sc) {
  sg <- .fg[.fg$scheme == sc, ]; sw <- sum(sg$joint_weight)
  acc <- numeric(.niter)
  for (k in seq_len(nrow(sg))) {
    nd <- .draws[[sg$cell[k]]]; if (is.null(nd)) next
    pert <- nd - .netc[[sg$cell[k]]]
    net_adj <- sg$proposed_net_share[k] + pert
    acc <- acc + sg$joint_weight[k] * (sg$scheme_net_share[k] - net_adj) / sg$scheme_net_share[k]
  }
  data.frame(scheme = sc, scheme_name = sg$scheme_name[1],
             iteration = seq_len(.niter), gap = acc / sw, stringsAsFactors = FALSE)
}))
write.csv(scheme_gap_mc, "engine/output/scheme_gap_mc.csv", row.names = FALSE)

# channel decomposition: forest-area/weight-aggregated deltas per scheme (ED figure)
scheme_decomp <- do.call(rbind, lapply(split(scheme_gaps, scheme_gaps$scheme), function(d) {
  w <- d$joint_weight; if (all(w == 0)) w <- rep(1, nrow(d))
  wm <- function(x) sum(w * x) / sum(w)
  scheme_net <- wm(d$scheme_net); proposed_net <- wm(d$proposed_net)
  data.frame(scheme = d$scheme[1], scheme_name = d$scheme_name[1],
             delta_L = wm(d$delta_L), delta_T = wm(d$delta_T), delta_b = wm(d$delta_b),
             scheme_net = scheme_net, proposed_net = proposed_net,
             pct_gap = (scheme_net - proposed_net) / scheme_net,  # ed scheme-gap stack + label
             stringsAsFactors = FALSE)
}))
rownames(scheme_decomp) <- NULL
write.csv(scheme_decomp, "engine/output/scheme_gap_decomposition.csv", row.names = FALSE)

# --- per-scheme weighted summary (EU schemes; CA_USFP excluded from figure) ----
wmean <- function(x, w) { ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_); sum(x[ok] * w[ok]) / sum(w[ok]) }
fig <- scheme_gaps[!scheme_gaps$exclude_figures, ]
scheme_summary <- do.call(rbind, lapply(split(fig, fig$scheme), function(d) data.frame(
  scheme = d$scheme[1], scheme_name = d$scheme_name[1],
  n_practices = length(unique(d$practice)),
  mean_net_share = wmean(d$scheme_net_share, d$joint_weight),
  mean_integrity_gap = wmean(d$integrity_gap_pct, d$joint_weight),
  stringsAsFactors = FALSE)))
scheme_summary <- scheme_summary[order(scheme_summary$mean_net_share), ]
rownames(scheme_summary) <- NULL
write.csv(scheme_summary, "engine/output/scheme_summary.csv", row.names = FALSE)

cat(sprintf("[10_schemes] OK — %d (scheme x practice) cells; mean integrity gap %.0f-%.0f%% across %d EU schemes\n",
            nrow(scheme_gaps), 100*min(scheme_summary$mean_integrity_gap),
            100*max(scheme_summary$mean_integrity_gap), nrow(scheme_summary)))
