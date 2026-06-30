# =============================================================================
# figures/tables.R  —  manuscript \input LaTeX tables from engine outputs
# =============================================================================
# Generates the .tex table fragments the manuscript \input's, from engine/output
# CSVs. Writes to figures/output/tables/. Assumes figures/theme.R sourced (eng()).
# Built incrementally; each block is independent.
# =============================================================================
TBL_OUT <- file.path(FIG_OUT, "tables")
dir.create(TBL_OUT, showWarnings = FALSE, recursive = TRUE)

.esc <- function(x) gsub("#", "\\\\#", gsub("_", "\\\\_",
  gsub("%", "\\\\%", gsub("&", "\\\\&", as.character(x)))))
# minimal booktabs-style LaTeX table writer
write_tex <- function(rows, header, align, caption, label, file, note = NULL) {
  con <- file.path(TBL_OUT, file)
  L <- c("\\begin{table}[H]\\centering",
         sprintf("\\caption{%s}", caption), sprintf("\\label{%s}", label),
         sprintf("\\begin{tabular}{%s}", align), "\\toprule",
         paste(paste(header, collapse = " & "), "\\\\"), "\\midrule",
         paste0(rows, " \\\\"), "\\bottomrule", "\\end{tabular}")
  if (!is.null(note)) L <- c(L, sprintf("\\par\\smallskip\\footnotesize %s", note))
  L <- c(L, "\\end{table}")
  writeLines(L, con); cat(sprintf("  wrote %s (%d rows)\n", file, length(rows)))
}

# --- leakage intensity sensitivity (anchors) ---------------------------------
local({
  d <- eng("leakage_sensitivity_range.csv"); d <- d[as.logical(d$is_anchor), ]
  d <- d[order(-d$ns_range), ]
  rows <- sprintf("%s & %s & %.0f & %.0f--%.0f & %.1f",
    .esc(d$practice), .esc(d$biome), 100 * d$ns_central,
    100 * pmin(d$ns_low, d$ns_high), 100 * pmax(d$ns_low, d$ns_high), 100 * d$ns_range)
  write_tex(rows,
    header = c("Practice", "Biome", "NCV central (\\%)", "NCV range (\\%)", "Spread (pp)"),
    align = "llrrr",
    caption = "\\textbf{Leakage intensity sensitivity.} Net climate value across $\\kappa\\in[0.33,1.27]$; central (mode) $\\kappa=0.60$.",
    label = "tab:leakage_sensitivity", file = "latex_leakage_sensitivity.tex",
    note = "Computed by the engine over the $\\kappa$ sweep (engine/output/leakage\\_sensitivity\\_range.csv).")
})

# --- harvest displacement (x) sensitivity (anchors) --------------------------
local({
  d <- eng("x_sensitivity_range.csv"); d <- d[as.logical(d$is_anchor), ]
  d <- d[order(-d$ns_range), ]
  rows <- sprintf("%s & %s & %.2f & %.0f & %.0f--%.0f & %.1f",
    .esc(d$practice), .esc(d$biome), d$x_base, 100 * d$ns_central,
    100 * pmin(d$ns_low, d$ns_high), 100 * pmax(d$ns_low, d$ns_high), 100 * d$ns_range)
  write_tex(rows,
    header = c("Practice", "Biome", "$x$", "NCV central (\\%)", "NCV range (\\%)", "Spread (pp)"),
    align = "llrrrr",
    caption = "\\textbf{Harvest displacement ($x$) sensitivity.} Net climate value under $\\pm$50\\% variation in $x$.",
    label = "tab:x_sensitivity", file = "latex_x_sensitivity.tex",
    note = "Computed by the engine over the $x$ sweep (engine/output/x\\_sensitivity\\_range.csv).")
})

# --- cross-scheme comparison (FULL S2a table; single-sourced from engine) ----
# Volatile numbers (MC-median gaps, 90% CIs, CA central gap) are computed from
# engine output; the curated descriptive cells (N, buffer/risk, leakage,
# liability, footnotes) are scheme-rulebook facts edited HERE if a scheme's
# rules change. Replaces the previously hardcoded main.tex table so the numbers
# can never drift out of sync with the engine again.
local({
  mc <- eng("scheme_gap_mc.csv"); sg <- eng("scheme_gaps.csv")
  med <- tapply(mc$gap, mc$scheme, function(x) as.numeric(median(x)))
  lo  <- tapply(mc$gap, mc$scheme, function(x) as.numeric(quantile(x, 0.05)))
  hi  <- tapply(mc$gap, mc$scheme, function(x) as.numeric(quantile(x, 0.95)))
  ca  <- sg[sg$scheme == "CA_USFP", ]
  caw <- if (all(ca$joint_weight == 0)) rep(1, nrow(ca)) else ca$joint_weight
  ca_gap <- sum(caw * ca$integrity_gap_pct) / sum(caw)

  meta <- list(
    WKS = list(name = "Wald-Klima-Standard (DE)",  N = "---",             buf = "15\\%$^c$",     leak = "0\\%$^c$",    liab = "20--30"),
    LBC = list(name = "Label Bas-Carbone (FR)",     N = "${\\sim}$1{,}200", buf = "10--25\\%$^a$", leak = "0--5\\%$^b$", liab = "20--30"),
    WCC = list(name = "Woodland Carbon Code (UK)",  N = "845",            buf = "20\\%",         leak = "0\\%",        liab = "40--100$^d$"),
    KSF = list(name = "Klimaskovfonden (DK)",       N = "251",            buf = "15\\%",         leak = "0\\%",        liab = "80--100$^f$"),
    PLC = list(name = "Peatland Code (UK)",         N = "200",            buf = "20\\%",         leak = "0\\%",        liab = "30--100"))
  eu <- names(meta); eu <- eu[order(-med[eu])]                 # descending MC-median gap
  eu_rows <- vapply(eu, function(id) sprintf(
    "    %s & %s & %s & %s & %s & %s & %.0f\\%% \\\\",
    id, meta[[id]]$name, meta[[id]]$N, meta[[id]]$buf, meta[[id]]$leak, meta[[id]]$liab,
    100 * med[id]), character(1))
  ci_str <- paste(sprintf("%s [%.0f, %.0f]", eu, 100 * lo[eu], 100 * hi[eu]), collapse = ", ")

  L <- c(
    "\\begin{table}[H]",
    "\\centering",
    "    \\scriptsize",
    "    \\setlength{\\tabcolsep}{3pt}",
    "    \\caption{\\textbf{Cross-scheme comparison.} Existing European and benchmark schemes compared against the proposed NCV framework. The integrity gap represents potential over-crediting---the difference between scheme issuance and framework-estimated net issuance once leakage, time discounting, and reversal risk are priced. Credit design and market acceptance in Table~\\ref{tab:scheme_credit_design}.}",
    "    \\label{tab:scheme_comparison}",
    "    \\begin{tabular}{L{3.0cm} L{3.5cm} l L{1.5cm} L{1.3cm} L{1.5cm} l}",
    "    \\toprule",
    "    \\textbf{Scheme} & \\textbf{Full name} & $\\boldsymbol{N}$ & \\textbf{Buffer/risk} & \\textbf{Leakage} & \\textbf{Liability (yr)} & \\textbf{Gap}$^h$ \\\\",
    "    \\midrule",
    "    Proposed & Risk-priced framework & --- & Variable & $-$2.3--74\\% & 10--100 & --- \\\\",
    "    \\addlinespace",
    eu_rows,
    "    \\addlinespace",
    sprintf("    CA\\_USFP$^g$ & California USFP (US)    & 172              & 8.7--19.2\\%%$^e$ & $\\leq$20\\%% & 40--100$^d$ & %.0f\\%%$^g$ \\\\", 100 * ca_gap),
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}",
    "    \\scriptsize",
    paste0("    \\item Notes: $N$ = approximate registered projects. Buffer/risk: WCC, PLC, WKS, KSF use pooled buffers; LBC and CA\\_USFP differ as noted. $^a$LBC applies project-level risk discounts (10\\% general + 0--15\\% fire zone uplift), not a pooled buffer (\\citealt{LabelBasCarbone_2025con}). $^b$GFSC applies 5\\% leakage (\\emph{fuite}) plus 10\\% general risk, 0--15\\% data uncertainty, and 0--30\\% fire risk; Boisement/Reconstitution/Balivage apply 10--25\\% risk discounts plus a 40\\% additionality penalty if BASI calculations are not performed---none addresses market leakage. $^c$Flat 15\\% permanence-buffer base for all methods (\\citealt{WaldKlimaStandard_2024}); the 5\\% leakage rate applies only to a reduced-harvest method (M03) not modelled here, so the afforestation and conversion methods carry 0\\% leakage. $^d$WCC: $\\text{Beta}(2,3)$ on $[40, 100]$, median~63~yr (\\citealt{ForestCarbon_2025}); CA\\_USFP: nominal 100~yr, effective range reflects near-depleted buffer pool (\\citealt{Badgley_2022b}). $^e$Risk-rated per project; 13.4\\% programme average; pool ${\\sim}$95\\% depleted (VROD v2025-12; \\citealt{Haya_2025_VROD}). $^f$Nominal permanent (fredskovspligt); Beta(5,1) on [80,100] in MC. $^g$Leakage and temporality only (buffer excluded: no US biome calibration); CA\\_USFP is the central-parameter gap, excluded from the Monte Carlo figure ensemble, so no MC interval. $^h$MC medians ($n = 10{,}000$); gap = (scheme $-$ proposed) / scheme. 90\\% CI: ", ci_str, ". Sorted by descending gap."),
    "    \\end{tablenotes}",
    "\\end{table}")
  writeLines(L, file.path(TBL_OUT, "latex_scheme_comparison.tex"))
  cat(sprintf("  wrote latex_scheme_comparison.tex (full S2a; %d EU rows + CA)\n", length(eu_rows)))
})

# --- temporality comparison: T deduction by method --------------------------
local({
  d <- eng("temporality_variants.csv")
  d <- d[!duplicated(paste(d$practice, d$biome)), ]   # one species per practice x biome
  d <- d[order(-(1 - d$omega_disc)), ]
  rows <- sprintf("%s & %s & %.0f & %.0f & %.0f",
    .esc(d$practice), .esc(d$biome), d$H,
    100 * (1 - d$omega_disc), 100 * (1 - d$omega_b100))
  write_tex(rows,
    header = c("Practice", "Biome", "$\\tau_2$ (yr)",
               "$T$ NPV-perm. (\\%)", "$T$ tonne-yr 100 (\\%)"),
    align = "llrrr",
    caption = "\\textbf{Temporality comparison.} Temporality deduction $T=1-\\omega$ under the NPV permanence benchmark ($H_{\\text{ref}}\\to\\infty$) vs tonne-year accounting ($H_{\\text{ref}}=100$ yr), per crediting period $\\tau_2$.",
    label = "tab:temporality_comparison", file = "latex_temporality_comparison.tex",
    note = "Engine temporality variants (engine/output/temporality\\_variants.csv).")
})

# --- supplementary parameter table: model constants -------------------------
local({
  mc <- eng("../params/model_constants.csv")
  keep <- c("r", "g", "tau_1", "H_perm", "kappa", "ell_max", "gamma", "alpha",
            "beta", "theta_base", "H0")
  mc <- mc[mc$name %in% keep, ]
  sym <- c(r = "$r$", g = "$g$", tau_1 = "$\\tau_1$", H_perm = "$H_{\\text{perm}}$",
           kappa = "$\\kappa$", ell_max = "$\\ell_{\\max}$", gamma = "$\\gamma$",
           alpha = "$\\alpha$", beta = "$\\beta$", theta_base = "$\\theta_{\\text{base}}$",
           H0 = "$H_0$")
  plabel <- unname(sym[mc$name]); plabel[is.na(plabel)] <- .esc(mc$name[is.na(plabel)])
  rows <- sprintf("%s & %s & %s", plabel, .esc(format(mc$value)), .esc(mc$source))
  write_tex(rows, header = c("Parameter", "Value", "Source"), align = "llp{8cm}",
    caption = "\\textbf{Framework parameters (global constants).} Central values; biome- and practice-level parameters in the respective supplementary tables.",
    label = "tab:framework_params_auto", file = "supplementary_parameter_tables.tex",
    note = "Sourced CSV engine/params/model\\_constants.csv.")
})

# --- H_ref sensitivity of NCV (practice x H_ref) ----------------------------
local({
  d <- eng("href_ncv.csv"); d <- d[!duplicated(paste(d$practice, d$biome, d$H_ref)), ]
  w <- reshape(d[, c("practice", "biome", "H_ref", "net_share")],
               idvar = c("practice", "biome"), timevar = "H_ref", direction = "wide")
  w <- w[order(-w$`net_share.Inf`), ]
  rows <- sprintf("%s & %s & %.0f & %.0f & %.0f", .esc(w$practice), .esc(w$biome),
    100 * w$`net_share.100yr`, 100 * w$`net_share.1000yr`, 100 * w$`net_share.Inf`)
  write_tex(rows, header = c("Practice", "Biome", "$H_{\\text{ref}}$=100yr", "1000yr", "$\\infty$"),
    align = "llrrr",
    caption = "\\textbf{NCV by reference horizon.} Net climate value (\\%) under tonne-year ($H_{\\text{ref}}=100$), $H_{\\text{ref}}=1000$, and the permanence benchmark ($H_{\\text{ref}}\\to\\infty$).",
    label = "tab:href_ncv", file = "table_href_ncv_latex.tex",
    note = "Engine H\\_ref sweep (engine/output/href\\_ncv.csv); 1000yr $\\approx\\infty$ as temporality saturates.")
})

# --- H_ref sensitivity of scheme integrity gaps -----------------------------
local({
  d <- eng("href_gaps.csv")
  w <- reshape(d[, c("scheme_name", "H_ref", "mean_gap")],
               idvar = "scheme_name", timevar = "H_ref", direction = "wide")
  w <- w[order(-w$`mean_gap.Inf`), ]
  rows <- sprintf("%s & %.0f & %.0f & %.0f", .esc(w$scheme_name),
    100 * w$`mean_gap.100yr`, 100 * w$`mean_gap.1000yr`, 100 * w$`mean_gap.Inf`)
  write_tex(rows, header = c("Scheme", "$H_{\\text{ref}}$=100yr", "1000yr", "$\\infty$"),
    align = "lrrr",
    caption = "\\textbf{Integrity gap by reference horizon.} Mean over-crediting (\\%) per scheme under each $H_{\\text{ref}}$; the gap widens with stricter permanence.",
    label = "tab:href_gaps", file = "table_href_gaps_latex.tex",
    note = "Engine H\\_ref sweep (engine/output/href\\_gaps.csv).")
})

# --- S7 modular adoption: net share under each subset of {L,T,b} -------------
local({
  d <- eng("clean_headline.csv"); d <- d[as.logical(d$is_anchor), ]
  d <- d[order(-d$net_share), ]
  Lc <- pmax(d$L, 0)                                  # clamp neg leakage for partials
  rows <- sprintf("%s (%s) & %.0f & %.0f & %.0f & %.0f & %.0f & %.0f & %.0f & %.0f & %.0f & %.0f",
    .esc(d$practice), .esc(substr(d$biome, 1, 3)),
    100 * d$L, 100 * d$T, 100 * d$b,
    100 * d$net_share,                                # Full NCV (signed L)
    100 * (1 - Lc), 100 * (1 - d$T), 100 * (1 - d$b), # L / T / b only
    100 * (1 - Lc) * (1 - d$T), 100 * (1 - Lc) * (1 - d$b), 100 * (1 - d$T) * (1 - d$b))
  write_tex(rows,
    header = c("Practice", "$L$", "$T$", "$b$", "Full NCV", "$L$ only", "$T$ only",
               "$b$ only", "$L{+}T$", "$L{+}b$", "$T{+}b$"),
    align = "lrrr|rrrrrrr",
    caption = "\\textbf{Modular adoption of the NCV framework.} Per-practice deductions ($L,T,b$) and net share under every subset of the three multiplicative channels (\\%, anchor biome, $k_0=0.01$). Schemes currently apply at most ``$b$ only''. Negative leakage clamped at 0 in partial columns; Full NCV uses signed $L$.",
    label = "tab:modular_adoption", file = "table_S7_modular_adoption.tex",
    note = "Engine headline (engine/output/clean\\_headline.csv).")
})

# --- jrc_buffer_comparison: our TVaR99 vs JRC, by forest type ----------------
local({
  d <- eng("jrc_country_comparison.csv")
  byft <- do.call(rbind, lapply(split(d, d$forest_type), function(z) data.frame(
    forest_type = z$forest_type[1], n = nrow(z), pct_within = 100 * mean(z$in_range),
    mean_ours = 100 * mean(z$b_ours), mean_jrc = 100 * mean(z$jrc_mean),
    med_absdiff = 100 * median(z$abs_diff), stringsAsFactors = FALSE)))
  allrow <- data.frame(forest_type = "All", n = nrow(d), pct_within = 100 * mean(d$in_range),
    mean_ours = 100 * mean(d$b_ours), mean_jrc = 100 * mean(d$jrc_mean),
    med_absdiff = 100 * median(d$abs_diff))
  byft <- rbind(byft, allrow)
  rows <- sprintf("%s & %d & %.0f & %.1f & %.1f & %.1f",
    .esc(tools::toTitleCase(byft$forest_type)), byft$n, byft$pct_within,
    byft$mean_ours, byft$mean_jrc, byft$med_absdiff)
  write_tex(rows,
    header = c("Forest type", "$n$ cells", "Within band (\\%)", "Our mean (\\%)",
               "JRC mean (\\%)", "Median $|\\Delta|$ (pp)"),
    align = "lrrrrr",
    caption = "\\textbf{Buffer cross-check vs JRC (Marinelli 2026).} Per-country empirical TVaR$_{99}$ buffer vs the JRC P10--P90 per-hexagon band. CONSISTENCY check (both built on the EFDA disturbance atlas), not independent validation.",
    label = "tab:jrc_buffer_comparison", file = "jrc_buffer_comparison.tex",
    note = "Engine JRC comparison (engine/output/jrc\\_country\\_comparison.csv).")
})
