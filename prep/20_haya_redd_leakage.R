# =============================================================================
# 20_haya_redd_leakage.R — VCS REDD+ Leakage Rates (Haya et al. 2023)
# =============================================================================
# Standalone script: not part of the main pipeline (00_master.R).
# Extracted from 02_data_extraction.R for potential future use.
#
# Source: Haya, B. K., Bomfim, B., Holm, J. A., & Parekh, A. (2023).
#   Quality Assessment of REDD+ Carbon Credit Projects.
#   Berkeley Carbon Trading Project, UC Berkeley.
#   Table 3.A14 (pp. 84-86): Project-specific leakage rates, n=73.
#   Table 3.4 / text (p. 73): Pooled methodology-specific rates.
#
# DEFINITIONS:
#   - "Effective leakage rate" = total leakage deduction / net avoided
#     emissions, from the most recent monitoring report(s).
#   - Activity-shifting: monitored emissions in leakage belt + ex ante
#     activity-shifting deductions (e.g. unconstrained drivers).
#   - Market leakage: VMD0011 lookup (LF_ME × LK_MAF × displaced area).
#   - "Pooled" rate (footnote 14, p. 73): total leakage deductions summed
#     across all projects / total net avoided emissions summed across all
#     projects. This is SIZE-WEIGHTED — not the simple average of
#     project-level percentages. We cannot reproduce pooled rates from
#     percentage data alone; the published pooled values are taken as given.
#
# 75 projects initially; 2 excluded for unclear leakage accounting → n=73.
# Methodologies: VM0006 (n=11), VM0007 (n=29), VM0009 (n=11), VM0015 (n=22).
# =============================================================================

library(tidyverse)

cat("\n--- VCS REDD+ leakage rates (Haya et al. 2023) ---\n")

# C1. Project-level data from Table 3.A14 (pp. 84-86)
# Rates are reported as percentages of net avoided emissions in the most
# recent monitoring report. We store as proportions (0-1).

haya_projects <- tibble::tribble(
  ~project_id,  ~methodology, ~activity_shifting, ~market, ~total,
  "VCS562",     "VM0009",     0.00,  0.00,  0.00,
  "VCS612",     "VM0009",     0.00,  0.00,  0.00,
  "VCS647",     "VM0007",     0.29,  0.00,  0.29,
  "VCS812",     "VM0007",     0.00,  0.00,  0.00,
  "VCS818",     "VM0007",     0.00,  0.00,  0.00,
  "VCS832",     "VM0007",     0.01,  0.21,  0.22,
  "VCS844",     "VM0007",     0.02,  0.00,  0.02,
  "VCS852",     "VM0007",     0.32,  0.00,  0.32,
  "VCS856",     "VM0009",     0.00,  0.00,  0.00,
  "VCS868",     "VM0007",     0.03,  0.00,  0.03,
  "VCS875",     "VM0007",     0.00,  0.03,  0.03,
  "VCS902",     "VM0009",     0.00,  0.00,  0.00,
  "VCS904",     "VM0006",     0.00,  0.15,  0.15,
  "VCS934",     "VM0009",     0.00,  0.00,  0.00,
  "VCS944",     "VM0015",     0.00,  0.00,  0.00,
  "VCS953",     "VM0007",     0.40,  0.00,  0.40,
  "VCS958",     "VM0015",     0.00,  0.00,  0.00,
  "VCS963",     "VM0007",     0.09,  0.00,  0.09,
  "VCS985",     "VM0007",     0.03,  0.00,  0.03,
  "VCS1067",    "VM0007",     0.00,  0.00,  0.00,
  "VCS1094",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1112",    "VM0007",     0.03,  0.00,  0.03,
  "VCS1113",    "VM0007",     0.12,  0.00,  0.12,
  "VCS1115",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1118",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1133",    "VM0007",     0.00,  0.03,  0.03,
  "VCS1168",    "VM0006",     0.00,  0.00,  0.00,
  "VCS1175",    "VM0007",     0.00,  0.00,  0.00,
  "VCS1201",    "VM0007",     0.04,  0.01,  0.05,
  "VCS1202",    "VM0009",     0.00,  0.00,  0.00,
  "VCS1215",    "VM0007",     0.00,  0.00,  0.00,
  "VCS1218",    "VM0007",     0.00,  0.00,  0.00,
  "VCS1311",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1325",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1326",    "VM0007",     0.00,  0.12,  0.12,
  "VCS1329",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1340",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1359",    "VM0006",     0.17,  0.00,  0.17,
  "VCS1360",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1382",    "VM0007",     0.00,  0.14,  0.14,
  "VCS1389",    "VM0006",     0.00,  0.21,  0.21,
  "VCS1390",    "VM0006",     0.00,  0.21,  0.21,
  "VCS1391",    "VM0006",     0.00,  0.20,  0.20,
  "VCS1392",    "VM0006",     0.00,  0.21,  0.21,
  "VCS1395",    "VM0006",     0.00,  0.20,  0.20,
  "VCS1396",    "VM0006",     0.00,  0.21,  0.21,
  "VCS1399",    "VM0006",     0.00,  0.20,  0.20,
  "VCS1400",    "VM0006",     0.00,  0.20,  0.20,
  "VCS1403",    "VM0007",     0.04,  0.00,  0.04,
  "VCS1408",    "VM0009",     0.08,  0.00,  0.08,
  "VCS1477",    "VM0007",     0.00,  0.00,  0.00,
  "VCS1503",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1532",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1541",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1566",    "VM0007",     0.11,  0.00,  0.11,
  "VCS1571",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1622",    "VM0015",     0.00,  0.09,  0.09,
  "VCS1650",    "VM0015",     0.33,  0.00,  0.33,
  "VCS1654",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1686",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1689",    "VM0009",     0.00,  0.01,  0.01,
  "VCS1748",    "VM0009",     0.00,  0.01,  0.01,
  "VCS1775",    "VM0009",     0.00,  0.00,  0.00,
  "VCS1811",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1882",    "VM0015",     0.00,  0.00,  0.00,
  "VCS1897",    "VM0007",     0.11,  0.00,  0.11,
  "VCS1899",    "VM0007",     0.00,  0.00,  0.00,
  "VCS1900",    "VM0007",     0.00,  0.10,  0.10,
  "VCS1953",    "VM0015",     0.00,  0.00,  0.00,
  "VCS2252",    "VM0015",     0.00,  0.00,  0.00,
  "VCS2290",    "VM0007",     0.08,  0.00,  0.08,
  "VCS2293",    "VM0009",     0.00,  0.00,  0.00,
  "VCS2324",    "VM0007",     0.00,  0.00,  0.00
)

# C2. Verify project counts match source
n_total <- nrow(haya_projects)
n_by_method <- table(haya_projects$methodology)
stopifnot(n_total == 73)
stopifnot(n_by_method[["VM0006"]] == 11)
stopifnot(n_by_method[["VM0007"]] == 29)
stopifnot(n_by_method[["VM0009"]] == 11)
stopifnot(n_by_method[["VM0015"]] == 22)
cat(sprintf("  Table 3.A14: %d projects across %d methodologies — verified\n",
            n_total, length(n_by_method)))

# C3. Compute simple (unweighted) summary statistics per methodology
# NOTE: These are UNWEIGHTED averages of project-level percentages.
# The Haya et al. "pooled" rates (Table 3.4) are SIZE-WEIGHTED
# (total deductions / total avoided emissions across projects).
# We cannot reproduce those from percentage data alone, so the pooled
# values are stored separately as published.

haya_method_stats <- haya_projects %>%
  group_by(methodology) %>%
  summarise(
    n_projects        = n(),
    mean_total        = mean(total),
    median_total      = median(total),
    sd_total          = sd(total),
    max_total         = max(total),
    share_zero        = mean(total == 0),
    mean_activity     = mean(activity_shifting),
    mean_market       = mean(market),
    .groups = "drop"
  )

haya_overall_stats <- haya_projects %>%
  summarise(
    n_projects        = n(),
    mean_total        = mean(total),
    median_total      = median(total),
    sd_total          = sd(total),
    max_total         = max(total),
    share_zero        = mean(total == 0),
    mean_activity     = mean(activity_shifting),
    mean_market       = mean(market)
  )

cat(sprintf("  Unweighted mean total leakage: %.1f%% (median %.0f%%, SD %.1f%%)\n",
            haya_overall_stats$mean_total * 100,
            haya_overall_stats$median_total * 100,
            haya_overall_stats$sd_total * 100))
cat(sprintf("  Share of projects with zero leakage: %.0f%%\n",
            haya_overall_stats$share_zero * 100))

# C4. Published pooled (size-weighted) rates from Table 3.4 / text (p. 73)
# These are NOT computable from percentage data — they require absolute
# deduction and avoided-emission volumes per project. Taken as given.
haya_pooled <- tibble::tribble(
  ~methodology, ~pooled_total, ~source,
  "VM0006",     0.148,         "Haya et al. 2023 Table 3.4",
  "VM0007",     0.090,         "Haya et al. 2023 Table 3.4",
  "VM0009",     0.024,         "Haya et al. 2023 Table 3.4",
  "VM0015",     0.035,         "Haya et al. 2023 Table 3.4",
  "ALL",        0.066,         "Haya et al. 2023 p.73"
)

# C5. Assemble output list
haya_redd_leakage <- list(
  projects       = haya_projects,
  method_stats   = haya_method_stats,
  overall_stats  = haya_overall_stats,
  pooled_rates   = haya_pooled,
  metadata       = list(
    source    = "Haya et al. (2023) Quality Assessment of REDD+ Carbon Credit Projects, BCTP",
    table     = "Table 3.A14, pp. 84-86",
    n         = 73L,
    note      = paste("Pooled rates are size-weighted (total deductions / total avoided emissions).",
                       "Simple averages of project-level rates differ from pooled rates.",
                       "Leakage rate = total deduction / net avoided emissions from most recent",
                       "monitoring report(s). Two projects excluded from original 75 for unclear",
                       "leakage accounting.")
  )
)

saveRDS(haya_redd_leakage, "data/processed/haya_redd_leakage.rds")
cat("Saved: data/processed/haya_redd_leakage.rds\n")
