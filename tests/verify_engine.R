#!/usr/bin/env Rscript
# =============================================================================
# verify_engine.R  —  invariant + regression gate for the rebuilt engine
# =============================================================================
# Fails loud if a structural invariant breaks or an output drifts. Run from analysis/,
# after run_engine.R:
#   Rscript tests/verify_engine.R              # invariants + checksum comparison
#   Rscript tests/verify_engine.R --bless      # rewrite the checksum manifest
#
# Why this exists: every silent fallback found in the 2026-07-28 audit was invisible
# precisely because nothing asserted the property it violated. The engine is fully
# seeded and byte-reproducible, so a checksum manifest turns any unintended change into
# a test failure instead of a quietly different number. --bless is the deliberate
# acknowledgement step when the methodology really did change.
#
# Structural invariants are exact; nothing here carries a tolerance except the
# median-draw residual (see check 4), which is bounded by MC granularity.
# =============================================================================
stopifnot(basename(getwd()) == "analysis")
OUT <- "engine/output"
MANIFEST <- "tests/output_manifest.csv"
bless <- "--bless" %in% commandArgs(TRUE)

.fail <- 0L
ok <- function(label, cond, detail = "") {
  pass <- isTRUE(cond)
  if (!pass) .fail <<- .fail + 1L
  cat(sprintf("  [%s] %-52s %s\n", if (pass) "PASS" else "FAIL", label, detail))
}
.rd <- function(f) {
  p <- file.path(OUT, f)
  if (!file.exists(p)) stop("verify: missing output ", p)
  read.csv(p, stringsAsFactors = FALSE)
}

cat("\n=== verify_engine ===\n")

# --- 1. adding-up identity in the raw draws --------------------------------------
# The multiplicative rule and its additive decomposition must agree exactly, per draw.
# This is the property the figure scripts silently violated by taking componentwise
# medians (the median is not linear).
mc <- readRDS(file.path(OUT, "mc_results.rds"))
id_err <- max(abs((mc$delta_leak + mc$delta_temp + mc$delta_buf) - (1 - mc$net_share)))
ok("draw decomposition sums to 1 - net_share", id_err < 1e-12, sprintf("max err %.2e", id_err))
ns_err <- max(abs(mc$net_share - (1 - mc$L) * (1 - mc$T) * (1 - mc$b)))
ok("net_share == (1-L)(1-T)(1-b)", ns_err < 1e-12, sprintf("max err %.2e", ns_err))

# --- 2. completeness: shapes tied to each other, not to magic numbers -------------
cells <- unique(mc[, c("practice", "biome", "species")])
mcs   <- .rd("mc_summary.csv")
hl    <- .rd("clean_headline.csv")
pol   <- .rd("policy_deductions.csv")
ok("mc_summary covers every MC cell", nrow(mcs) == nrow(cells),
   sprintf("%d vs %d", nrow(mcs), nrow(cells)))
ok("clean_headline covers every MC cell", nrow(hl) == nrow(cells),
   sprintf("%d vs %d", nrow(hl), nrow(cells)))
ok("policy_deductions covers every MC cell", nrow(pol) == nrow(cells),
   sprintf("%d vs %d", nrow(pol), nrow(cells)))
draws_per_cell <- table(paste(mc$practice, mc$biome, mc$species, sep = "\r"))
ok("every cell has the same draw count", length(unique(draws_per_cell)) == 1,
   sprintf("n = %s", paste(unique(draws_per_cell), collapse = "/")))

# --- 3. no NA in any published output ---------------------------------------------
# An NA in a published table means a cell failed to compute. Deliberate NAs must be
# listed here with a reason, so that adding one is a conscious act.
allowed_na <- list(
  # NA when the country has no end-century uplift (Uend absent).
  "practice_buffer.csv" = "b_rcp85_2100",
  # The observed 1986-2023 record is one realised series, so it has no ensemble spread;
  # only the projected scenarios carry p10/p90 (114 rows = 38 years x 3 biomes).
  "buffer_biome_disturbance.csv" = c("rate_p10", "rate_p90"),
  # Sparse long-format stat table: each row populates only its relevant column.
  "sens_additive_summary.csv" = c("multiplicative", "additive", "spearman_mult_vs_add"),
  # The pooled "ALL" rows span biomes, which have different assumed c, so there is no
  # single assumed value to print; inventing a weighted one would fabricate a parameter.
  "sens_within_country_c_biome.csv" = c("c_assumed", "n_eff_assumed")
)
na_bad <- character()
for (f in list.files(OUT, pattern = "\\.csv$")) {
  d <- .rd(f)
  cols <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], allowed_na[[f]])
  hit <- cols[vapply(d[cols], anyNA, logical(1))]
  if (length(hit)) na_bad <- c(na_bad, sprintf("%s:%s", f, paste(hit, collapse = ",")))
}
ok("no NA in published numeric columns", length(na_bad) == 0,
   if (length(na_bad)) paste(na_bad, collapse = " ") else "")

# --- 4. figures agree with the canonical medians ----------------------------------
# The figure tables must report the same NCV as mc_summary. This is exactly the
# fig3a-vs-ED2-vs-text discrepancy the audit found; it would now fail here.
key <- function(p, b, s) paste(p, b, s, sep = "\r")
med <- setNames(mcs$p50_share, key(mcs$practice, mcs$biome, mcs$species))
edn <- read.csv(file.path(OUT, "figdata/fd_ed_all_practices_net.csv"), stringsAsFactors = FALSE)
# ED2 is keyed by practice + variant label, so match through the cell list it was built
# from: one row per cell, same count, joined on practice and the median value itself.
ed_err <- NA_real_
if (nrow(edn) == nrow(cells)) {
  cand <- vapply(seq_len(nrow(edn)), function(i) {
    m <- med[grepl(paste0("^", edn$practice[i], "\r"), names(med))]
    min(abs(m - edn$net_share[i]))
  }, numeric(1))
  ed_err <- max(cand)
}
# tolerance = MC granularity: with an even draw count the median lies between two order
# statistics, so the closest single draw differs by O(1/n), far below label precision.
ok("ED2 net_share matches an mc_summary median", !is.na(ed_err) && ed_err < 1e-3,
   sprintf("max err %.1e pp", 100 * ed_err))

f3 <- read.csv(file.path(OUT, "figdata/fd_fig3_a_net.csv"), stringsAsFactors = FALSE)
f3_err <- max(vapply(seq_len(nrow(f3)), function(i) {
  p <- trimws(sub("\\[.*$", "", f3$bar_label[i]))
  m <- med[grepl(paste0("^", p, "\r"), names(med))]
  if (!length(m)) return(Inf)
  min(abs(m - f3$net_share[i]))
}, numeric(1)))
ok("fig3a net_share matches an mc_summary median", f3_err < 1e-3,
   sprintf("max err %.1e pp", 100 * f3_err))

# --- 5. scheme gap decomposition identity ----------------------------------------
sg <- .rd("scheme_gaps.csv")
sg_err <- max(abs((sg$delta_L + sg$delta_T + sg$delta_b) - (sg$scheme_net - sg$proposed_net)))
ok("scheme gap deltas sum to the gap", sg_err < 1e-10, sprintf("max err %.2e", sg_err))

# --- 6. byte-level regression against the blessed manifest ------------------------
files <- sort(list.files(OUT, pattern = "\\.(csv|rds)$", recursive = TRUE))
sums <- vapply(files, function(f) tools::md5sum(file.path(OUT, f))[[1]], character(1))
cur <- data.frame(file = files, md5 = unname(sums), stringsAsFactors = FALSE)
if (bless) {
  dir.create("tests", showWarnings = FALSE)
  write.csv(cur, MANIFEST, row.names = FALSE)
  cat(sprintf("  [BLESS] manifest rewritten: %d files\n", nrow(cur)))
} else if (!file.exists(MANIFEST)) {
  ok("output manifest present", FALSE, "run with --bless to create it")
} else {
  prev <- read.csv(MANIFEST, stringsAsFactors = FALSE)
  m <- merge(prev, cur, by = "file", all = TRUE, suffixes = c("_blessed", "_now"))
  changed <- m$file[!is.na(m$md5_blessed) & !is.na(m$md5_now) &
                      m$md5_blessed != m$md5_now]
  missing <- m$file[is.na(m$md5_now)]
  added   <- m$file[is.na(m$md5_blessed)]
  ok("no output changed vs blessed manifest", length(changed) == 0,
     if (length(changed)) paste(changed, collapse = " ") else "")
  ok("no blessed output missing", length(missing) == 0,
     if (length(missing)) paste(missing, collapse = " ") else "")
  if (length(added))
    cat(sprintf("  [note] %d new output(s) not yet blessed: %s\n",
                length(added), paste(added, collapse = " ")))
}

cat(sprintf("\n=== %s ===\n", if (.fail == 0L) "all checks passed" else
            sprintf("%d CHECK(S) FAILED", .fail)))
if (.fail > 0L) quit(status = 1L)
