# =============================================================================
# 14_href_sensitivity.R  —  reference-horizon (H_ref) sensitivity of NCV + gaps
# =============================================================================
# Recomputes net_share and scheme integrity gaps at H_ref in {100, 1000, Inf}.
# H_ref enters only temporality T (= temporality_T(tau_2, H_ref)); leakage L and
# buffer b are H_ref-invariant, and scheme-side deductions are H_ref-agnostic.
# Reuses 02_model (leakage_L, temporality_T, resolve_x, tau_2_temporality) +
# engine/output/{clean_headline, biome_buffer} + engine/params/{practices, schemes,
# scheme_coverage}. Outputs href_ncv.csv + href_gaps.csv. Assumes 02 + 04 + 10 ran.
# =============================================================================
cat("[14_href] reference-horizon (H_ref) sensitivity...\n")

H_REF <- read.csv("engine/params/href_values.csv", stringsAsFactors = FALSE)  # label, H_ref
practices <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
hl   <- read.csv("engine/output/clean_headline.csv", stringsAsFactors = FALSE)
bbuf <- read.csv("engine/output/biome_buffer.csv", stringsAsFactors = FALSE)
schemes  <- read.csv("engine/params/schemes.csv", stringsAsFactors = FALSE)
coverage <- read.csv("engine/params/scheme_coverage.csv", stringsAsFactors = FALSE)
H_perm <- .const("H_perm")
hkey <- function(p, b, s) paste(p, b, s, sep = "\r")
h_Lb <- hl[, c("practice", "biome", "species", "L", "b")]

# biome buffer (no establishment floor; matches 04/10 post-drop)
buf_of <- function(biome, ft, protected) {
  z <- bbuf[bbuf$biome == biome & bbuf$forest_type == ft, ]
  if (protected) z$b_H100 else z$b_H40
}

# --- (1) NCV by H_ref (anchors) ----------------------------------------------
anc <- practices[as.logical(practices$is_anchor), ]
ncv <- do.call(rbind, lapply(seq_len(nrow(anc)), function(i) {
  row <- anc[i, ]; k <- hkey(row$practice, row$biome, row$species)
  lb <- h_Lb[hkey(h_Lb$practice, h_Lb$biome, h_Lb$species) == k, ]
  if (nrow(lb) != 1) return(NULL)
  tau2 <- tau_2_temporality(row)
  do.call(rbind, lapply(seq_len(nrow(H_REF)), function(j) {
    Th <- temporality_T(tau2, H_ref = H_REF$H_ref[j])
    data.frame(practice = row$practice, biome = row$biome, species = row$species,
               H_ref = H_REF$label[j],
               net_share = (1 - lb$L) * (1 - Th) * (1 - lb$b), stringsAsFactors = FALSE)
  }))
}))
write.csv(ncv, "engine/output/href_ncv.csv", row.names = FALSE)

# --- (2) scheme integrity gaps by H_ref --------------------------------------
wmean <- function(x, w) { ok <- !is.na(x) & w > 0; if (!any(ok)) NA_real_ else sum(x[ok]*w[ok])/sum(w[ok]) }
rows <- list()
for (k in seq_len(nrow(coverage))) {
  cv <- coverage[k, ]; sc <- schemes[schemes$scheme_id == cv$scheme_id, ]
  apr <- anc[anc$practice == cv$practice, ]; if (nrow(apr) == 0) next
  eb <- isTRUE(sc$exclude_buffer)
  scheme_ns <- if (eb) (1 - sc$leakage_rate) else (1 - sc$leakage_rate) * (1 - sc$buffer_rate)
  liab_mid <- (sc$liability_years_min + sc$liability_years) / 2
  for (j in seq_len(nrow(apr))) {
    row <- apr[j, ]; protected <- isTRUE(as.logical(row$legally_protected))
    biome <- if (nzchar(sc$regional_override)) sc$regional_override else row$biome
    tau2 <- if (protected) H_perm else max(row$tau_2, liab_mid)
    x <- resolve_x(row); L <- leakage_L(row$practice, biome, x)
    b <- buf_of(biome, row$forest_type, protected)
    ftw <- if (row$forest_type == "broadleaf") cv$ft_broadleaf else cv$ft_conifer
    w <- (if (is.na(cv$practice_weight)) 1 else cv$practice_weight) * (if (is.na(ftw)) 1 else ftw)
    for (hh in seq_len(nrow(H_REF))) {
      Th <- temporality_T(tau2, H_ref = H_REF$H_ref[hh])
      prop <- if (eb) (1 - L) * (1 - Th) else (1 - L) * (1 - Th) * (1 - b)
      rows[[length(rows) + 1]] <- data.frame(scheme = cv$scheme_id, scheme_name = sc$name,
        H_ref = H_REF$label[hh], gap_pct = (scheme_ns - prop) / scheme_ns,
        w = w, exclude_figures = isTRUE(sc$exclude_figures), stringsAsFactors = FALSE)
    }
  }
}
g <- do.call(rbind, rows); g <- g[!g$exclude_figures, ]
gaps <- do.call(rbind, lapply(split(g, list(g$scheme, g$H_ref), drop = TRUE), function(d)
  data.frame(scheme = d$scheme[1], scheme_name = d$scheme_name[1], H_ref = d$H_ref[1],
             mean_gap = wmean(d$gap_pct, d$w), stringsAsFactors = FALSE)))
rownames(gaps) <- NULL
write.csv(gaps, "engine/output/href_gaps.csv", row.names = FALSE)

cat(sprintf("[14_href] OK — NCV %d rows, gaps %d (scheme x H_ref). H_ref: %s\n",
            nrow(ncv), nrow(gaps), paste(H_REF$label, collapse = ", ")))
