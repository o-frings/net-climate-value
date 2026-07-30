# =============================================================================
# 04_headline.R  —  net climate value per practice (the headline result)
# =============================================================================
# Assembles the verified components into the headline issuance share:
#   net_share = (1 - L) * (1 - T) * (1 - b)        (manuscript Eq issuance_rule)
# for every practice x biome x forest-type. L and T come from 02_model.R; b is
# the biome-level empirical TVaR99 buffer = forest-area-weighted mean of the
# clean per-country buffers (03_buffer.R). Legally-protected practices
# (afforestation) are priced at the H_perm horizon for BOTH T and b.
# Assumes 01_data.R, 02_model.R, 03_buffer.R have run (run_clean.R sources them).
# =============================================================================
cat("[04_headline] assembling net_share = (1-L)(1-T)(1-b)...\n")

practices    <- read.csv("engine/params/practices.csv", stringsAsFactors = FALSE)
clean_buf    <- read.csv("engine/output/clean_buffer_rates.csv", stringsAsFactors = FALSE)
efda_sum     <- as.data.frame(readRDS("data/processed/efda_country_summary.rds"))
H_perm       <- .const("H_perm")   # from 02_model.R (sourced model constant)
# NOTE (2026-06): the afforestation establishment-risk floor was REMOVED. The
# buffer now prices the SAME mature-stand natural disturbance (fire/wind/insect)
# as the JRC CRCF benchmark and nothing more. Establishment / non-establishment
# is a non-accrual handled by ex-post verification of realised Q (lower Q), not a
# reversal the buffer should price; an extra floor would partly double-count Q and
# rested on expert estimates from per-seedling mortality (wrong level). See
# docs/audits/PARAM_SOURCING_AUDIT.md (establishment_floor + JRC scope).

# --- biome-level buffer = forest-area-weighted mean of per-country buffers ----
# Each EFDA zone (incl. France/Italy sub-zones) carries its whole-country buffer
# at the zone's forest area, then averaged within biome x forest-type x horizon.
zone <- merge(efda_sum[, c("country_root", "biome", "forest_kha")],
              clean_buf, by.x = "country_root", by.y = "country")
biome_buffer <- function(biome, ft, horizon_col) {
  z <- zone[zone$biome == biome & zone$forest_type == ft, ]
  if (nrow(z) == 0) stop("no buffer for biome=", biome, " ft=", ft)
  sum(z$forest_kha * z[[horizon_col]]) / sum(z$forest_kha)
}

# Biome-level buffer point estimate + batch-means SE per biome x forest_type x
# horizon (forest-area-weighted, matching analysis/R/26). Written to engine/output
# so the Monte Carlo layer (07) can draw b ~ N(b_central, b_se). This is the
# UNFLOORED biome buffer; the headline/MC apply the establishment floor on top.
biome_buffer_table <- do.call(rbind, lapply(sort(unique(zone$biome)), function(bm) {
  do.call(rbind, lapply(c("broadleaf", "conifer"), function(ft) {
    z <- zone[zone$biome == bm & zone$forest_type == ft, ]
    if (nrow(z) == 0) return(NULL)
    w <- z$forest_kha / sum(z$forest_kha)
    data.frame(biome = bm, forest_type = ft,
               b_H40  = sum(w * z$b_TVaR99_H40),  se_H40  = sum(w * z$se_TVaR99_H40),
               b_H100 = sum(w * z$b_TVaR99_H100), se_H100 = sum(w * z$se_TVaR99_H100),
               stringsAsFactors = FALSE)
  }))
}))
write.csv(biome_buffer_table, "engine/output/biome_buffer.csv", row.names = FALSE)

# --- per-practice net_share ---------------------------------------------------
# The buffer b now applies the practice-specific vulnerability multipliers
# (R_mult, lambda_mult, c_mult; practices.csv / manuscript Supp Table mgmt) on top
# of the biome base, via the empirical TVaR99 bootstrap (practice_buffer_rate, 03).
# The unmultiplied biome_buffer table above stays the biome-mean reference.
H_buf <- function(protected) if (protected) H_perm else 40
headline <- do.call(rbind, lapply(seq_len(nrow(practices)), function(i) {
  row <- practices[i, ]
  protected <- isTRUE(as.logical(row$legally_protected))
  x  <- resolve_x(row)                                   # 02_model.R
  L  <- leakage_L(row$practice, row$biome, x)            # 02_model.R
  T  <- temporality_T(tau_2_temporality(row))            # 02_model.R
  pb <- practice_buffer_rate(row$biome, row$forest_type, H_buf(protected),
                             row$R_mult, row$lambda_mult, row$c_mult)  # 03_buffer.R
  b  <- pb$b
  data.frame(practice = row$practice, biome = row$biome, species = row$species,
             is_anchor = as.logical(row$is_anchor),
             x = x, L = L, T = T, b = b, b_se = pb$se,
             net_share = net_share(L, T, b), stringsAsFactors = FALSE)
}))
rownames(headline) <- NULL

write.csv(headline, "engine/output/clean_headline.csv", row.names = FALSE)

# --- practice-level buffer table + range (present-day + end-of-century RCP8.5) --
# Each practice x biome x forest_type at its horizon, buffer at present-day climate
# and at sustained end-of-century RCP8.5 uplift. Feeds the ED practice-spread figure
# and the manuscript's "b in [lo, hi] across practice-biome-climate" range.
u100_85 <- setNames(biome_data$U_100_rcp85, biome_data$biome)   # biome_data from 01_data.R
combos  <- unique(practices[, c("practice", "biome", "forest_type",
                                "R_mult", "lambda_mult", "c_mult", "legally_protected")])
practice_buffer <- do.call(rbind, lapply(seq_len(nrow(combos)), function(i) {
  cc <- combos[i, ]; H <- H_buf(isTRUE(as.logical(cc$legally_protected)))
  pres <- practice_buffer_rate(cc$biome, cc$forest_type, H, cc$R_mult, cc$lambda_mult, cc$c_mult)
  Uend <- u100_85[[cc$biome]]
  endc <- if (is.null(Uend) || is.na(Uend)) NA_real_ else
    practice_buffer_rate(cc$biome, cc$forest_type, H, cc$R_mult, cc$lambda_mult, cc$c_mult,
                         uplift_vec = rep(Uend, H))$b
  data.frame(practice = cc$practice, biome = cc$biome, forest_type = cc$forest_type,
             H = H, R_mult = cc$R_mult, lambda_mult = cc$lambda_mult, c_mult = cc$c_mult,
             b_present = pres$b, se_present = pres$se, b_rcp85_2100 = endc,
             stringsAsFactors = FALSE)
}))
rownames(practice_buffer) <- NULL
write.csv(practice_buffer, "engine/output/practice_buffer.csv", row.names = FALSE)
b_all  <- c(practice_buffer$b_present, practice_buffer$b_rcp85_2100)
# An NA buffer here means a cell failed to compute, so na.rm would report the published
# b range over an unknown subset. Require every cell to have produced a rate.
if (anyNA(b_all)) stop("practice buffer: ", sum(is.na(b_all)), " of ", length(b_all),
                       " cells are NA; b range would cover an unknown subset")
rng_lo <- min(b_all); rng_hi <- max(b_all)
cat(sprintf("[04_headline] practice buffer range b in [%.2f, %.2f] across %d practice-biome combos x {present, RCP8.5-2100}\n",
            rng_lo, rng_hi, nrow(combos)))
anc <- headline[headline$is_anchor, ]
cat(sprintf("[04_headline] OK — %d rows (%d anchors). Anchor net_share: %.1f%%-%.1f%% (mean %.1f%%)\n",
            nrow(headline), nrow(anc), 100*min(anc$net_share), 100*max(anc$net_share),
            100*mean(anc$net_share)))
