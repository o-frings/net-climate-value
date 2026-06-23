# =============================================================================
# 11_country.R  —  country-level outputs (ED figures: disturbance, JRC, pool)
# =============================================================================
# Per-country summaries derived from the committed EFDA extract. Figure RENDERING
# (choropleth map, JRC dumbbell, pool-buildup curve) lives in the P4 figure layer;
# this module computes the NUMBERS behind them. Reads only data/processed/.
#
# (1) Country disturbance summary (ed_fig_country_disturbance): forest-area-weighted
#     natural-disturbance rate 1986-2023 per country + the 2018-2023 / 1986-2017
#     elevation ratio. Sub-national EFDA zones (France, Italy) aggregated to country.
# =============================================================================
cat("[11_country] country-level disturbance summary...\n")

efda <- as.data.frame(readRDS("data/processed/efda_country_summary.rds"))
stopifnot(all(c("country_root", "forest_kha", "lambda_full",
                "lambda_pre2018", "lambda_post2018") %in% names(efda)))

wm <- function(x, w) { ok <- !is.na(x) & !is.na(w); sum(x[ok] * w[ok]) / sum(w[ok]) }
country_disturbance <- do.call(rbind, lapply(split(efda, efda$country_root), function(d) {
  data.frame(
    country         = d$country_root[1],
    lambda_full     = wm(d$lambda_full,     d$forest_kha),
    lambda_pre2018  = wm(d$lambda_pre2018,  d$forest_kha),
    lambda_post2018 = wm(d$lambda_post2018, d$forest_kha),
    forest_kha      = sum(d$forest_kha, na.rm = TRUE),
    stringsAsFactors = FALSE)
}))
country_disturbance$elevation_ratio <-
  country_disturbance$lambda_post2018 / pmax(country_disturbance$lambda_pre2018, 1e-6)
rownames(country_disturbance) <- NULL
country_disturbance <- country_disturbance[order(-country_disturbance$lambda_full), ]

write.csv(country_disturbance, "engine/output/country_disturbance.csv", row.names = FALSE)
cat(sprintf("[11_country] disturbance: %d countries; rate %.3f%%-%.3f%%; top elevation %.1fx\n",
            nrow(country_disturbance), 100*min(country_disturbance$lambda_full),
            100*max(country_disturbance$lambda_full), max(country_disturbance$elevation_ratio)))

# =============================================================================
# (2) Parametric premium buffer per zone (ed_fig_parametric_vs_empirical)
# =============================================================================
# Actuarial premium principle b = (1+theta) * lambda_adj / tau, evaluated per EFDA
# ZONE x forest_type for Protected afforestation (H=40, tau=1/40). Faithful port of
# calc_buffer_rate_zone(): lambda_adj = lambda_zone*lambda_mult * R^alpha * E_Z(sev),
# theta = theta_base*sqrt(H/H_ref)*sqrt(c_zone*c_mult) (N_pool=Inf). Forest-type R
# from forest_type_R.csv (biome-specific). Compared against the empirical VaR99/TVaR99.
.mc  <- setNames(read.csv("engine/params/model_constants.csv")$value,
                 read.csv("engine/params/model_constants.csv")$name)
k    <- function(n) unname(.mc[[n]])
ftR  <- read.csv("engine/params/forest_type_R.csv", stringsAsFactors = FALSE)
ccdf <- read.csv("engine/params/biome_correlation.csv", stringsAsFactors = FALSE)
c_of <- setNames(ccdf$c, ccdf$biome)
R_of2 <- function(b, ft) { v <- ftR$R[ftR$biome == b & ftR$forest_type == ft]
  if (length(v) != 1) stop("no R for ", b, "/", ft); v }
E_Z_of <- function(p_sr) p_sr + (1 - p_sr) * k("partial_severity_a") /
                          (k("partial_severity_a") + k("partial_severity_b"))
LAMBDA_MULT_AFF <- 0.9; C_MULT_AFF <- 0.9   # Protected afforestation (practices.csv)
H <- 40; tau <- 1 / 40; alpha <- k("alpha")

param_buffer <- function(lambda, severity, biome, ft) {
  lambda_adj <- lambda * LAMBDA_MULT_AFF * (R_of2(biome, ft) ^ alpha) * E_Z_of(severity)
  cc    <- c_of[[biome]] * C_MULT_AFF
  theta <- k("theta_base") * sqrt(H / k("H0")) * sqrt(max(cc, 0))   # N_pool=Inf
  min(max((1 + theta) * lambda_adj / tau, 0), 1)
}

zones <- efda[!is.na(efda$lambda_full) & !is.na(efda$severity), ]
country_parametric <- do.call(rbind, lapply(seq_len(nrow(zones)), function(i) {
  z <- zones[i, ]
  do.call(rbind, lapply(c("broadleaf", "conifer"), function(ft)
    data.frame(zone_label = z$zone_label, country_root = z$country_root,
               biome = z$biome, forest_type = ft, forest_kha = z$forest_kha,
               b_parametric = param_buffer(z$lambda_full, z$severity, z$biome, ft),
               stringsAsFactors = FALSE)))
}))
rownames(country_parametric) <- NULL
write.csv(country_parametric, "engine/output/country_parametric_buffer.csv", row.names = FALSE)
cat(sprintf("[11_country] parametric buffer: %d zone x forest-type rows, b %.1f%%-%.1f%%\n",
            nrow(country_parametric), 100*min(country_parametric$b_parametric),
            100*max(country_parametric$b_parametric)))
