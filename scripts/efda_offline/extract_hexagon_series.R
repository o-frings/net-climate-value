# Extract per-HEXAGON annual natural-disturbance rates from the EFDA rasters.
#
# Purpose. The committed artefacts are country-aggregated, so the within-country spatial
# correlation c (which sets N_eff = round(1/c) inside 03_buffer's per-country bootstrap,
# and therefore the country-level buffer and the headline NCV) cannot be estimated or
# tested for drift from them, and that parameter was queried in review. The JRC 35 km hexagons
# are the natural sub-national unit: they are the grain the JRC risk model itself uses, so an
# estimate on that grid is directly comparable to it.
#
# Usage (from ~/efda_scratch):  Rscript extract_hexagon_series.R <country_lower> [gpkg]
# Output: hex_rates/<country>.rds  — hex_id x year long table with lambda_natural
#
# Method follows extract_country_split.R (same rasters, same agent codes, same forest
# mask), but rasterises hex_id instead of a two-level biome zone, and does ONE multi-band
# zonal call over all 39 years rather than a per-year freq() loop.
suppressPackageStartupMessages({ library(terra); library(sf); library(dplyr) })

args    <- commandArgs(trailingOnly = TRUE)
country <- args[1]
if (is.na(country)) stop("Usage: Rscript extract_hexagon_series.R <country> [gpkg]")
# Same convention as extract_country_split.R: env var, repo-relative default, and fail loud
# rather than read an unreproducible absolute path.
GPKG <- if (length(args) >= 2) args[2] else
  Sys.getenv("JRC_GPKG", unset = "data/JRC-risk-model/crcf_risk_bp_maps.gpkg")
if (!file.exists(GPKG))
  stop("JRC gpkg not found at '", GPKG, "'. Pass it as the 2nd argument or set the ",
       "JRC_GPKG env var to its location. Source: Marinelli et al. (2026) JRC CRCF ",
       "risk model.", call. = FALSE)

MIN_FOREST_PIX <- 1000L      # ~900 ha; below this a hexagon rate is too noisy to correlate
YEARS <- 1985:2023
AGENT_NATURAL <- c(1L, 2L)   # 1 = wind/bark beetle, 2 = fire (3 = harvest, excluded)

agent_tif <- paste0("disturbance_agent_1985_2023_", country, ".tif")
fmask_tif <- paste0("forest_mask_", country, ".tif")
zip_path  <- paste0(country, ".zip")
if (!file.exists(agent_tif) || !file.exists(fmask_tif)) {
  if (!file.exists(zip_path)) stop("neither rasters nor zip present for ", country)
  cat("[", country, "] unzipping\n"); unzip(zip_path, files = c(agent_tif, fmask_tif), overwrite = TRUE)
}

agent <- rast(agent_tif)
fmask <- rast(fmask_tif)
if (nlyr(agent) != length(YEARS))
  stop("expected ", length(YEARS), " bands, got ", nlyr(agent), " for ", country)

# --- hexagons for this country, on the EFDA grid --------------------------------
hex <- st_read(GPKG, layer = "forest_type_data", quiet = TRUE)
hex <- hex[!duplicated(hex$hex_id), c("hex_id", "bioregion")]     # one row per hexagon
hex <- st_transform(hex, crs(agent))
# Keep hexagons overlapping this country's raster extent; the forest mask then decides
# which actually contain forest, so no country polygon is needed.
hex <- hex[st_intersects(hex, st_as_sfc(st_bbox(ext(agent), crs = st_crs(hex))),
                         sparse = FALSE)[, 1], ]
if (!nrow(hex)) stop("no hexagons overlap the raster extent for ", country)
hexr <- rasterize(vect(hex), agent[[1]], field = "hex_id", background = NA)

# --- forest pixels per hexagon --------------------------------------------------
fpix <- zonal(fmask, hexr, fun = "sum", na.rm = TRUE)
names(fpix) <- c("hex_id", "forest_pix")
# zonal returns NA for a hexagon with no forest pixel under it. That is a genuine zero, not
# missing data, so make it explicit rather than letting NA propagate into the keep index.
fpix$forest_pix[is.na(fpix$forest_pix)] <- 0
fpix <- fpix[!is.na(fpix$hex_id), ]
keep <- fpix$forest_pix >= MIN_FOREST_PIX
# Report the exclusion rather than filtering silently: these hexagons are mostly
# non-forest, and a rate computed on a handful of pixels is noise.
cat(sprintf("[%s] hexagons overlapping: %d | with >=%d forest pixels: %d (dropped %d)\n",
            country, nrow(fpix), MIN_FOREST_PIX, sum(keep), sum(!keep)))
if (!any(keep)) { cat("[", country, "] no hexagon passes the forest threshold; skipping\n"); quit(status = 0) }
fpix <- fpix[keep, ]

# --- natural-disturbance pixels per hexagon per year ----------------------------
# One multi-band zonal over a 0/1 natural-disturbance stack masked to forest.
nat <- (agent %in% AGENT_NATURAL) * fmask
zn  <- zonal(nat, hexr, fun = "sum", na.rm = TRUE)
names(zn) <- c("hex_id", as.character(YEARS))
zn <- zn[zn$hex_id %in% fpix$hex_id, ]

long <- do.call(rbind, lapply(as.character(YEARS), function(y) {
  data.frame(hex_id = zn$hex_id, year = as.integer(y),
             natural_pix = zn[[y]], stringsAsFactors = FALSE)
}))
long <- merge(long, fpix, by = "hex_id")
long <- merge(long, st_drop_geometry(hex), by = "hex_id")
long$country <- country
long$lambda_natural <- long$natural_pix / long$forest_pix
if (anyNA(long$lambda_natural)) stop("NA lambda_natural for ", country)

dir.create("hex_rates", showWarnings = FALSE)
saveRDS(long, file.path("hex_rates", paste0(country, ".rds")))
cat(sprintf("[%s] OK — %d hexagons x %d years; mean lambda_natural 1986-2023 %.3f%%/yr\n",
            country, length(unique(long$hex_id)), length(YEARS),
            100 * mean(long$lambda_natural[long$year >= 1986])))
