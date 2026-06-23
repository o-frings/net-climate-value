# Extract a country split into sub-national zones by JRC hexagon bioregion.
# Used for France and Italy which span multiple biogeographic regions.
#
# Usage: Rscript extract_country_split.R <country_lower>
#
# Output: country_rates/<country>_Temperate.rds, country_rates/<country>_Mediterranean.rds

suppressPackageStartupMessages({
  library(terra); library(sf); library(dplyr); library(tidyr); library(rnaturalearth)
})

args    <- commandArgs(trailingOnly = TRUE)
country <- args[1]
if (is.na(country)) stop("Usage: Rscript extract_country_split.R <country>")

# Country name lookup for rnaturalearth
ne_name <- c(france = "France", italy = "Italy")[country]
if (is.na(ne_name)) stop("Country not configured for split: ", country)

agent_tif <- paste0("disturbance_agent_1985_2023_", country, ".tif")
fmask_tif <- paste0("forest_mask_", country, ".tif")
zip_path  <- paste0(country, ".zip")

if (!file.exists(agent_tif) || !file.exists(fmask_tif)) {
  if (!file.exists(zip_path)) stop("Zip not found: ", zip_path)
  cat("Unzipping...\n")
  unzip(zip_path, files = c(agent_tif, fmask_tif), overwrite = TRUE)
}

cat("[", country, "] Loading rasters...\n")
agent <- rast(agent_tif)
fmask <- rast(fmask_tif)

# Load JRC hexagons + filter to country.
# Path is configurable via the JRC_GPKG env var; default is the canonical
# in-repo deposit location. The JRC CRCF risk maps (Marinelli et al. 2026) are
# not redistributable in-repo by default — deposit crcf_risk_bp_maps.gpkg at the
# path below (or point JRC_GPKG at it) before regenerating the France/Italy
# sub-zone splits. Fail loud rather than read an unreproducible absolute path.
jrc_gpkg <- Sys.getenv("JRC_GPKG",
                       unset = "data/JRC-risk-model/crcf_risk_bp_maps.gpkg")
if (!file.exists(jrc_gpkg)) {
  stop("JRC gpkg not found at '", jrc_gpkg, "'. Set the JRC_GPKG env var to ",
       "its location, or deposit crcf_risk_bp_maps.gpkg there. Source: ",
       "Marinelli et al. (2026) JRC CRCF risk model.", call. = FALSE)
}
cat("[", country, "] Loading JRC hexagons + country boundary from ", jrc_gpkg, "...\n")
jrc_hex <- st_read(jrc_gpkg, layer = "forest_type_data", quiet = TRUE) |>
  st_transform(3035)

# Dedup: forest_type_data has one row per hex × forest_type — keep one per hex
jrc_hex_unique <- jrc_hex |>
  group_by(hex_id) |> slice_head(n = 1) |> ungroup()

country_poly <- ne_countries(country = ne_name, scale = 50, returnclass = "sf") |>
  st_transform(3035)

country_hex <- st_intersection(jrc_hex_unique, country_poly) |>
  mutate(biome_zone = ifelse(bioregion == "Mediterranean",
                             "Mediterranean", "Temperate"))

cat(sprintf("[%s] Hexagon bioregion distribution within country:\n", country))
print(country_hex |> st_drop_geometry() |> count(bioregion, biome_zone))

# Rasterize biome zone onto EFDA grid
biome_rast <- rasterize(vect(country_hex), agent[[1]], field = "biome_zone",
                        background = NA)

years <- 1985:2023
zones <- c("Temperate", "Mediterranean")
all_out <- list()

for (zone in zones) {
  cat(sprintf("\n[%s/%s] Computing zonal stats...\n", country, zone))
  # Mask: forest pixels within this zone
  zone_mask <- (biome_rast == zone) * fmask
  zone_pixels <- as.numeric(global(zone_mask, "sum", na.rm = TRUE))
  if (is.na(zone_pixels) || zone_pixels < 1000) {
    cat(sprintf("  zone has too few pixels (%s); skipping\n", zone_pixels))
    next
  }
  cat(sprintf("  forest pixels in %s zone: %d (%.2f Mha)\n",
              zone, zone_pixels, zone_pixels * 900 / 1e10))

  out <- list()
  for (yi in seq_along(years)) {
    band <- agent[[yi]] * zone_mask
    f <- freq(band) |> as.data.frame()
    for (a in 1:3) if (!a %in% f$value)
      f <- rbind(f, data.frame(layer = 1, value = a, count = 0))
    out[[yi]] <- tibble(country = paste(country, zone, sep = "_"),
                        year = years[yi],
                        wind_beetle = f$count[f$value == 1],
                        fire        = f$count[f$value == 2],
                        harvest     = f$count[f$value == 3])
  }
  ann <- bind_rows(out) |>
    mutate(forest_pixels = zone_pixels,
           forest_ha = zone_pixels * 900 / 1e4,
           natural_pix = wind_beetle + fire,
           all_pix = wind_beetle + fire + harvest,
           lambda_natural = natural_pix / zone_pixels,
           lambda_all     = all_pix     / zone_pixels)
  out_path <- sprintf("country_rates/%s_%s.rds", country, zone)
  saveRDS(ann, out_path)
  cat(sprintf("[%s/%s] lambda_natural 1986-2023: %.3f%%/yr (n=%d hex pixels)\n",
              country, zone,
              mean(ann$lambda_natural[ann$year >= 1986]) * 100, zone_pixels))
  cat(sprintf("[%s/%s] Saved -> %s\n", country, zone, out_path))
  all_out[[zone]] <- ann
}
