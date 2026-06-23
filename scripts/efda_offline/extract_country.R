# Per-country EFDA extraction.
# Reads agent stack + forest mask, computes annual disturbance fraction by
# agent (1=wind/bark-beetle, 2=fire, 3=harvest), saves per-country tibble.
#
# Usage: Rscript extract_country.R <country_lowercase>
#   e.g. Rscript extract_country.R france

suppressPackageStartupMessages({library(terra); library(dplyr); library(tidyr)})

args    <- commandArgs(trailingOnly = TRUE)
country <- args[1]
if (is.na(country)) stop("Usage: Rscript extract_country.R <country>")

zip_path   <- paste0(country, ".zip")
out_path   <- paste0("country_rates/", country, ".rds")
agent_tif  <- paste0("disturbance_agent_1985_2023_", country, ".tif")
fmask_tif  <- paste0("forest_mask_", country, ".tif")

dir.create("country_rates", showWarnings = FALSE)

# Unzip if needed
if (!file.exists(agent_tif) || !file.exists(fmask_tif)) {
  if (!file.exists(zip_path)) stop("Zip not found: ", zip_path)
  cat(sprintf("Unzipping %s...\n", zip_path))
  unzip(zip_path, files = c(agent_tif, fmask_tif), overwrite = TRUE)
}

agent <- rast(agent_tif)
fmask <- rast(fmask_tif)
years <- 1985:2023
if (nlyr(agent) != length(years))
  stop("Expected 39 layers in agent stack, got ", nlyr(agent))

t0 <- Sys.time()
forest_pixels <- as.numeric(global(fmask, "sum", na.rm = TRUE))
forest_ha <- forest_pixels * 900 / 1e4

cat(sprintf("[%s] Forest pixels: %s (%.2f Mha at 30m)\n",
            country, format(forest_pixels, big.mark = ","),
            forest_ha / 1e6))

# Per-year, per-agent counts (mask × agent, then freq)
out <- list()
for (yi in seq_along(years)) {
  band <- agent[[yi]] * fmask
  f <- freq(band) |> as.data.frame()
  for (a in 1:3) if (!a %in% f$value)
    f <- rbind(f, data.frame(layer = 1, value = a, count = 0))
  out[[yi]] <- tibble(
    country     = country,
    year        = years[yi],
    wind_beetle = f$count[f$value == 1],
    fire        = f$count[f$value == 2],
    harvest     = f$count[f$value == 3]
  )
}

ann <- bind_rows(out) |>
  mutate(
    forest_pixels  = forest_pixels,
    forest_ha      = forest_ha,
    natural_pix    = wind_beetle + fire,
    all_pix        = wind_beetle + fire + harvest,
    lambda_natural = natural_pix / forest_pixels,
    lambda_all     = all_pix     / forest_pixels
  )

saveRDS(ann, out_path)

elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("[%s] Done in %.1fs.\n", country, elapsed))
cat(sprintf("[%s] lambda_natural 1986-2017: %.3f%%/yr\n", country,
            mean(ann$lambda_natural[ann$year >= 1986 & ann$year <= 2017]) * 100))
cat(sprintf("[%s] lambda_natural 2018-2023: %.3f%%/yr\n", country,
            mean(ann$lambda_natural[ann$year >= 2018]) * 100))
cat(sprintf("[%s] lambda_natural 1986-2023: %.3f%%/yr\n", country,
            mean(ann$lambda_natural[ann$year >= 1986]) * 100))
cat(sprintf("[%s] Saved -> %s\n", country, out_path))
