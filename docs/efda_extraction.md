# EFDA per-country extraction (offline)

The country-level natural disturbance rates that feed `COUNTRY_PARAMS` are
extracted from the **European Forest Disturbance Atlas v2.1.1**
(Viana-Soto & Senf 2025), Zenodo: <https://doi.org/10.5281/zenodo.13333034>.

The extraction is done **offline** rather than as part of `02_data_extraction.R`
because the source data is ~58 GB of 30-m GeoTIFFs (per-country ZIPs), which is
too large to ship in the project repo. The committed artefacts are the
~140 KB summary `.rds` files in `data/processed/efda_country_rates/`.

## Pipeline summary

1. **Download** per-country ZIPs from Zenodo (`https://zenodo.org/api/records/13333034/files/<country>.zip/content`)
2. **Unzip** to extract `disturbance_agent_1985_2023_<country>.tif` and `forest_mask_<country>.tif`
3. **Per-year, per-agent zonal aggregation** with `terra::freq` on the masked
   agent stack — agent codes: `1` = wind/bark beetle, `2` = fire, `3` = harvest
4. **Filter to natural** (codes 1+2; harvest is excluded because it is
   contractually managed within CRCF projects, not a reversal source)
5. **Forest-area weighting** by FAO FRA 2020 country totals
6. **Optional bioregion split** for France and Italy using JRC hexagon
   bioregion attribution (`data/JRC-risk-model/crcf_risk_bp_maps.gpkg`)

The output of step 5 (per-country) and step 6 (sub-national zones) is one
`.rds` file per zone in `data/processed/efda_country_rates/`. `02_data_extraction.R`
Part E reads these, joins with Senf 2021 severity, and writes
`efda_country_summary.rds` which `03_parameters.R` consumes.

## Re-running the offline extraction

Scripts (originally in `~/efda_scratch/`):

- `extract_country.R` — single-country extraction, ~2-20 min per country
- `extract_country_split.R` — bioregion split (France, Italy only)
- `download_queue.sh` — parallel ZIP download with retry
- `process_all.sh` — idempotent wrapper, runs extraction on all available ZIPs

Total wall-clock for a clean rebuild: ~3-4 hours of download + ~1-2 hours of
extraction (laptop, single-threaded `terra::freq`).

## Schema (`efda_country_rates/<zone>.rds`)

```
country         <chr>   # e.g. "germany", "france_Temperate"
year            <int>   # 1985-2023
wind_beetle     <dbl>   # disturbed pixel count, code 1
fire            <dbl>   # disturbed pixel count, code 2
harvest         <dbl>   # disturbed pixel count, code 3
forest_pixels   <dbl>   # constant, total forest pixels in zone
forest_ha       <dbl>   # forest_pixels × 30² / 1e4
natural_pix     <dbl>   # wind_beetle + fire
all_pix         <dbl>   # wind_beetle + fire + harvest
lambda_natural  <dbl>   # natural_pix / forest_pixels (annual fraction)
lambda_all      <dbl>   # all_pix / forest_pixels (annual fraction, incl. harvest)
```
