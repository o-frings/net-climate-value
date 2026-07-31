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

## Per-hexagon extraction (offline) — for the within-country correlation `c`

The tables above are country (or country × bioregion) aggregates, so they cannot identify the
**within**-country spatial correlation `c` that `03_buffer` uses to set `K = round(1/c)`
decorrelated cells. Estimating `c`, and testing it for drift, needs a sub-national grain. The
JRC 35 km hexagons are the natural choice: they are the unit the JRC risk model itself uses,
so an estimate on that grid is directly comparable to their product.

Scripts are tracked in `scripts/efda_offline/` alongside the other offline extraction code and
are run from the scratch directory holding the rasters (the ~58 GB of GeoTIFFs are not in the
repo):

| script | role |
|---|---|
| `extract_hexagon_series.R` | one country → `hex_rates/<country>.rds`; rasterises `hex_id` onto the EFDA grid and does a single multi-band `zonal` over all 39 years |
| `hex_one.sh` | single-country wrapper (skips work already done, logs to `logs/`) |
| `process_hex_all.sh` | runs all countries, `JOBS` at a time, smallest rasters first so partial results accumulate |
| `combine_hex_rates.R` | concatenates `hex_rates/*.rds` → `analysis/data/processed/efda_hex_rates.rds` |

Re-run with:

```sh
cd ~/efda_scratch                # wherever the rasters live
cp <repo>/analysis/scripts/efda_offline/{extract_hexagon_series.R,hex_one.sh,process_hex_all.sh,combine_hex_rates.R} .
export JRC_GPKG=<repo>/data/JRC-risk-model/crcf_risk_bp_maps.gpkg
export NCV_ANALYSIS_DIR=<repo>/analysis
JOBS=5 ./process_hex_all.sh      # idempotent; ~2-3 h for the full set
Rscript combine_hex_rates.R
cd <repo>/analysis
Rscript engine/R/sens_within_country_correlation.R
```

Hexagons with fewer than `MIN_FOREST_PIX = 1000` forest pixels (~900 ha) are dropped and the
count reported, because a rate computed on a handful of pixels is noise. The combined file
**is** committed (`data/processed/efda_hex_rates.rds`), so the analysis is reproducible from
the repo without re-running the raster step.

### Schema (`efda_hex_rates.rds`)

```
hex_id          <dbl>   # JRC 35 km hexagon id (crcf_risk_bp_maps.gpkg, layer forest_type_data)
bioregion       <chr>   # JRC bioregion label for the hexagon
country         <chr>   # EFDA file key, lowercase (e.g. "germany")
year            <int>   # 1985-2023
natural_pix     <dbl>   # disturbed pixel count, agent codes 1 (wind/beetle) + 2 (fire)
forest_pix      <dbl>   # constant per hexagon, forest pixels under it
lambda_natural  <dbl>   # natural_pix / forest_pix (annual fraction)
```
