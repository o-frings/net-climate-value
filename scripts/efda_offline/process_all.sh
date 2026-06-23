#!/usr/bin/env bash
# Process all available zips: extract + aggregate.
# Idempotent — skips countries already in country_rates/.
set -u
cd "$(dirname "$0")"

mkdir -p country_rates

for zip in *.zip; do
  [[ -f "$zip" ]] || continue
  c="${zip%.zip}"
  out="country_rates/${c}.rds"
  if [[ -f "$out" ]]; then
    echo "skip $c (already processed)"
    continue
  fi
  echo "==> processing $c"
  Rscript extract_country.R "$c" 2>&1 | tail -6
done

echo
echo "==> aggregating to biomes"
Rscript aggregate_biome.R
