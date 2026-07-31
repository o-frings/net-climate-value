#!/usr/bin/env bash
# Extract one country to hex_rates/, skipping if already done. Called by process_hex_all.sh.
set -u
cd "$(dirname "$0")"
c="$1"
[ -f "hex_rates/${c}.rds" ] && { echo "skip ${c} (done)"; exit 0; }
start=$(date +%s)
if Rscript extract_hexagon_series.R "$c" > "logs/hex_${c}.log" 2>&1; then
  echo "ok   ${c} ($(( $(date +%s) - start ))s)"
else
  echo "FAIL ${c} — see logs/hex_${c}.log"
fi
