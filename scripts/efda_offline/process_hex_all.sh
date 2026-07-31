#!/usr/bin/env bash
# Per-hexagon EFDA extraction for all available countries.
#
# Idempotent: skips any country already in hex_rates/. Runs JOBS countries concurrently
# (default 4 — terra is memory-hungry, so this is well below the core count on purpose).
# Countries are processed SMALLEST RASTER FIRST so partial results accumulate quickly and
# the drift analysis can run on whatever has completed.
#
# Usage:  ./process_hex_all.sh            # 4 concurrent
#         JOBS=6 ./process_hex_all.sh     # more concurrency
set -u
cd "$(dirname "$0")"
mkdir -p hex_rates logs

JOBS=${JOBS:-4}
echo "=== per-hexagon extraction: ${JOBS} concurrent, smallest rasters first ==="

# -S sorts by size descending, -r reverses it -> ascending
ls -S -r disturbance_agent_1985_2023_*.tif 2>/dev/null \
  | sed -E 's/^disturbance_agent_1985_2023_(.*)\.tif$/\1/' \
  | xargs -P "$JOBS" -n 1 ./hex_one.sh

echo "=== done: $(ls hex_rates/*.rds 2>/dev/null | wc -l | tr -d ' ') of $(ls disturbance_agent_1985_2023_*.tif | wc -l | tr -d ' ') countries extracted ==="
