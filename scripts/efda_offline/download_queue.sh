#!/usr/bin/env bash
# EFDA download queue: skip-if-exists, retry on failure, log progress
set -u
cd "$(dirname "$0")"
LOG="download.log"
BASE="https://zenodo.org/api/records/13333034/files"

# Priority order: temperate (most-needed) first, then Med, Boreal, UK, smaller
COUNTRIES=(
  # Temperate (~80% biome forest area)
  france germany poland austria romania hungary slovakia
  belgium netherlands denmark switzerland slovenia
  # Mediterranean (~80%)
  spain italy greece portugal croatia bulgaria
  # Boreal (~80%, BY/UA excluded as non-EU)
  sweden finland estonia latvia lithuania
  # UK biome (separate)
  unitedkingdom ireland
  # Lower priority extras (smaller/peripheral)
  bosniaherzegovina serbia macedonia albania montenegro
)

for c in "${COUNTRIES[@]}"; do
  zip="${c}.zip"
  if [[ -f "$zip" && $(stat -f%z "$zip") -gt 0 ]]; then
    echo "[$(date +%H:%M:%S)] SKIP $zip (already present)" >> "$LOG"
    continue
  fi
  echo "[$(date +%H:%M:%S)] START $zip" >> "$LOG"
  for attempt in 1 2 3; do
    if curl -L --fail --retry 3 --retry-delay 30 -C - \
            -o "$zip" "$BASE/$zip/content"; then
      sz=$(stat -f%z "$zip")
      echo "[$(date +%H:%M:%S)] OK $zip ($((sz/1024/1024)) MB)" >> "$LOG"
      break
    else
      echo "[$(date +%H:%M:%S)] FAIL $zip attempt $attempt" >> "$LOG"
      sleep 60
    fi
  done
done
echo "[$(date +%H:%M:%S)] QUEUE COMPLETE" >> "$LOG"
