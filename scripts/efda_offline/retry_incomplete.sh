#!/usr/bin/env bash
# Retry any incomplete zips. Compares file size to expected from Zenodo API.
# Aggressive retry settings: 10 attempts, --retry-all-errors, no per-attempt max-time cap.
set -u
cd "$(dirname "$0")"
BASE="https://zenodo.org/api/records/13333034/files"
LOG="retry.log"

# Expected sizes (bytes) from Zenodo API, hardcoded
declare -A EXPECTED=(
  [albania.zip]=350000000
  [austria.zip]=1340000000
  [belarus.zip]=2770000000
  [belgium.zip]=230000000
  [bosniaherzegovina.zip]=800000000
  [bulgaria.zip]=1300000000
  [croatia.zip]=790000000
  [czechia.zip]=900000000
  [denmark.zip]=320000000
  [estonia.zip]=840000000
  [finland.zip]=4580000000
  [france.zip]=4840000000
  [germany.zip]=3120000000
  [greece.zip]=1630000000
  [hungary.zip]=660000000
  [ireland.zip]=260000000
  [italy.zip]=3150000000
  [latvia.zip]=1150000000
  [lithuania.zip]=670000000
  [macedonia.zip]=370000000
  [moldova.zip]=100000000
  [montenegro.zip]=290000000
  [netherlands.zip]=220000000
  [norway.zip]=2270000000
  [poland.zip]=3240000000
  [portugal.zip]=1510000000
  [romania.zip]=2210000000
  [serbia.zip]=1200000000
  [slovakia.zip]=590000000
  [slovenia.zip]=330000000
  [spain.zip]=5470000000
  [sweden.zip]=6430000000
  [switzerland.zip]=430000000
  [ukraine.zip]=2910000000
  [unitedkingdom.zip]=980000000
)

for zip in "${!EXPECTED[@]}"; do
  expected=${EXPECTED[$zip]}
  if [[ -f "$zip" ]]; then
    actual=$(stat -f%z "$zip")
    # Within 5% of expected = complete
    threshold=$((expected * 95 / 100))
    if [[ $actual -ge $threshold ]]; then
      continue
    fi
    echo "[$(date +%H:%M:%S)] INCOMPLETE $zip ($actual / $expected bytes, $((actual*100/expected))%)" >> "$LOG"
  else
    echo "[$(date +%H:%M:%S)] MISSING $zip" >> "$LOG"
  fi
  echo "[$(date +%H:%M:%S)] RETRY $zip" >> "$LOG"
  for attempt in {1..15}; do
    if curl -L --fail --retry 5 --retry-delay 60 --retry-all-errors -C - \
            -o "$zip" "$BASE/$zip/content"; then
      sz=$(stat -f%z "$zip")
      echo "[$(date +%H:%M:%S)] OK $zip ($((sz/1024/1024)) MB, attempt $attempt)" >> "$LOG"
      break
    else
      echo "[$(date +%H:%M:%S)] FAIL $zip attempt $attempt" >> "$LOG"
      sleep 90
    fi
  done
done
echo "[$(date +%H:%M:%S)] RETRY SWEEP COMPLETE" >> "$LOG"
