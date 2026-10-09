#!/bin/bash
# =============================================================================
# sync_to_manuscript.sh  —  copy rebuilt figures + tables into the manuscript
# =============================================================================
# Mirrors the old analysis/sync_figures.sh, but sources the REBUILT outputs:
#   figures/output/*.pdf        -> paper/figures/  (vector PDFs; the manuscript embeds no PNGs)
#   figures/output/tables/*.tex -> paper/tables/
# Only files the manuscript folder already holds are updated. Outputs the manuscript
# does not use are listed and skipped, so a new figure or table enters the manuscript
# only when someone adds it there on purpose. Any failure stops the script.
#
# WARNING: this OVERWRITES the committed manuscript figures/tables with the
# rebuilt ones, whose numbers have SHIFTED (establishment-floor dropped -> higher
# afforestation NCV; Boreal conifer R 1.37->1.26). Do NOT run until the manuscript
# PROSE/numbers are being reconciled in the same pass (see docs/audits/P5_MANUSCRIPT_RESYNC.md),
# or the figures and text will be inconsistent. Run from analysis/:
#   bash figures/sync_to_manuscript.sh
# =============================================================================
set -euo pipefail
shopt -s nullglob
MAN="${MAN:-../paper}"
SRC="figures/output"
for d in "$MAN/figures" "$MAN/tables" "$SRC" "$SRC/tables"; do
  [ -d "$d" ] || { echo "ERROR: folder not found: $d" >&2; exit 1; }
done

# update <source dir> <glob> <manuscript dir>
update() {
  local src=$1 pat=$2 dest=$3 n=0 f b
  local files=("$src"/$pat)
  [ ${#files[@]} -gt 0 ] || { echo "ERROR: no $pat in $src" >&2; exit 1; }
  for f in "${files[@]}"; do
    b=$(basename "$f")
    if [ -e "$dest/$b" ]; then cp "$f" "$dest/$b"; n=$((n + 1))
    else echo "  skipped (not in the manuscript): $b"; fi
  done
  for f in "$dest"/$pat; do
    [ -e "$src/$(basename "$f")" ] || echo "  WARNING: in the manuscript but not produced: $(basename "$f")"
  done
  echo "$dest: $n files updated"
}

update "$SRC" "*.pdf" "$MAN/figures"
update "$SRC/tables" "*.tex" "$MAN/tables"
echo "NEXT: reconcile manuscript.tex numbers/captions per docs/audits/P5_MANUSCRIPT_RESYNC.md."
