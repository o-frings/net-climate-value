#!/bin/bash
# =============================================================================
# sync_to_manuscript.sh  —  copy rebuilt figures + tables into the manuscript
# =============================================================================
# Mirrors the old analysis/sync_figures.sh, but sources the REBUILT outputs:
#   figures/output/*.{pdf,png}  -> manuscript_overleaf_clone/Figures/
#   figures/output/tables/*.tex -> manuscript_overleaf_clone/Tables/
#
# WARNING: this OVERWRITES the committed manuscript figures/tables with the
# rebuilt ones, whose numbers have SHIFTED (establishment-floor dropped -> higher
# afforestation NCV; Boreal conifer R 1.37->1.26). Do NOT run until the manuscript
# PROSE/numbers are being reconciled in the same pass (see P5_MANUSCRIPT_RESYNC.md),
# or the figures and text will be inconsistent. Run from analysis/:
#   bash figures/sync_to_manuscript.sh
# =============================================================================
set -e
MAN="../manuscript_overleaf_clone"
SRC="figures/output"
[ -d "$MAN" ] || { echo "ERROR: manuscript folder not found at $MAN" >&2; exit 1; }
mkdir -p "$MAN/Figures" "$MAN/Tables"

echo "Figures: $SRC/*.{pdf,png} -> $MAN/Figures/"
rsync -av "$SRC/"*.pdf "$MAN/Figures/" 2>/dev/null || true
rsync -av "$SRC/"*.png "$MAN/Figures/" 2>/dev/null || true
echo "Tables : $SRC/tables/*.tex -> $MAN/Tables/"
rsync -av "$SRC/tables/"*.tex "$MAN/Tables/" 2>/dev/null || true

echo "Done. $(ls -1 "$MAN/Figures"/*.pdf | wc -l) figure PDFs, $(ls -1 "$MAN/Tables"/*.tex | wc -l) tables in manuscript."
echo "NEXT: reconcile main.tex numbers/captions per P5_MANUSCRIPT_RESYNC.md."
