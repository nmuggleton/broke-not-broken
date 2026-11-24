#!/usr/bin/env bash
# =====================================================================
# download_scf.sh
#
# Fetches the Survey of Consumer Finances 2022 microdata needed by
# R/4_structural/figure_3/main.R into data/raw/scf/, and unpacks it.
#
# Four files:
#   scf2022s.zip        -> p22i6.dta      full public dataset, 5
#                                         implicates (X-variables)
#   scfp2022s.zip       -> rscfp2022.dta  summary extract, the Fed's
#                                         own debt aggregates
#   scf2022rw1s.zip     -> p22_rw1.dta    999 bootstrap replicate
#                                         weights, plus the mm
#                                         multiplicities
#   bulletin.macro.txt                    the Fed's SAS macro. Not
#                                         read by any R code, but the
#                                         CCBAL / OTHLOC / EDN_INST
#                                         definitions replicated in
#                                         load_scf_credit_price.R are
#                                         taken from it verbatim, so
#                                         it is kept alongside the
#                                         data as the reference.
#   codebk2022.txt                        codebook, for variable
#                                         lookups.
#
# Unpacked size is about 305 MB. Run from the repository root.
# =====================================================================

set -euo pipefail

BASE="https://www.federalreserve.gov/econres/files"
DEST="data/raw/scf"

mkdir -p "$DEST"

for f in scf2022s.zip scfp2022s.zip scf2022rw1s.zip \
         bulletin.macro.txt codebk2022.txt; do
    if [ -f "$DEST/$f" ]; then
        echo "have    $f"
        continue
    fi
    echo "fetching $f"
    curl -fSL --retry 3 --retry-delay 5 -o "$DEST/$f" "$BASE/$f"
done

for z in scf2022s.zip scfp2022s.zip scf2022rw1s.zip; do
    echo "unzipping $z"
    unzip -o -q "$DEST/$z" -d "$DEST"
done

echo
echo "checking expected files:"
missing=0
for f in p22i6.dta rscfp2022.dta p22_rw1.dta; do
    if [ -f "$DEST/$f" ]; then
        echo "  ok      $f"
    else
        echo "  MISSING $f"
        missing=1
    fi
done

if [ "$missing" -ne 0 ]; then
    echo
    echo "One or more expected .dta files are absent. The Federal" >&2
    echo "Reserve may have renamed a release; check" >&2
    echo "federalreserve.gov/econres/scfindex.htm before running" >&2
    echo "R/4_structural/figure_3/main.R." >&2
    exit 1
fi

echo
echo "done. R/4_structural/figure_3/main.R can now be run from the repo root."
