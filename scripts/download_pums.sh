#!/usr/bin/env bash
#
# download_pums.sh
#
# Downloads ACS PUMS housing files for Section 2.3's US income
# comparator, and checks that the columns load_pums.R needs are
# present before you commit to a multi-gigabyte read.
#
# HOUSING FILES only. The person files are not needed: NP, NRC, NOC
# and NPF on the housing record are enough to reconstruct EPI's
# parents-by-children grid without a SERIALNO join. Downloading the
# person files roughly doubles the transfer for no benefit.
#
# Usage:
#   scripts/download_pums.sh              # list files, download nothing
#   scripts/download_pums.sh --download   # fetch, unzip, verify
#
# The default is a dry run. Census blocks clients that
# hammer the FTP site, and the exact filenames in the 5-year release
# are worth eyeballing before committing to the transfer.
#
# Written for bash 3.2, which is what macOS ships. That rules out
# mapfile/readarray and associative arrays.

set -euo pipefail

YEAR="2024"
PERIOD="5-Year"
BASE="https://www2.census.gov/programs-surveys/acs/data/pums"
BASE="${BASE}/${YEAR}/${PERIOD}"

OUTDIR="data/raw/pums"

# Housing files only. Covers both the single national archive
# (csv_hus.zip) and the split form (csv_husa.zip, csv_husb.zip, ...)
# that the 5-year release uses when the file is large.
PATTERN="csv_hus.*\.zip"

# Polite defaults. --limit-rate and --wait are what keep you under
# the rate limit; -c makes a dropped transfer resumable rather than
# restarting several gigabytes from zero.
WGET_OPTS=(
    --continue
    --tries=5
    --waitretry=30
    --retry-connrefused
    --limit-rate=5m
    --wait=2
    --random-wait
    --progress=dot:giga
    --user-agent="Mozilla/5.0 (academic research)"
)

# Columns load_pums.R reads. Checked against the unzipped header
# before any R runs, so a wrong file is caught in seconds rather
# than after a long read.
REQUIRED_COLS=(
    SERIALNO STATE PUMA TYPEHUGQ WGTP NP NRC NOC NPF ADJINC FINCP
    HINCP
)

log() { printf '%s\n' "$*" >&2; }
die() { log "ERROR: $*"; exit 1; }

command -v wget >/dev/null 2>&1 || die "wget not found."
command -v unzip >/dev/null 2>&1 || die "unzip not found."

log "Listing ${BASE} ..."
listing="$(wget -q -O - "${BASE}/" || true)"
[ -n "${listing}" ] || die "Could not read the directory listing. \
Open ${BASE}/ in a browser and check the path is still current."

# Read into an array with a while-read loop rather than mapfile:
# mapfile is a bash 4.0 builtin and macOS ships bash 3.2, so
# mapfile here fails with "command not found" on a stock Mac.
files=()
while IFS= read -r line; do
    [ -n "${line}" ] && files+=("${line}")
done < <(
    printf '%s' "${listing}" \
        | grep -oE 'href="[^"]+"' \
        | sed 's/href="//; s/"$//' \
        | grep -E "^${PATTERN}$" \
        | sort -u
)

[ "${#files[@]}" -gt 0 ] || die "No files matching ${PATTERN}. \
The release may name them differently; list the directory and \
adjust PATTERN."

log "Housing archives found:"
for f in "${files[@]}"; do log "  ${f}"; done

if [ "${1:-}" != "--download" ]; then
    log ""
    log "Dry run. Re-run with --download to fetch these."
    exit 0
fi

mkdir -p "${OUTDIR}"
cd "${OUTDIR}"

for f in "${files[@]}"; do
    log "Fetching ${f} ..."
    wget "${WGET_OPTS[@]}" "${BASE}/${f}"
done

for f in "${files[@]}"; do
    log "Unzipping ${f} ..."
    unzip -o -q "${f}"
done

csvs=()
while IFS= read -r line; do
    [ -n "${line}" ] && csvs+=("${line}")
done < <(find . -maxdepth 1 -name 'psam_h*.csv' | sort)
[ "${#csvs[@]}" -gt 0 ] || die "No psam_h*.csv after unzipping."

log ""
log "Unzipped:"
ls -lh psam_h*.csv >&2

# Verify the header of the first CSV carries every column the
# loader needs.
header="$(head -1 "${csvs[0]}")"
missing=()
for col in "${REQUIRED_COLS[@]}"; do
    printf '%s' "${header}" | tr ',' '\n' | grep -qx "${col}" \
        || missing+=("${col}")
done

if [ "${#missing[@]}" -gt 0 ]; then
    die "Missing column(s) in ${csvs[0]}: ${missing[*]}. \
This may be a person file rather than a housing file, or a \
different release."
fi

log ""
log "All required columns present in ${csvs[0]}."

# ADJINC should vary across a 5-year file, since the factor depends
# on interview year. A single distinct value means this is a 1-year
# file, which is too thin for state-by-family-type cells.
adjinc_col="$(
    printf '%s' "${header}" | tr ',' '\n' | grep -nx ADJINC | cut -d: -f1
)"
n_adjinc="$(
    tail -n +2 "${csvs[0]}" \
        | head -200000 \
        | cut -d, -f"${adjinc_col}" \
        | sort -u \
        | wc -l \
        | tr -d ' '
)"

if [ "${n_adjinc}" -le 1 ]; then
    log ""
    log "WARNING: ADJINC takes only ${n_adjinc} distinct value in the"
    log "first 200,000 rows. A 5-year file should show several, one"
    log "per interview year. Check this is not a 1-year release."
else
    log "ADJINC takes ${n_adjinc} distinct values: consistent with a"
    log "5-year file."
fi

log ""
log "Done. Point load_pums.R at ${OUTDIR}/psam_h*.csv"
