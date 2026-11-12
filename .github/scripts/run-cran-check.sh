#!/usr/bin/env bash
## Check the built tarball the way CRAN does, inside `nix-shell default.nix`.
##
## Usage (from a folder that holds CerebroNexus_*.tar.gz, outside the checkout):
##   run-cran-check.sh standard     # what R-CMD-check runs
##   run-cran-check.sh nosuggests   # Suggests packages hidden, 2 cores, as on CRAN's noSuggests flavor
set -eu

mode="${1:-standard}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

## R only flags tarballs above 10 MB and CRAN asks for 5 MB of data and
## documentation; stay clear of both so a PR that adds data cannot slip past.
max_tarball_bytes=8000000

unset CI NOT_CRAN NODE_PATH
export NODE_OPTIONS=--no-global-search-paths

case "$mode" in
  standard)
    # testthat runs 2 test processes by default; the runner has 4 cores.
    export TESTTHAT_CPUS=4
    ;;
  nosuggests)
    export _R_CHECK_DEPENDS_ONLY_=true
    export _R_CHECK_LIMIT_CORES_=true
    ;;
  *)
    echo "unknown mode: $mode" >&2
    exit 2
    ;;
esac

tarball="$(ls CerebroNexus_*.tar.gz)"
bytes="$(stat -c %s "$tarball")"
echo "$tarball: $bytes bytes (limit $max_tarball_bytes)"
if [ "$bytes" -gt "$max_tarball_bytes" ]; then
  echo "Tarball is larger than $max_tarball_bytes bytes; CRAN's guideline is 5 MB of data and documentation and 10 MB in total." >&2
  exit 1
fi

R CMD check --as-cran --no-manual "$tarball"
log=CerebroNexus.Rcheck/00check.log
test -f "$log"
Rscript "$here/check-log-notes.R" "$log"
