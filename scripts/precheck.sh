#!/usr/bin/env bash
# Stable local checks with isolated workers and one combined failure summary.
set -uo pipefail

cd "$(git rev-parse --show-toplevel)"

precheck_mode="${1:-fast}"
logic_shards="${CEREBRO_PRECHECK_LOGIC_SHARDS:-4}"
browser_shards="${CEREBRO_PRECHECK_BROWSER_SHARDS:-2}"
precheck_root="$(mktemp -d "${TMPDIR:-/tmp}/cerebro-precheck.XXXXXX")"
logs_dir="$precheck_root/logs"
mkdir -p "$logs_dir"

failures=()
shard_pids=()
shard_labels=()
shard_logs=()

require_positive_integer() {
  local name="$1"
  local value="$2"
  if [[ ! "$value" =~ ^[1-9][0-9]*$ ]]; then
    echo "$name must be a positive integer, got: $value" >&2
    exit 2
  fi
}

run_logged_step() {
  local label="$1"
  shift
  local log="$logs_dir/${label//[^[:alnum:]_-]/_}.log"
  echo "[$label] starting"
  if "$@" >"$log" 2>&1; then
    echo "[$label] passed"
  else
    failures+=("$label")
    echo "[$label] failed"
    sed -n '1,240p' "$log"
  fi
}

run_parallel_group() {
  local group="$1"
  local shards="$2"
  local browser="${3:-false}"
  local shard
  local run_dir
  local log
  local label
  local index

  shard_pids=()
  shard_labels=()
  shard_logs=()

  for ((shard = 1; shard <= shards; shard += 1)); do
    label="$group-$shard-of-$shards"
    run_dir="$precheck_root/$label"
    log="$logs_dir/$label.log"
    mkdir -p "$run_dir/tmp" "$run_dir/cache" "$run_dir/artifacts"
    echo "[$label] starting"
    (
      export TMPDIR="$run_dir/tmp"
      export XDG_CACHE_HOME="$run_dir/cache"
      export CEREBRO_TEST_ARTIFACT_DIR="$run_dir/artifacts"
      if [[ "$browser" == "true" ]]; then
        export CEREBRO_RUN_BROWSER_TESTS=true
      fi
      Rscript scripts/run-test-shard.R \
        --group "$group" \
        --shard "$shard" \
        --shards "$shards"
    ) >"$log" 2>&1 &
    shard_pids+=("$!")
    shard_labels+=("$label")
    shard_logs+=("$log")
  done

  for index in "${!shard_pids[@]}"; do
    if wait "${shard_pids[$index]}"; then
      echo "[${shard_labels[$index]}] passed"
    else
      failures+=("${shard_labels[$index]}")
      echo "[${shard_labels[$index]}] failed"
      sed -n '1,240p' "${shard_logs[$index]}"
    fi
  done
}

run_docs() {
  local docs_source="$precheck_root/docs-source"
  local docs_destination="$precheck_root/pkgdown-site"
  local docs_files="$precheck_root/docs-files"
  local copy_log="$logs_dir/docs-copy.log"

  mkdir -p "$docs_source"
  git ls-files --cached --others --exclude-standard -z >"$docs_files"
  if ! rsync -a --from0 --files-from="$docs_files" ./ "$docs_source/" \
    >"$copy_log" 2>&1; then
    failures+=("docs-copy")
    echo "[docs-copy] failed"
    sed -n '1,240p' "$copy_log"
    return
  fi

  run_logged_step docs env \
    CEREBRO_DOCS_SOURCE="$docs_source" \
    CEREBRO_DOCS_DESTINATION="$docs_destination" \
    Rscript -e \
    'pkgdown::build_site_github_pages(pkg = Sys.getenv("CEREBRO_DOCS_SOURCE"), new_process = FALSE, install = TRUE, dest_dir = Sys.getenv("CEREBRO_DOCS_DESTINATION"))'
}

require_positive_integer CEREBRO_PRECHECK_LOGIC_SHARDS "$logic_shards"
require_positive_integer CEREBRO_PRECHECK_BROWSER_SHARDS "$browser_shards"

case "$precheck_mode" in
  air)
    if ! command -v air >/dev/null; then
      echo "air not found (brew install air)" >&2
      exit 2
    fi
    run_logged_step air air format --check .
    ;;
  fast)
    run_logged_step install R CMD INSTALL .
    run_parallel_group logic "$logic_shards"
    run_parallel_group process-sensitive 1
    ;;
  full)
    run_logged_step install R CMD INSTALL .
    run_parallel_group logic "$logic_shards"
    run_parallel_group process-sensitive 1
    run_parallel_group browser "$browser_shards" true
    run_logged_step package-check Rscript -e \
      "devtools::check(args = c('--no-tests'), vignettes = TRUE, error_on = 'warning')"
    ;;
  docs)
    run_docs
    ;;
  *)
    echo "Usage: scripts/precheck.sh [air|fast|full|docs]" >&2
    exit 2
    ;;
esac

if ((${#failures[@]})); then
  echo
  echo "Precheck failed in ${#failures[@]} step(s):"
  printf '  - %s\n' "${failures[@]}"
  echo "Full logs: $logs_dir"
  exit 1
fi

rm -rf "$precheck_root"
echo "Precheck passed: $precheck_mode"
