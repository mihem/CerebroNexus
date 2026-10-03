#!/usr/bin/env bash

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd -P)"
BENCH_ROOT="$REPO/tests/bench"
CACHE_ROOT="${BENCH_CACHE_ROOT:-${XDG_CACHE_HOME:-$HOME/.cache}/cerebronexus-benchmark}"
STATE_DIR="$CACHE_ROOT/runner"
PID_FILE="$STATE_DIR/benchmark.pid"
EXIT_FILE="$STATE_DIR/benchmark.exit"
LOG_FILE="$STATE_DIR/benchmark.log"
SCRIPT="$BENCH_ROOT/benchmark/run.sh"

bench_sha256_file() {
  local path=$1
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$path" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$path" | awk '{print $1}'
  elif command -v openssl >/dev/null 2>&1; then
    openssl dgst -sha256 "$path" | awk '{print $NF}'
  else
    echo "no SHA-256 implementation found" >&2
    return 1
  fi
}

bench_fetch_source() {
  local url=$1
  local expected_bytes=$2
  local scratch_dir=$3
  local expected_sha=${4:-}
  local name
  local file
  local part
  local bytes
  local sha
  local recorded_sha
  local cache_root
  name=$(basename "${url%%\?*}")
  mkdir -p "$scratch_dir"

  if [ -n "${BENCH_SOURCE_CACHE:-}" ]; then
    mkdir -p "$BENCH_SOURCE_CACHE"
    cache_root=$(cd "$BENCH_SOURCE_CACHE" && pwd) || return 1
    file="$cache_root/$name"
    if [ -f "$file" ]; then
      if [ ! -f "$file.sha256" ]; then
        echo "cached source has no SHA-256 sidecar: $file" >&2
        return 1
      fi
      recorded_sha=$(tr -d '[:space:]' < "$file.sha256")
      sha=$(bench_sha256_file "$file") || return 1
      bytes=$(wc -c < "$file" | tr -d '[:space:]')
      if [ "$sha" != "$recorded_sha" ] || [ "$bytes" != "$expected_bytes" ]; then
        echo "cached source failed checksum or size validation: $file" >&2
        return 1
      fi
      if [ -n "$expected_sha" ] && [ "$sha" != "$expected_sha" ]; then
        echo "cached source differs from the pinned SHA-256: $file" >&2
        return 1
      fi
    else
      part="$file.part"
      curl -fL --retry 3 --retry-delay 5 --continue-at - \
        -o "$part" "$url" || return 1
      bytes=$(wc -c < "$part" | tr -d '[:space:]')
      if [ "$bytes" != "$expected_bytes" ]; then
        echo "downloaded source has unexpected size: $bytes != $expected_bytes" >&2
        return 1
      fi
      sha=$(bench_sha256_file "$part") || return 1
      if [ -n "$expected_sha" ] && [ "$sha" != "$expected_sha" ]; then
        echo "downloaded source differs from the pinned SHA-256" >&2
        return 1
      fi
      mv "$part" "$file"
      printf '%s\n' "$sha" > "$file.sha256.tmp"
      mv "$file.sha256.tmp" "$file.sha256"
    fi
    BENCH_FETCHED_FILE="$scratch_dir/$name"
    ln -sfn "$file" "$BENCH_FETCHED_FILE"
  else
    BENCH_FETCHED_FILE="$scratch_dir/$name"
    part="$BENCH_FETCHED_FILE.part"
    curl -fL --retry 3 --retry-delay 5 --continue-at - \
      -o "$part" "$url" || return 1
    bytes=$(wc -c < "$part" | tr -d '[:space:]')
    if [ "$bytes" != "$expected_bytes" ]; then
      echo "downloaded source has unexpected size: $bytes != $expected_bytes" >&2
      return 1
    fi
    sha=$(bench_sha256_file "$part") || return 1
    if [ -n "$expected_sha" ] && [ "$sha" != "$expected_sha" ]; then
      echo "downloaded source differs from the pinned SHA-256" >&2
      return 1
    fi
    mv "$part" "$BENCH_FETCHED_FILE"
  fi

  BENCH_FETCHED_BYTES=$bytes
  BENCH_FETCHED_SHA256=$sha
}

run_profile_engine() (
set +e

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
export BENCH_ROOT="$REPO/tests/bench"
RESULT_ROOT="${BENCH_RESULT_ROOT:-$BENCH_ROOT/results/benchmark}"
export BENCH_PROFILE="${BENCH_PROFILE:-quick}"
export BENCH_THREADS="${BENCH_THREADS:-1}"

case "$BENCH_THREADS" in
  ''|*[!0-9]*|0)
    echo "BENCH_THREADS must be a positive integer" >&2
    exit 1
    ;;
esac
export OMP_NUM_THREADS="$BENCH_THREADS"
export OPENBLAS_NUM_THREADS="$BENCH_THREADS"
export MKL_NUM_THREADS="$BENCH_THREADS"
export VECLIB_MAXIMUM_THREADS="$BENCH_THREADS"
export BLIS_NUM_THREADS="$BENCH_THREADS"
export RCPP_PARALLEL_NUM_THREADS="$BENCH_THREADS"

SCRATCH_PARENT="${BENCH_SCRATCH_PARENT:-${TMPDIR:-/tmp}}"
mkdir -p "$SCRATCH_PARENT"
SCRATCH="$(mktemp -d "$SCRATCH_PARENT/cerebro-bench.XXXXXX")" || exit 1
SCRATCH_MARKER="$SCRATCH/.cerebro-benchmark-scratch"
: > "$SCRATCH_MARKER"
export BENCH_SCRATCH="$SCRATCH"
export BENCH_LIB="$SCRATCH/rlib"
# Benchmark evidence must not load packages or startup hooks from the caller's
# personal R installation. Nix-provided site libraries remain available.
export R_ENVIRON_USER=/dev/null
export R_PROFILE_USER=/dev/null
export NOT_CRAN=true
export R_LIBS_USER="$SCRATCH/r-user-library"
export BENCH_RUN_ID="${BENCH_RUN_ID:-$(date -u +%Y%m%dT%H%M%SZ)-$(git -C "$REPO" rev-parse --short=12 HEAD)-$BENCH_PROFILE}"

STAGE="$SCRATCH/result"
LOG_DIR="$STAGE/logs"
SCHEDULE="$STAGE/05_schedule.csv"
SCHEDULE_TSV="$SCRATCH/05_schedule.tsv"
EXPORT_CSV="$STAGE/10_export.csv"
ACCESS_CSV="$STAGE/20_access.csv"
CRASH_CSV="$STAGE/crashes.csv"
SOURCE_MANIFEST="$STAGE/source_manifest.csv"
QUERY_PLAN_MANIFEST="$STAGE/query_plan_manifest.csv"
QUERY_PANEL="$STAGE/query_panel.csv"

cleanup() {
  local code=$?
  trap - EXIT INT TERM
  if [ "${BENCH_KEEP:-0}" = "1" ]; then
    echo "==> keeping scratch at $SCRATCH (BENCH_KEEP=1)"
  elif [ "$code" -ne 0 ] && [ "${BENCH_KEEP_ON_FAILURE:-1}" = "1" ]; then
    echo "==> keeping failed-run scratch at $SCRATCH (BENCH_KEEP_ON_FAILURE=1)"
  elif [ -f "$SCRATCH_MARKER" ] && [[ "$(basename "$SCRATCH")" == cerebro-bench.* ]]; then
    echo "==> removing scratch $SCRATCH"
    rm -rf -- "$SCRATCH"
  else
    echo "!! refusing to remove unverified scratch path: $SCRATCH" >&2
    code=1
  fi
  exit "$code"
}
trap cleanup EXIT INT TERM

mkdir -p "$STAGE" "$LOG_DIR" "$SCRATCH/sources" "$SCRATCH/query-plans" \
  "$BENCH_LIB" "$R_LIBS_USER"
printf '%s\n' 'run_id,profile,source,n_cells,backend,export_repeat,order_position,stage,exit_code' > "$CRASH_CSV"
printf '%s\n' 'run_id,source,url,bytes,sha256' > "$SOURCE_MANIFEST"

echo "==> run:      $BENCH_RUN_ID"
echo "==> profile:  $BENCH_PROFILE"
echo "==> scratch:  $SCRATCH"
echo "==> results:  $RESULT_ROOT"
df -h "$SCRATCH" | tail -1

echo "==> inspecting source dimensions (metadata only)"
Rscript "$BENCH_ROOT/benchmark/cli.R" inspect "$STAGE/00_probe.csv" 2>&1 \
  | tee "$LOG_DIR/probe.log" || exit 1
Rscript "$BENCH_ROOT/benchmark/cli.R" environment "$STAGE/run_manifest.csv" || exit 1
Rscript "$BENCH_ROOT/benchmark/cli.R" plan "$SCHEDULE" "$SCHEDULE_TSV" || exit 1

RESOURCE_COMMAND="resources"
BUILD_COMMAND="export"
if [ "$BENCH_PROFILE" = "scale" ] || \
   [ "$BENCH_PROFILE" = "full" ] || [ "$BENCH_PROFILE" = "panel_c2" ]; then
  RESOURCE_COMMAND="full-resources"
  BUILD_COMMAND="build-full"
fi

echo "==> checking whether this machine can run the plan"
Rscript "$BENCH_ROOT/benchmark/cli.R" "$RESOURCE_COMMAND" \
  "$STAGE/00_probe.csv" "$SCHEDULE" "$STAGE/run_manifest.csv" \
  "$STAGE/resource_check.csv" || exit 1

echo "==> installing branch under test into $BENCH_LIB"
R CMD INSTALL --no-docs --no-byte-compile --library="$BENCH_LIB" "$REPO" \
  > "$LOG_DIR/install.log" 2>&1 || {
  echo "!! install failed, see $LOG_DIR/install.log"
  tail -20 "$LOG_DIR/install.log"
  exit 1
}

SOURCES=$(Rscript -e 'source(file.path(Sys.getenv("BENCH_ROOT"), "benchmark", "core.R")); cat(bench_active_sources(), sep="\n")')

for src in $SOURCES; do
  url=$(Rscript -e "source(file.path(Sys.getenv('BENCH_ROOT'), 'benchmark', 'core.R')); cat(BENCH_SOURCES[['$src']]\$url)")
  expected_bytes=$(Rscript -e "source(file.path(Sys.getenv('BENCH_ROOT'), 'benchmark', 'core.R')); cat(BENCH_SOURCES[['$src']]\$expected_bytes)")
  expected_sha=$(Rscript -e "source(file.path(Sys.getenv('BENCH_ROOT'), 'benchmark', 'core.R')); cat(BENCH_SOURCES[['$src']]\$expected_sha256)")

  echo "==> [$src] fetching $(basename "${url%%\?*}")"
  if ! bench_fetch_source \
    "$url" "$expected_bytes" "$SCRATCH/sources" "$expected_sha"; then
    echo "!! download failed for $src; validation will preserve the previous run"
    continue
  fi
  file=$BENCH_FETCHED_FILE
  bytes=$BENCH_FETCHED_BYTES
  sha256=$BENCH_FETCHED_SHA256
  printf '"%s","%s","%s",%s,"%s"\n' \
    "$BENCH_RUN_ID" "$src" "$url" "$bytes" "$sha256" >> "$SOURCE_MANIFEST"
  echo "    local copy: $bytes bytes, sha256 ${sha256:0:12}..."

  tiers=$(awk -F '\t' -v source="$src" '$2 == source {print $3}' \
    "$SCHEDULE_TSV" | sort -n -u)
  for tier in $tiers; do
    query_plan="$SCRATCH/query-plans/${src}_${tier}.rds"
    echo "==> [$src / $tier] preparing frozen query plan"
    Rscript "$BENCH_ROOT/benchmark/cli.R" query-plan \
      "$src" "$tier" "$SCRATCH" "$query_plan" "$QUERY_PLAN_MANIFEST" \
      "$QUERY_PANEL" \
      > "$LOG_DIR/query_plan_${src}_${tier}.log" 2>&1 || {
      tail -20 "$LOG_DIR/query_plan_${src}_${tier}.log"
      exit 1
    }
  done

  while IFS=$'\t' read -r profile row_source tier comparison export_repeat order_position backend access_repeats; do
    [ "$row_source" = "$src" ] || continue
    tag="${src}_${tier}_${backend}_r${export_repeat}"
    query_plan="$SCRATCH/query-plans/${src}_${tier}.rds"
    out_dir="$SCRATCH/export/$tag"
    crb="$out_dir/bench.crb"
    build_command="$BUILD_COMMAND"
    missing_ok=0
    if [ "$BENCH_PROFILE" = "scale" ] && \
       [ "$backend" = "embedded" ]; then
      build_command="export"
      missing_ok=1
    fi

    echo "==> [$tag] build (position $order_position)"
    Rscript "$BENCH_ROOT/benchmark/cli.R" "$build_command" \
      "$src" "$tier" "$backend" "$export_repeat" "$order_position" \
      "$SCRATCH" "$EXPORT_CSV" "$query_plan" \
      > "$LOG_DIR/export_$tag.log" 2>&1
    rc=$?
    tail -3 "$LOG_DIR/export_$tag.log" | sed 's/^/    /'
    if [ "$rc" -ne 0 ]; then
      echo "    !! export process died (exit $rc)"
      printf '"%s","%s","%s",%s,"%s",%s,%s,"export",%s\n' \
        "$BENCH_RUN_ID" "$BENCH_PROFILE" "$src" "$tier" "$backend" \
        "$export_repeat" "$order_position" "$rc" >> "$CRASH_CSV"
      if [ "${BENCH_KEEP:-0}" != "1" ]; then
        rm -rf -- "$out_dir"
      fi
      continue
    fi

    if [ ! -f "$crb" ]; then
      if [ "$missing_ok" = "1" ]; then
        if [ "${BENCH_KEEP:-0}" != "1" ]; then
          rm -rf -- "$out_dir"
        fi
        continue
      fi
      echo "    !! export succeeded but artifact is missing: $crb" >&2
      printf '"%s","%s","%s",%s,"%s",%s,%s,"export-artifact",1\n' \
        "$BENCH_RUN_ID" "$BENCH_PROFILE" "$src" "$tier" "$backend" \
        "$export_repeat" "$order_position" >> "$CRASH_CSV"
      exit 1
    fi

    for access_repeat in $(seq 1 "$access_repeats"); do
      echo "==> [$tag] access repeat $access_repeat/$access_repeats"
      Rscript "$BENCH_ROOT/benchmark/cli.R" access \
        "$src" "$tier" "$backend" "$export_repeat" "$order_position" \
        "$access_repeat" "$crb" "$ACCESS_CSV" "$query_plan" \
        > "$LOG_DIR/access_${tag}_a${access_repeat}.log" 2>&1
      rc=$?
      tail -2 "$LOG_DIR/access_${tag}_a${access_repeat}.log" | sed 's/^/    /'
      if [ "$rc" -ne 0 ]; then
        echo "    !! access process died (exit $rc)"
        printf '"%s","%s","%s",%s,"%s",%s,%s,"access-%s",%s\n' \
          "$BENCH_RUN_ID" "$BENCH_PROFILE" "$src" "$tier" "$backend" \
          "$export_repeat" "$order_position" "$access_repeat" "$rc" >> "$CRASH_CSV"
      fi
    done

    if [ "${BENCH_KEEP:-0}" != "1" ]; then
      rm -rf -- "$out_dir"
    fi
  done < "$SCHEDULE_TSV"

  if [ "${BENCH_KEEP:-0}" != "1" ]; then
    rm -f -- "$file"
  fi
done

echo "==> checking measurements"
Rscript "$BENCH_ROOT/benchmark/cli.R" validate "$STAGE" || exit 1

echo "==> writing report"
Rscript "$BENCH_ROOT/benchmark/cli.R" report "$STAGE" 2>&1 \
  | tee "$LOG_DIR/report.log" || exit 1

if [ "$BENCH_PROFILE" = "scale" ] || \
   [ "$BENCH_PROFILE" = "full" ] || [ "$BENCH_PROFILE" = "panel_c2" ]; then
  echo "==> drawing benchmark figures"
  Rscript "$BENCH_ROOT/benchmark/cli.R" figure "$STAGE" "$STAGE/figures" \
    > "$LOG_DIR/figures.log" 2>&1 || {
    tail -20 "$LOG_DIR/figures.log"
    exit 1
  }
fi

echo "==> checking report and figures"
Rscript "$BENCH_ROOT/benchmark/cli.R" evidence "$STAGE" || exit 1
Rscript "$BENCH_ROOT/benchmark/cli.R" check "$STAGE" || exit 1

echo "==> publishing immutable result run"
Rscript "$BENCH_ROOT/benchmark/cli.R" publish "$STAGE" "$RESULT_ROOT" "$BENCH_RUN_ID" || exit 1

echo "==> done: $RESULT_ROOT/runs/$BENCH_RUN_ID"
)

read_pid() {
  local pid=""
  [ -f "$PID_FILE" ] || return 1
  read -r pid < "$PID_FILE" || true
  case "$pid" in
    ''|*[!0-9]*) return 1 ;;
    *) printf '%s\n' "$pid" ;;
  esac
}

is_running() {
  local pid
  pid=$(read_pid) || return 1
  kill -0 "$pid" 2>/dev/null
}

show_status() {
  local code pid
  if is_running; then
    pid=$(read_pid)
    printf 'benchmark running: PID=%s\n' "$pid"
    ps -p "$pid" -o pid=,etime=,stat=,cmd= || true
  elif [ -f "$EXIT_FILE" ]; then
    read -r code < "$EXIT_FILE" || code="unknown"
    printf 'benchmark finished: exit=%s\n' "$code"
  else
    printf 'benchmark is not running\n'
  fi
  printf 'log: %s\n' "$LOG_FILE"
}

require_clean() {
  local dirty
  dirty=$(git -C "$REPO" status --porcelain --untracked-files=all -- \
    . ":(exclude,glob)tests/bench/results/**")
  [ -z "$dirty" ] || {
    printf '%s\n' "$dirty" >&2
    printf 'benchmark requires a clean Git worktree\n' >&2
    return 1
  }
}

run_profile() {
  local profile="$1"
  local result_name="$2"
  local head_sha study_id
  require_clean
  head_sha=$(git -C "$REPO" rev-parse HEAD)
  study_id="$(date -u +%Y%m%dT%H%M%SZ)-${head_sha:0:12}-$result_name"
  printf '==> running %s\n' "$result_name"
  BENCH_PROFILE="$profile" \
    BENCH_STUDY_ID="$study_id" \
    BENCH_RUN_ID="$study_id" \
    BENCH_RESULT_ROOT="$BENCH_ROOT/results/benchmark/$result_name" \
    "$SCRIPT" _profile
}

run_all() {
  run_profile full full
  run_profile scale scale
}

manifest_value() {
  Rscript -e 'm <- read.csv(commandArgs(TRUE)[1], stringsAsFactors = FALSE); value <- m$value[m$key == commandArgs(TRUE)[2]]; if (length(value) != 1L || is.na(value) || !nzchar(value)) quit(status = 1L); cat(value)' "$1" "$2"
}

resume_inside() {
  local scratch stage manifest result_name
  scratch="${BENCH_RECOVERY_SCRATCH:-}"
  [ -f "$scratch/.cerebro-benchmark-scratch" ] || {
    printf 'unsafe benchmark scratch directory: %s\n' "$scratch" >&2
    return 1
  }
  stage="$scratch/result"
  manifest="$stage/run_manifest.csv"
  [ -f "$manifest" ] || {
    printf 'missing run manifest: %s\n' "$manifest" >&2
    return 1
  }

  export BENCH_ROOT="$REPO/tests/bench"
  export BENCH_SCRATCH="$scratch"
  export BENCH_LIB="$scratch/rlib"
  export R_LIBS_USER="$scratch/r-user-library"
  export R_ENVIRON_USER=/dev/null
  export R_PROFILE_USER=/dev/null
  export NOT_CRAN=true
  export BENCH_RUN_ID="$(manifest_value "$manifest" run_id)"
  export BENCH_STUDY_ID="$(manifest_value "$manifest" study_id)"
  export BENCH_PROFILE="$(manifest_value "$manifest" profile)"
  case "$BENCH_PROFILE" in
    full|panel_c2) result_name=full ;;
    scale) result_name=scale ;;
    *)
      printf 'unsupported benchmark profile: %s\n' "$BENCH_PROFILE" >&2
      return 1
      ;;
  esac

  Rscript "$BENCH_ROOT/benchmark/cli.R" validate "$stage"
  Rscript "$BENCH_ROOT/benchmark/cli.R" report "$stage"
  mkdir -p "$stage/logs"
  Rscript "$BENCH_ROOT/benchmark/cli.R" figure "$stage" "$stage/figures" \
    > "$stage/logs/figures-resume.log" 2>&1
  Rscript "$BENCH_ROOT/benchmark/cli.R" evidence "$stage"
  Rscript "$BENCH_ROOT/benchmark/cli.R" check "$stage"
  Rscript "$BENCH_ROOT/benchmark/cli.R" publish \
    "$stage" "$BENCH_ROOT/results/benchmark/$result_name" "$BENCH_RUN_ID"
  printf '0\n' > "$EXIT_FILE"
  printf 'benchmark result published: %s\n' \
    "$BENCH_ROOT/results/benchmark/$result_name/runs/$BENCH_RUN_ID"
}

resume_run() {
  local scratch command
  [ "$#" -eq 1 ] && [ -n "$1" ] || {
    printf 'usage: %s resume <scratch-directory>\n' "$SCRIPT" >&2
    return 2
  }
  command -v nix-shell >/dev/null 2>&1 || {
    printf 'nix-shell is required\n' >&2
    return 1
  }
  scratch="$(cd "$1" && pwd -P)"
  [ -f "$scratch/.cerebro-benchmark-scratch" ] || {
    printf 'not a benchmark scratch directory: %s\n' "$scratch" >&2
    return 1
  }
  require_clean
  export BENCH_RECOVERY_SCRATCH="$scratch"
  printf -v command 'bash %q _resume' "$SCRIPT"
  nix-shell "$REPO/default.nix" -A shell --run "$command"
}

worker() {
  local code command target tmp_exit
  target="${1:-all}"
  case "$target" in
    all)
      printf '==> running full benchmark, then scale benchmark\n'
      ;;
    full|scale)
      printf '==> running %s benchmark\n' "$target"
      ;;
    *)
      printf 'unknown benchmark target: %s\n' "$target" >&2
      return 2
      ;;
  esac
  printf -v command 'bash %q _inside %q' "$SCRIPT" "$target"
  set +e
  nix-shell "$REPO/default.nix" -A shell --run "$command"
  code=$?
  set -e
  tmp_exit="$EXIT_FILE.$$"
  printf '%s\n' "$code" > "$tmp_exit"
  mv -f -- "$tmp_exit" "$EXIT_FILE"
  exit "$code"
}

[ "${BASH_SOURCE[0]}" = "$0" ] || return 0

ACTION="${1:-run}"
RUN_TARGET=all
case "$ACTION" in
  status)
    mkdir -p "$STATE_DIR"
    show_status
    exit 0
    ;;
  _worker)
    worker "${2:-all}"
    ;;
  _inside)
    case "${2:-all}" in
      all) run_all ;;
      full) run_profile full full ;;
      scale) run_profile scale scale ;;
      *) printf 'unknown benchmark target: %s\n' "$2" >&2; exit 2 ;;
    esac
    exit 0
    ;;
  _profile)
    run_profile_engine
    exit $?
    ;;
  _resume)
    resume_inside
    exit $?
    ;;
  resume)
    resume_run "${2:-}"
    exit $?
    ;;
  run)
    ;;
  full|scale)
    RUN_TARGET="$ACTION"
    ;;
  *)
    printf 'usage: %s [run|full|scale|status|resume <scratch-directory>]\n' \
      "$0" >&2
    exit 2
    ;;
esac

export BENCH_THREADS="${BENCH_THREADS:-1}"
export BENCH_SOURCE_CACHE="${BENCH_SOURCE_CACHE:-$CACHE_ROOT/sources}"
export BENCH_SCRATCH_PARENT="${BENCH_SCRATCH_PARENT:-$CACHE_ROOT/scratch}"
export BENCH_STORAGE_DESCRIPTION="${BENCH_STORAGE_DESCRIPTION:-host=$(hostname); local benchmark storage}"

mkdir -p "$STATE_DIR" "$BENCH_SOURCE_CACHE" "$BENCH_SCRATCH_PARENT"
for external_dir in "$STATE_DIR" "$BENCH_SOURCE_CACHE" "$BENCH_SCRATCH_PARENT"; do
  external_dir=$(cd "$external_dir" && pwd -P)
  case "$external_dir/" in
    "$REPO/"*)
      printf 'benchmark cache and scratch must be outside the Git checkout: %s\n' "$external_dir" >&2
      exit 1
      ;;
  esac
done
if is_running; then
  show_status
  exit 1
fi
command -v nix-shell >/dev/null 2>&1 || {
  printf 'nix-shell is required\n' >&2
  exit 1
}

require_clean

rm -f -- "$PID_FILE" "$EXIT_FILE" "$LOG_FILE"
nohup "$SCRIPT" _worker "$RUN_TARGET" > "$LOG_FILE" 2>&1 < /dev/null &
pid=$!
printf '%s\n' "$pid" > "$PID_FILE"
sleep 2
if ! kill -0 "$pid" 2>/dev/null; then
  tail -n 40 "$LOG_FILE" >&2 || true
  exit 1
fi

printf 'benchmark started: PID=%s\n' "$pid"
printf 'log: %s\n' "$LOG_FILE"
printf 'status: bash %s status\n' "$SCRIPT"
