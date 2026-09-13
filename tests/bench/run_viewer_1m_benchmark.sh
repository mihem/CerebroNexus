#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
before_ref="${1:-27303f21}"
after_ref="${2:-HEAD}"
output_dir="${3:-$repo_root/tests/bench/scratch/viewer-1m-$(date +%Y%m%d-%H%M%S)}"
repeats="${REPEATS:-3}"
cache_dir="${CEREBRO_LARGE_CACHE:-}"

before_sha="$(git -C "$repo_root" rev-parse "${before_ref}^{commit}")"
after_sha="$(git -C "$repo_root" rev-parse "${after_ref}^{commit}")"
pr165_sha="$(git -C "$repo_root" rev-parse '69893a2b^{commit}')"
worktree_root="$(mktemp -d "${TMPDIR:-/tmp}/cerebronexus-viewer-1m.XXXXXX")"
before_root="$worktree_root/before"
after_root="$worktree_root/after"

cleanup() {
  git -C "$repo_root" worktree remove "$before_root" >/dev/null 2>&1 || true
  git -C "$repo_root" worktree remove "$after_root" >/dev/null 2>&1 || true
  rmdir "$worktree_root" >/dev/null 2>&1 || true
}
trap cleanup EXIT

if [[ -d "$output_dir" && -n "$(ls -A "$output_dir")" ]]; then
  printf 'output directory is not empty: %s\n' "$output_dir" >&2
  exit 1
fi
mkdir -p "$output_dir"
output_dir="$(cd "$output_dir" && pwd -P)"
git -C "$repo_root" worktree add --detach "$before_root" "$before_sha"
git -C "$repo_root" worktree add --detach "$after_root" "$after_sha"

export CEREBRO_BENCH_REPO_ROOT="$repo_root"
export CEREBRO_LARGE_CACHE="$cache_dir"
crb="$(Rscript - <<'RS'
devtools::load_all(Sys.getenv("CEREBRO_BENCH_REPO_ROOT"), quiet = TRUE)
source(file.path(
  Sys.getenv("CEREBRO_BENCH_REPO_ROOT"),
  "tests", "bench", "prepare_viewer_1m_data.R"
))
cache <- Sys.getenv("CEREBRO_LARGE_CACHE")
if (!nzchar(cache)) cache <- NULL
prepared <- prepareViewer1mBenchmarkData(cache)
thin_qs2 <- sub("[.]crb$", "_thin_qs2.crb", prepared)
cat(if (file.exists(thin_qs2)) thin_qs2 else prepared)
RS
)"

test -f "$crb"

{
  printf 'pr165_sha\t%s\n' "$pr165_sha"
  printf 'before_ref\t%s\n' "$before_ref"
  printf 'before_sha\t%s\n' "$before_sha"
  printf 'after_ref\t%s\n' "$after_ref"
  printf 'after_sha\t%s\n' "$after_sha"
  printf 'crb\t%s\n' "$crb"
  printf 'repeats\t%s\n' "$repeats"
} > "$output_dir/run_manifest.tsv"

Rscript "$repo_root/tests/bench/viewer_1m_hot_paths.R" \
  "$before_root" "$after_root" "$crb" "$repeats" \
  > "$output_dir/hot_paths.tsv"

Rscript "$repo_root/tests/bench/viewer_1m_bundle.R" \
  "$before_root" "$after_root" "$crb" "$repeats" \
  > "$output_dir/bundle.tsv"

printf 'benchmark results\t%s\n' "$output_dir"
