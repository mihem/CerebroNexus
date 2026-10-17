#!/usr/bin/env bash
# Run with bash; executable bits need not survive a Windows transfer.
set -euo pipefail

fail() { printf 'Viewer setup: %s\n' "$*" >&2; exit 1; }
usage() {
  printf '%s\n' 'Usage: bash viewer/start-linux.sh [--prepare-only] [--user USER]' \
    'Default: prepare authentication permissions and start the App as the current user.' \
    '--user requires --prepare-only; run as root to prepare for a service account.'
}
prepare_only=false
runtime_user=''
while (($#)); do
  case "$1" in
    --prepare-only) prepare_only=true; shift ;;
    --user)
      (($# >= 2)) && [[ -n "$2" && "$2" != -* ]] || fail '--user requires an account name.'
      runtime_user=$2; shift 2 ;;
    --help|-h) usage; exit 0 ;;
    *) usage >&2; fail "Unknown argument: $1" ;;
  esac
done
[[ $(uname -s) == Linux ]] || fail 'This entry point is for Linux. On Windows use shiny::runApp().'
[[ -z "$runtime_user" || "$prepare_only" == true ]] || fail '--user requires --prepare-only; start the service separately.'
if [[ -n "$runtime_user" ]]; then
  runtime_uid=$(id -u -- "$runtime_user") || fail 'The runtime account does not exist.'
  runtime_gid=$(id -g -- "$runtime_user") || fail 'Cannot resolve the runtime group.'
else
  runtime_uid=$(id -u)
  runtime_gid=$(id -g)
fi
[[ "$runtime_uid" != 0 ]] || fail 'Choose a non-root runtime account with --prepare-only --user USER.'
[[ $EUID == 0 || $EUID == "$runtime_uid" ]] || fail 'Run as the runtime user, or use sudo with --prepare-only --user USER.'

app_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd -- "$app_dir"
[[ -f app.R && ! -L app.R ]] || fail 'Run the script bundled inside an exported App.'

# Inspect every target before changing anything. Never traverse linked auth paths
# or modify a hard-linked secret outside this deployment.
paths=()
modes=()
if [[ -e private-data/auth || -L private-data/auth || -e viewer-auth.env || -L viewer-auth.env ]]; then
  for directory in private-data private-data/auth; do
    [[ -d "$directory" && ! -L "$directory" ]] || fail "Expected a real directory: $directory"
  done
  paths+=(private-data/auth private-data/auth/credentials.sqlite)
  modes+=(700 600)
  [[ -f private-data/auth/credentials.sqlite && ! -L private-data/auth/credentials.sqlite ]] || fail 'Expected a regular authentication database.'
  if [[ -e viewer-auth.env || -L viewer-auth.env ]]; then
    [[ -f viewer-auth.env && ! -L viewer-auth.env ]] || fail 'Expected a regular viewer-auth.env file.'
    paths+=(viewer-auth.env)
    modes+=(600)
  fi
fi
for path in "${paths[@]}"; do
  if [[ -f "$path" ]]; then
    [[ $(stat -c %h -- "$path") == 1 ]] || fail "Refusing a hard-linked authentication file: $path"
  fi
  [[ $EUID == 0 || $(stat -c %u -- "$path") == "$runtime_uid" ]] || fail "Not owned by the runtime user: $path. Ask the operator to run --prepare-only --user USER."
done
for i in "${!paths[@]}"; do
  path=${paths[$i]}
  mode=${modes[$i]}
  if [[ $(stat -c %u -- "$path") != "$runtime_uid" ]]; then
    chown -- "$runtime_uid:$runtime_gid" "$path" || fail "Cannot set ownership on $path. Prepare writable storage before deployment."
  fi
  if [[ $(stat -c %a -- "$path") != "$mode" ]]; then
    chmod -- "$mode" "$path" || fail "Cannot protect $path. Prepare permissions before a read-only deployment; use a Linux filesystem that supports POSIX modes."
  fi
  [[ $(stat -c %a -- "$path") == "$mode" && $(stat -c %u -- "$path") == "$runtime_uid" ]] || fail "Permissions did not persist on $path. Copy the App to a Linux filesystem supporting POSIX modes."
done

as_runtime() {
  if [[ $EUID == "$runtime_uid" ]]; then
    "$@"
  else
    runuser -u "$runtime_user" -- "$@"
  fi
}
as_runtime test -r "$app_dir/app.R" || fail 'The runtime user cannot read app.R. Check access through every parent directory.'
for path in "${paths[@]}"; do
  if [[ -d "$path" ]]; then
    as_runtime test -x "$app_dir/$path" || fail "The runtime user cannot traverse $path. Check parent directory access."
  else
    as_runtime test -r "$app_dir/$path" || fail "The runtime user cannot read $path. Check parent directory access."
  fi
done
printf '%s\n' 'Viewer deployment permissions are ready. Authentication is still checked at App startup.'
[[ "$prepare_only" == true ]] && exit 0
command -v Rscript >/dev/null || fail 'Rscript is not on PATH. Install/configure the App runtime dependencies first.'
exec Rscript -e 'shiny::runApp(".", launch.browser = FALSE)'
