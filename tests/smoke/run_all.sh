#!/usr/bin/env bash
# Interactive test runner for cerebroAppLite
# Run from the tests/ directory:  bash run_all.sh
set -uo pipefail
cd "$(dirname "$0")"

NONINTERACTIVE=0

# ── Colors ──────────────────────────────────────────────────────────────────
BOLD='\033[1m'
DIM='\033[2m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
RESET='\033[0m'

# ── Script registry ─────────────────────────────────────────────────────────
# Format:  ID | file | description | app_dir | dep_id (prerequisite script)
SCRIPTS=(
  "00|00_prepare_synthetic_data.R|Prepare synthetic smoke fixtures||"
  "10|10_convert_embedded.R|Seurat -> Cerebro conversion (embedded)||"
  "11|11_convert_bpcells.R|Seurat -> Cerebro conversion (bpcells)||"
  "12|12_convert_h5.R|Seurat -> Cerebro conversion (h5)||"
  "20|20_app_embedded.R|Shiny app generation (embedded crbs)|result/20_app_embedded|10"
  "21|21_app_bpcells.R|Shiny app generation (bpcells crbs)|result/21_app_bpcells|11"
  "22|22_app_h5.R|Shiny app generation (h5 crbs)|result/22_app_h5|12"
  "30|30_verify_class_compat.R|Cerebro_v1.3 class API compatibility||10"
  "40|40_verify_module_load.R|Shared projection + group_filters module load (C)||"
  "41|41_verify_spatial_roundtrip.R|Spatial data round-trip + module source (D)||10"
  "90|90_export_bpcells.R|BPCells exporter smoke test||"
  "91|91_attach_bpcells.R|BPCells runtime attach smoke test||90"
  "92|92_bench_expression_access.R|Expression matrix access benchmark||10"
  "93|93_bench_backend_compare.R|Backend compare bench (server side, callr-isolated)||12"
  "94|94_bench_web_load.R|Web load bench (callr + chromote, end-to-end browser)||12"
  "95|95_bench_backend_plot.R|Backend compare 5-panel plot (consumes 93 + 94 csvs)||93"
  "96|96_bench_size_scaling.R|Disk-size scaling bench (synthetic fixtures, 3 sizes × 4 backends)||"
  "97|97_profile_coldpath.R|Hot-path cold profile (profvis: package load + crb + plot)||10"
  "98|98_profile_bpcells_vs_embedded.R|BPCells vs embedded micro-benchmark (load / memory / gene / block)||90"
  "99|99_profile_deep.R|Deep Rprof dive (hover-info, dgCMatrix access, plotly scaling)||10"
)

# Map: script ID -> its result directory (for dependency checking)
# Uses plain function instead of associative array for bash 3.x compat (macOS)
result_dir_for() {
  case "$1" in
    00) echo "data" ;;
    10) echo "result/10_convert_embedded" ;;
    11) echo "result/11_convert_bpcells" ;;
    12) echo "result/12_convert_h5" ;;
    20) echo "result/20_app_embedded" ;;
    21) echo "result/21_app_bpcells" ;;
    22) echo "result/22_app_h5" ;;
    30) echo "result/30_verify_class_compat" ;;
    40) echo "result/40_verify_module_load" ;;
    41) echo "result/41_verify_spatial_roundtrip" ;;
    90) echo "result/90_export_bpcells" ;;
    93) echo "result/93_bench_backend_compare" ;;
    94) echo "result/93_bench_backend_compare" ;;
    95) echo "result/93_bench_backend_compare" ;;
    96) echo "result/96_bench_size_scaling" ;;
    97) echo "result/97_profile_coldpath" ;;
    98) echo "result/98_profile_bpcells_vs_embedded" ;;
    99) echo "result/99_profile_deep" ;;
    *)  echo "" ;;
  esac
}

# ── Helpers ─────────────────────────────────────────────────────────────────
field() { echo "$1" | cut -d'|' -f"$2"; }

find_entry() {
  local target="$1"
  for entry in "${SCRIPTS[@]}"; do
    if [[ "$(field "$entry" 1)" == "$target" ]]; then
      echo "$entry"
      return 0
    fi
  done
  return 1
}

# Check if a dependency's result directory has content
dep_ready() {
  local dep_id="$1"
  local dir
  dir=$(result_dir_for "$dep_id")
  [[ -n "$dir" && -d "$dir" ]] && [[ -n "$(ls -A "$dir" 2>/dev/null)" ]]
}

dep_desc() {
  local dep_id="$1"
  local entry
  entry=$(find_entry "$dep_id") && field "$entry" 3
}

# ── Menu ────────────────────────────────────────────────────────────────────
print_menu() {
  echo ""
  echo -e "${BOLD}=== Test Scripts ===${RESET}"
  echo ""
  echo -e "  ${DIM}Pipeline${RESET}"
  for entry in "${SCRIPTS[@]}"; do
    local id=$(field "$entry" 1)
    [[ "$id" -ge 90 ]] && continue
    local desc=$(field "$entry" 3)
    local dep=$(field "$entry" 5)
    local tag=""
    if [[ -n "$dep" ]] && ! dep_ready "$dep"; then
      tag=" ${RED}(needs [$dep] first)${RESET}"
    fi
    printf "    ${CYAN}[%s]${RESET}  %s%b\n" "$id" "$desc" "$tag"
  done
  echo ""
  echo -e "  ${DIM}Auxiliary (smoke / bench)${RESET}"
  for entry in "${SCRIPTS[@]}"; do
    local id=$(field "$entry" 1)
    [[ "$id" -lt 90 ]] && continue
    local desc=$(field "$entry" 3)
    local dep=$(field "$entry" 5)
    local tag=""
    if [[ -n "$dep" ]] && ! dep_ready "$dep"; then
      tag=" ${RED}(needs [$dep] first)${RESET}"
    fi
    printf "    ${CYAN}[%s]${RESET}  %s%b\n" "$id" "$desc" "$tag"
  done

  # Detect launchable apps
  local has_apps=false
  for entry in "${SCRIPTS[@]}"; do
    local app_dir=$(field "$entry" 4)
    if [[ -n "$app_dir" && -f "$app_dir/app.R" ]]; then
      has_apps=true
      break
    fi
  done

  if $has_apps; then
    echo ""
    echo -e "  ${DIM}Launch existing app${RESET}"
    for entry in "${SCRIPTS[@]}"; do
      local id=$(field "$entry" 1)
      local app_dir=$(field "$entry" 4)
      if [[ -n "$app_dir" && -f "$app_dir/app.R" ]]; then
        printf "    ${GREEN}[r%s]${RESET} Run %s\n" "$id" "$app_dir/"
      fi
    done
  fi

  echo ""
  echo -e "  ${DIM}Other${RESET}"
  echo -e "    ${CYAN}[a]${RESET}   Run all scripts (10 -> 99, sequential)"
  echo -e "    ${CYAN}[q]${RESET}   Quit"
  echo ""
}

# ── Run / launch ────────────────────────────────────────────────────────────
run_script() {
  local file="$1"
  echo ""
  echo -e "${BOLD}>>> Running src/${file}${RESET}"
  echo "────────────────────────────────────────────────"
  if Rscript "src/${file}"; then
    echo "────────────────────────────────────────────────"
    echo -e "${GREEN}<<< Finished src/${file}${RESET}"
    echo ""
    return 0
  else
    echo "────────────────────────────────────────────────"
    echo -e "${RED}<<< Failed src/${file}${RESET}"
    echo ""
    return 1
  fi
}

kill_port() {
  local port="$1"
  local pids
  pids=$(lsof -ti :"$port" 2>/dev/null || true)
  if [[ -n "$pids" ]]; then
    echo -e "${DIM}    Killing process(es) on port ${port}: ${pids}${RESET}"
    echo "$pids" | xargs kill -9 2>/dev/null || true
    sleep 0.3
  fi
}

launch_app() {
  local app_dir="$1"
  local port
  port=$(grep -oE 'port\s*=\s*[0-9]+' "$app_dir/app.R" 2>/dev/null \
         | grep -oE '[0-9]+' | tail -1)
  port="${port:-1337}"

  # Clean up port before launching
  kill_port "$port"

  echo ""
  echo -e "${BOLD}>>> Launching Shiny app: ${app_dir}/ (port ${port})${RESET}"
  echo -e "${DIM}    Press Ctrl+C to stop the server${RESET}"
  echo "────────────────────────────────────────────────"

  # Run in foreground; on Ctrl+C, trap ensures the port is freed
  trap "kill_port $port; echo -e '${GREEN}Port ${port} released.${RESET}'; trap - INT" INT
  (cd "$app_dir" && Rscript -e "shiny::runApp('app.R', port=${port}, launch.browser=TRUE)")
  trap - INT
}

ask_launch() {
  local app_dir="$1"
  if [[ "$NONINTERACTIVE" -eq 1 ]]; then
    return 0
  fi
  if [[ -f "$app_dir/app.R" ]]; then
    echo ""
    read -rp "$(echo -e "${YELLOW}App generated. Launch it now? [y/N]: ${RESET}")" yn
    case "$yn" in
      [Yy]*) launch_app "$app_dir" ;;
      *) echo "Skipped." ;;
    esac
  fi
}

# Check deps, offer to run prerequisite, then run target.
# Returns 0 if the script was run, 1 if skipped.
run_by_id() {
  local id="$1"
  local entry
  entry=$(find_entry "$id") || { echo -e "${RED}Unknown script: ${id}${RESET}"; return 1; }
  local file=$(field "$entry" 2)
  local app_dir=$(field "$entry" 4)
  local dep=$(field "$entry" 5)

  # Dependency check
  if [[ -n "$dep" ]] && ! dep_ready "$dep"; then
    local dep_name
    dep_name=$(dep_desc "$dep")
    if [[ "$NONINTERACTIVE" -eq 1 ]]; then
      echo -e "${DIM}Auto-running prerequisite [$dep] for [$id]: ${dep_name}${RESET}"
      run_by_id "$dep" || return 1
    else
      echo ""
      echo -e "${YELLOW}Script [$id] requires output from [$dep] ($dep_name),${RESET}"
      echo -e "${YELLOW}but $(result_dir_for "$dep")/ is empty or missing.${RESET}"
      echo ""
      read -rp "$(echo -e "${YELLOW}Run [$dep] first? [Y/n]: ${RESET}")" yn
      case "$yn" in
        [Nn]*) echo "Skipped."; return 1 ;;
        *)     run_by_id "$dep" || return 1 ;;
      esac
    fi
  fi

  run_script "$file" || return 1
  [[ -n "$app_dir" ]] && ask_launch "$app_dir"
  return 0
}

run_all() {
  local failed=0
  for entry in "${SCRIPTS[@]}"; do
    local id=$(field "$entry" 1)
    if ! run_by_id "$id"; then
      failed=1
    fi
  done
  return "$failed"
}

run_once_and_exit() {
  local target="$1"
  NONINTERACTIVE=1

  echo -e "${BOLD}cerebroAppLite test runner${RESET}"
  echo -e "${DIM}Working directory: $(pwd)${RESET}"

  case "$target" in
    a|A|all|--all)
      run_all
      exit "$?"
      ;;
    [0-9]* )
      local id
      id=$(printf "%02d" "$target" 2>/dev/null || echo "$target")
      run_by_id "$id"
      exit "$?"
      ;;
    -h|--help|help)
      echo "Usage: bash run_all.sh [--all|ID]"
      echo "  no args  interactive menu"
      echo "  --all    run all scripts once, then exit"
      echo "  ID       run a single script once, then exit"
      exit 0
      ;;
    *)
      echo -e "${RED}Invalid argument: ${target}${RESET}"
      echo "Usage: bash run_all.sh [--all|ID]"
      exit 1
      ;;
  esac
}

if [[ $# -gt 0 ]]; then
  run_once_and_exit "$1"
fi

# ── Main loop ───────────────────────────────────────────────────────────────
echo -e "${BOLD}cerebroAppLite test runner${RESET}"
echo -e "${DIM}Working directory: $(pwd)${RESET}"

while true; do
  print_menu
  read -rp "$(echo -e "${YELLOW}Enter choice: ${RESET}")" choice

  case "$choice" in
    q|Q|quit|exit)
      echo "Bye."
      exit 0
      ;;
    a|A|all)
      run_all
      ;;
    r[0-9]*)
      id="${choice#r}"
      entry=$(find_entry "$id") || { echo -e "${RED}Unknown: ${choice}${RESET}"; continue; }
      app_dir=$(field "$entry" 4)
      if [[ -z "$app_dir" ]]; then
        echo -e "${RED}Script ${id} is not a launchable app.${RESET}"
      elif [[ ! -f "$app_dir/app.R" ]]; then
        echo -e "${RED}${app_dir}/app.R not found. Run [${id}] first to generate it.${RESET}"
      else
        launch_app "$app_dir"
      fi
      ;;
    [0-9]*)
      id=$(printf "%02d" "$choice" 2>/dev/null || echo "$choice")
      run_by_id "$id"
      ;;
    *)
      echo -e "${RED}Invalid choice: ${choice}${RESET}"
      ;;
  esac
done
