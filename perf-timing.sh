#!/usr/bin/env bash
# Performance timing script for dead-code / satisfiability checkers.
# Runs each test file under three flag combinations and prints a timing table.
#
# Usage:
#   ./perf-timing.sh                  # run all perf files
#   ./perf-timing.sh seq-loops        # run only files under perf/seq-loops/
#   ./perf-timing.sh nest-loops inv-complexity branches

set -euo pipefail

VERCORS="$(dirname "$0")/bin/vct"
EXAMPLES="$(dirname "$0")/examples/concepts/perf"
TIMEOUT=120   # seconds per run before giving up

# ── colour helpers ────────────────────────────────────────────────────────────
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
CYAN='\033[0;36m'; BOLD='\033[1m'; NC='\033[0m'

# ── flag sets to benchmark ────────────────────────────────────────────────────
# Each entry: "label|disabled flags|active checkers description"
# Checkers: checkSat(requires), checkPostSat(ensures), detectDeadCode, checkInvSat, checkLockInvSat(always on)
declare -a FLAG_SETS=(
  "all-on||checkSat + checkPostSat + detectDeadCode + checkInvSat + checkLockInvSat"
  "checkInvSat-only|--dev-no-dead --dev-no-sat --dev-no-post-sat|checkInvSat + checkLockInvSat"
  "detectDead-only|--dev-no-loop-inv-sat --dev-no-sat --dev-no-post-sat|detectDeadCode + checkLockInvSat"
  "checkPostSat-only|--dev-no-dead --dev-no-sat --dev-no-loop-inv-sat|checkPostSat + checkLockInvSat"
  "checkLockSat-only|--dev-no-dead --dev-no-sat --dev-no-post-sat --dev-no-loop-inv-sat|checkLockInvSat only"
)

# ── pick directories to scan ──────────────────────────────────────────────────
if [[ $# -eq 0 ]]; then
  DIRS=("$EXAMPLES")
else
  DIRS=()
  for arg in "$@"; do
    DIRS+=("$EXAMPLES/$arg")
  done
fi

# ── collect .pvl files ────────────────────────────────────────────────────────
mapfile -t FILES < <(find "${DIRS[@]}" -name "*.pvl" | sort)

if [[ ${#FILES[@]} -eq 0 ]]; then
  echo "No .pvl files found under: ${DIRS[*]}"
  exit 1
fi

echo ""
echo -e "${BOLD}VerCors performance timing${NC}"
echo -e "Binary : $VERCORS"
echo -e "Files  : ${#FILES[@]}"
echo -e "Timeout: ${TIMEOUT}s per run"
echo ""

# ── column guide (which checkers are active per column) ───────────────────────
echo -e "${CYAN}Columns and active smoke-test checkers:${NC}"
col=1
for entry in "${FLAG_SETS[@]}"; do
  label="${entry%%|*}"
  rest="${entry#*|}"
  active="${rest#*|}"
  disabled="${rest%%|*}"
  printf "  ${BOLD}[col %d] %-22s${NC} active: %s\n" "$col" "$label" "$active"
  if [[ -n "$disabled" ]]; then
    printf "  %30s disabled flags: %s\n" "" "$disabled"
  fi
  (( col++ ))
done
echo ""

# ── header line ───────────────────────────────────────────────────────────────
printf "${BOLD}%-55s" "FILE"
for entry in "${FLAG_SETS[@]}"; do
  label="${entry%%|*}"
  printf "  %-22s" "$label"
done
printf "${NC}\n"
printf '%0.s─' {1..165}
printf '\n'

# ── run each file × each flag set ─────────────────────────────────────────────
for file in "${FILES[@]}"; do
  rel="${file#$EXAMPLES/}"
  printf "%-55s" "$rel"

  for entry in "${FLAG_SETS[@]}"; do
    rest="${entry#*|}"
    flags_str="${rest%%|*}"
    # shellcheck disable=SC2206
    flags=($flags_str)

    start=$(date +%s%3N)
    if timeout "$TIMEOUT" "$VERCORS" --backend silicon "${flags[@]}" "$file" \
         > /dev/null 2>&1; then
      verdict="ok"
      colour="$GREEN"
    else
      exit_code=$?
      if [[ $exit_code -eq 124 ]]; then
        verdict="TIMEOUT"
        colour="$RED"
      else
        # non-zero exit can mean expected failure (unsat tests) — still record time
        verdict="fail"
        colour="$YELLOW"
      fi
    fi
    end=$(date +%s%3N)
    elapsed=$(( end - start ))

    printf "  ${colour}%5dms %-14s${NC}" "$elapsed" "[$verdict]"
  done
  printf "\n"
done

printf '%0.s─' {1..165}
printf '\n'
echo ""
echo -e "${CYAN}Legend:${NC}"
echo "  ${GREEN}[ok]${NC}      — verified / expected pass"
echo "  ${YELLOW}[fail]${NC}    — verification failed (expected for UNSAT files)"
echo "  ${RED}[TIMEOUT]${NC} — exceeded ${TIMEOUT}s limit"
echo ""
