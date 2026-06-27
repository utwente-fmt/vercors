#!/usr/bin/env bash
# Usage:
#   ./perf-timing.sh <folder> [flagset1] [flagset2] ...
#   ./perf-timing.sh <folder> --random <N>
#
# Each flagset is a quoted string of flags, or "" for all checkers on.
#
# Examples:
#   ./perf-timing.sh branches "" "--dev-no-dead"
#   ./perf-timing.sh inv-complexity "--dev-no-dead --dev-no-sat --dev-no-post-sat" ""
#   ./perf-timing.sh branches --random 4

set -euo pipefail

VERCORS="$(dirname "$0")/bin/vct"
EXAMPLES="$(dirname "$0")/examples/concepts/perf"
TIMEOUT=180

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; BOLD='\033[1m'; NC='\033[0m'

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <folder> [flagset1] [flagset2] ..."
    echo "       $0 <folder> --random <N>"
    echo ""
    echo "Available disable flags: --dev-no-dead  --dev-no-sat  --dev-no-post-sat  --dev-no-loop-inv-sat"
    echo ""
    echo "Examples:"
    echo "  $0 branches \"\" \"--dev-no-dead\""
    echo "  $0 inv-complexity \"--dev-no-dead --dev-no-sat --dev-no-post-sat\" \"\""
    echo "  $0 branches --random 4"
    exit 1
fi

TARGET="$EXAMPLES/$1"; shift

ALL_FLAGS=("--dev-no-dead" "--dev-no-sat" "--dev-no-post-sat" "--dev-no-loop-inv-sat")
ALL_CHECKERS=("detectDeadCode" "checkSat" "checkPostSat" "checkInvSat")

# Build flag sets
declare -a FLAG_SETS=()
if [[ "${1:-}" == "--random" ]]; then
    N="${2:-4}"
    for ((i=0; i<N; i++)); do
        fs=""
        for f in "${ALL_FLAGS[@]}"; do [[ $(( RANDOM % 2 )) -eq 1 ]] && fs="$fs $f" || true; done
        FLAG_SETS+=("${fs# }")
    done
else
    while [[ $# -gt 0 ]]; do FLAG_SETS+=("$1"); shift; done
fi

if [[ ${#FLAG_SETS[@]} -eq 0 ]]; then
    echo "No flag sets given. Pass at least one (use \"\" for all checkers on)."; exit 1
fi

if [[ -f "$TARGET" ]]; then
    FILES=("$TARGET")
elif [[ -d "$TARGET" ]]; then
    mapfile -t FILES < <(find "$TARGET" -maxdepth 1 -name "*.pvl" | sort)
else
    echo "Not found: $TARGET"; exit 1
fi
[[ ${#FILES[@]} -eq 0 ]] && { echo "No .pvl files found"; exit 1; }

# Print what checkers are active for each flag set
echo ""
echo -e "${BOLD}$(basename "$TARGET")${NC} — ${#FILES[@]} file(s), ${#FLAG_SETS[@]} flag set(s)"
echo ""
echo "Active checkers per run:"
for idx in "${!FLAG_SETS[@]}"; do
    fs="${FLAG_SETS[$idx]}"
    active=""
    for ci in "${!ALL_FLAGS[@]}"; do
        flag="${ALL_FLAGS[$ci]}"
        checker="${ALL_CHECKERS[$ci]}"
        if [[ "$fs" != *"$flag"* ]]; then
            active="$active $checker"
        fi
    done
    active="$active checkLockInvSat"  # always on
    label="${fs:-<all on>}"
    printf "  col %-2d  %-50s  active:%s\n" "$((idx+1))" "[$label]" "$active"
done
echo ""

# Column header
printf "${BOLD}%-50s" "FILE"
colnum=0
for fs in "${FLAG_SETS[@]}"; do
    colnum=$(( colnum + 1 ))
    printf "  %-18s" "col $colnum"
done
printf "${NC}\n"
printf '%.0s─' $(seq 1 $(( 50 + ${#FLAG_SETS[@]} * 20 ))); echo ""

# Run each file × each flag set
for file in "${FILES[@]}"; do
    printf "%-50s" "$(basename "$file")"
    for fs in "${FLAG_SETS[@]}"; do
        label="${fs:-<all on>}"
        printf "  [running: %s...]" "${label:0:25}" >&2
        start=$(date +%s%3N)
        # shellcheck disable=SC2086
        if timeout "$TIMEOUT" "$VERCORS" $fs "$file" >/dev/null 2>&1; then
            v="ok"; c="$GREEN"
        else
            ec=$?
            [[ $ec -eq 124 ]] && { v="TIMEOUT"; c="$RED"; } || { v="fail"; c="$YELLOW"; }
        fi
        end=$(date +%s%3N)
        printf "\r%-50s" "" >&2
        ms=$(( end - start ))
        printf "  ${c}%6dms [%-7s]${NC}" "$ms" "$v"
    done
    echo ""
done

printf '%.0s─' $(seq 1 $(( 50 + ${#FLAG_SETS[@]} * 20 ))); echo ""
echo ""
echo -e "Legend: ${GREEN}ok${NC}=pass  ${YELLOW}fail${NC}=expected fail  ${RED}TIMEOUT${NC}=>${TIMEOUT}s"
echo ""
