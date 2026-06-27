#!/usr/bin/env bash
# Usage: ./trace-count.sh <file.pvl> <report-interval> [timeout-seconds] [extra flags...]
# Runs vct with --dev-silicon-branch-condition-report-interval and reports
# the highest "Silicon has explored N branch traces" count seen, plus
# whether verification completed or was cut off by the timeout.
set -uo pipefail

FILE="$1"; INTERVAL="$2"; TIMEOUT="${3:-120}"; shift 3 || true
EXTRA_FLAGS=("$@")

VERCORS="$(dirname "$0")/../../bin/vct"

start=$(date +%s)
OUT=$(timeout "$TIMEOUT" "$VERCORS" --dev-silicon-branch-condition-report-interval "$INTERVAL" --dev-no-dead "${EXTRA_FLAGS[@]}" "$FILE" 2>&1)
rc=$?
end=$(date +%s)

MAX=$(echo "$OUT" | grep -oP 'explored \d+ branch traces' | grep -oP '\d+' | sort -n | tail -1)
MAX="${MAX:-0}"

if [[ $rc -eq 124 ]]; then
    STATUS="TIMEOUT"
elif echo "$OUT" | grep -q "Verification completed successfully"; then
    STATUS="completed"
else
    STATUS="other(rc=$rc)"
fi

echo "$(basename "$FILE")  interval=$INTERVAL  elapsed=$((end-start))s  max_traces=$MAX  status=$STATUS"
