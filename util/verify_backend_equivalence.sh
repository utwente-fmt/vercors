#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  util/verify_backend_equivalence.sh init
  util/verify_backend_equivalence.sh check

Purpose:
  Guard optimization work by comparing generated backend output against a stored baseline.

Defaults:
  VCT_BIN=bin/vct
  INPUT_FILE=examples/concepts/gpgpu/opencl_vector_add.cl
  VCT_FLAGS="--skip-backend --dev-unsafe-optimization"
  BASELINE_FILE=util/perf-baselines/opencl_vector_add_baseline.vpr
  WORK_DIR=tmp/backend-equivalence

Commands:
  init   Generate and store baseline output at BASELINE_FILE.
  check  Generate current output and compare it byte-for-byte against BASELINE_FILE.

Exit codes:
  0 on match / successful init
  1 on mismatch or missing baseline
EOF
}

if [[ $# -ne 1 ]]; then
  usage
  exit 1
fi

MODE="$1"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

VCT_BIN="${VCT_BIN:-$REPO_ROOT/bin/vct}"
INPUT_FILE="${INPUT_FILE:-examples/concepts/gpgpu/opencl_vector_add.cl}"
VCT_FLAGS="${VCT_FLAGS:---skip-backend --dev-unsafe-optimization}"
BASELINE_FILE="${BASELINE_FILE:-$REPO_ROOT/util/perf-baselines/opencl_vector_add_baseline.vpr}"
WORK_DIR="${WORK_DIR:-$REPO_ROOT/tmp/backend-equivalence}"

mkdir -p "$(dirname "$BASELINE_FILE")" "$WORK_DIR"

warmup() {
  "$VCT_BIN" --version >/dev/null 2>&1
}

generate_vpr() {
  local out_base="$1"
  rm -f "${out_base}"-*.vpr
  # shellcheck disable=SC2086
  "$VCT_BIN" "$INPUT_FILE" $VCT_FLAGS --backend-file-base "$out_base" >/dev/null 2>"$WORK_DIR/$(basename "$out_base").stderr"

  local out_file="${out_base}-0.vpr"
  if [[ ! -f "$out_file" ]]; then
    echo "Expected output file not found: $out_file" >&2
    exit 1
  fi

  printf '%s\n' "$out_file"
}

case "$MODE" in
  init)
    warmup
    generated="$(generate_vpr "$WORK_DIR/baseline")"
    cp "$generated" "$BASELINE_FILE"
    sha256sum "$BASELINE_FILE" >"$BASELINE_FILE.sha256"
    printf 'Stored baseline: %s\n' "$BASELINE_FILE"
    ;;
  check)
    if [[ ! -f "$BASELINE_FILE" ]]; then
      echo "Missing baseline file: $BASELINE_FILE" >&2
      echo "Run: util/verify_backend_equivalence.sh init" >&2
      exit 1
    fi

    warmup
    generated="$(generate_vpr "$WORK_DIR/current")"
    if diff -u "$BASELINE_FILE" "$generated" >"$WORK_DIR/latest.diff"; then
      printf 'MATCH: %s equals current generated output (%s).\n' "$BASELINE_FILE" "$generated"
    else
      echo "MISMATCH: generated output differs from baseline." >&2
      echo "Diff saved at: $WORK_DIR/latest.diff" >&2
      exit 1
    fi
    ;;
  *)
    usage
    exit 1
    ;;
esac
