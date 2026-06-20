# Optimization Handoff (2026-06-20)

## Goal
Optimize `vct` runtime for `examples/concepts/gpgpu/opencl_vector_add.cl` while keeping correctness.

## Benchmark protocol
- Warmup before measurements: `/home/lars/data/vercors/bin/vct --version`
- Timed command: `/home/lars/data/vercors/bin/vct examples/concepts/gpgpu/opencl_vector_add.cl --skip-backend --dev-unsafe-optimization`
- Runs per revision: 4

## Confirmed results
- Baseline `f8d7596fe1`: avg `13.405s`
- Opt 1 `14da9aa9bc` (skip idempotency check): avg `11.449s` (`-14.59%` vs baseline)
- Opt 2 `11af5c77cc` (Either-based coercion fallback): avg `11.336s` (`-15.43%` vs baseline)
- Working tree Forall structural-sharing experiment: avg `10.875s` (`~4.07%` faster than `11af5c77cc` reference)

## Implemented code experiment
- Applied Forall-only structural sharing in generated rewrite code at `out/vercors/col/helpers/sources.dest/ForallRewrite.scala`.
- Correctness smoke run passed (`EXIT:0`) for the benchmark command.

## Profile/hotspots state
- Fresh profile generated with `--profile` and saved as `profile.pprof.gz`.
- Main hotspot notes and pprof best-practice references are recorded in `optimisations.yaml`.

## Important context
- Name Resolution micro-optimization trial in `Resolution.scala` regressed and was reverted.
- `optimisations.yaml` is the canonical run ledger and should be kept up to date.

## Recommended next steps
1. Re-run `--profile` on current working tree after any additional optimization and compare against this profile.
2. If Forall fast path remains stable, generalize structural-sharing pattern to another high-frequency rewrite node.
3. Keep using the same warmup and 4-run protocol for fair comparison.
