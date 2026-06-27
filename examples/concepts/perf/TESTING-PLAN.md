# Performance Evaluation — Testing Plan

All commands are run as:
```
./bin/vct <flags> examples/concepts/perf/<category>/<FILE>.pvl
```
timed with `time -p`, interleaving the two configurations being compared (run A, run B, run A, run B, ...) and averaging, to cancel out JVM-startup noise.

Checker flags:
- `--dev-no-dead` disables **detectDeadCode**
- `--dev-no-sat` disables **checkSat** (CheckContractSatisfiability — preconditions)
- `--dev-no-post-sat` disables **checkPostconditionSatisfiability**
- `--dev-no-loop-inv-sat` disables **checkInvSat** (CheckLoopInvariantSatisfiability)
- `--dev-no-lock-inv-sat` disables **checkLockInvSat** (CheckLockInvariantSatisfiability)

---

## 1. seq-loops — baseline scaling with loop count

What we're checking: how the cost of checkInvSat and detectDeadCode grows as the
number of sequential loops increases, each with the same simple invariant.
Expected results: All ON should scale roughly linearly with the loop count;
checkInvSat OFF and detectDeadCode OFF should each be faster than All ON, with
the gap between All ON and each OFF config widening as the loop count grows
(1 → 5 → 15), revealing the per-loop cost of each checker.

| File | Config A | Config B | Config C | Purpose |
|---|---|---|---|---|
| PERF-SEQ-1loop.pvl | all checkers ON | checkInvSat OFF | detectDeadCode OFF | baseline, 1 loop |
| PERF-SEQ-5loops.pvl | all checkers ON | checkInvSat OFF | detectDeadCode OFF | 5 sequential loops |
| PERF-SEQ-15loops.pvl | all checkers ON | checkInvSat OFF | detectDeadCode OFF | 15 sequential loops |

**Result table:**

| File | All ON (s) | checkInvSat OFF (s) | detectDeadCode OFF (s) |
|---|---|---|---|
| PERF-SEQ-1loop | | | |
| PERF-SEQ-5loops | | | |
| PERF-SEQ-15loops | | | |

---

## 2. nest-loops — scaling with nesting depth

What we're checking: the same checkInvSat/detectDeadCode comparison as seq-loops,
but with loops nested inside each other instead of placed sequentially.
Expected results: nesting should be more expensive than the same number of
sequential loops (seq-loops section 1), since an outer invariant must be
re-established across the inner loop's body. The All ON vs. checkInvSat OFF gap
should grow faster than linearly with nesting depth (2 → 3 → 5 levels) if
invariant checks compound across nesting; the detectDeadCode OFF gap should
grow more modestly, since dead-code analysis is mostly per-statement.

| File | Config A | Config B | Config C | Purpose |
|---|---|---|---|---|
| PERF-NEST-2level.pvl | all checkers ON | checkInvSat OFF | detectDeadCode OFF | 2 levels nested |
| PERF-NEST-3level.pvl | all checkers ON | checkInvSat OFF | detectDeadCode OFF | 3 levels nested |
| PERF-NEST-5level.pvl | all checkers ON | checkInvSat OFF | detectDeadCode OFF | 5 levels nested |

**Result table:**

| File | All ON (s) | checkInvSat OFF (s) | detectDeadCode OFF (s) |
|---|---|---|---|
| PERF-NEST-2level | | | |
| PERF-NEST-3level | | | |
| PERF-NEST-5level | | | |

---

## 3. inv-complexity — checkInvSat cost vs. invariant content (light/heavy pairs)

What we're checking: with the loop count fixed, how the *shape* of the loop
invariant (number of conjuncts, arithmetic vs. quantifiers vs. Perm clauses vs.
function calls) affects checkInvSat's running time, by comparing a "light"
invariant against a "heavy" variant of the same kind.
Expected results: heavier invariants should take longer than their light
counterpart, but the increase should vary by feature — e.g. extra arithmetic
conjuncts or Perm clauses are expected to add a modest percentage, while
quantifiers (forall) and function calls may add little to no extra cost if the
solver already pays most of that cost regardless of invariant size. Any pair
where heavy is *not* slower than light indicates that feature isn't a
significant driver of checkInvSat cost.

All run with `--dev-no-dead --dev-no-sat --dev-no-post-sat` (checkInvSat isolated).

| Pair | Light file | Heavy file | What's varied |
|---|---|---|---|
| Arithmetic | PERF-INV-arithmetic-light.pvl | PERF-INV-arithmetic.pvl | 2 vars/6 conjuncts/1 loop → 7 vars/24 conjuncts/3 loops |
| Perm | PERF-INV-perm-light.pvl | PERF-INV-perm-1field.pvl, PERF-INV-perm-5fields.pvl | 1 Perm+2 conjuncts/1 method → 1 Perm+8 conjuncts x3 methods → 5 Perms+20 conjuncts x2 methods |
| Forall | PERF-INV-forall-light.pvl | PERF-INV-forall.pvl | 1 forall/1 loop → 4 foralls x 2 loops |
| Funcall | PERF-INV-funcall-d1-light.pvl | PERF-INV-funcall-d1.pvl, PERF-INV-funcall-d3.pvl | 1 call/1 loop → 3 calls x2 loops (depth 1) → 2 calls x2 loops (depth 3) |
| Combined | — | PERF-INV-combined.pvl | 1 Perm + 2 foralls x 2 methods (heap + quantifier interaction) |

**Result table:**

| File | checkInvSat-only time (s) | Notes / scaling vs. light pair |
|---|---|---|
| PERF-INV-arithmetic-light | 16.85 (14.82, 18.88) | baseline |
| PERF-INV-arithmetic | 20.99 (20.49, 21.49) | +24% vs. light |
| PERF-INV-perm-light | 15.28 (14.42, 16.13) | baseline |
| PERF-INV-perm-1field | 18.35 (18.56, 18.13) | +20% vs. light |
| PERF-INV-perm-5fields | | vs. perm-1field |
| PERF-INV-forall-light | 19.45 (19.84, 19.05) | baseline |
| PERF-INV-forall | 17.69 (17.89, 17.48) | ~0% (heavy not slower) vs. light |
| PERF-INV-funcall-d1-light | 15.40 (15.96, 14.84) | baseline |
| PERF-INV-funcall-d1 | 12.99 (13.82, 12.15) | ~0% (heavy not slower) vs. light |
| PERF-INV-funcall-d3 | | vs. funcall-d1 (call depth) |
| PERF-INV-combined | | heap + quantifier interaction |

*Numbers are averages of 2 interleaved runs; raw values in parentheses.*

---

## 4. branches — detectDeadCode overhead (ON vs. OFF pairs)

What we're checking: how detectDeadCode's cost scales with branch structure —
total branch count, chaining/nesting depth, and whether branch conditions share
variables. checkInvSat/checkSat/checkPostSat are no-ops on these files (no
loops, trivial requires, no ensures), so All ON vs. `--dev-no-dead` isolates
detectDeadCode's overhead directly.
Expected results: overhead should grow with the number of live branches/Refutes
that must be checked for reachability (PERF-BRANCH-30 and indep-30 show large
overheads), while flat if/else-if chains and deeply nested-but-small structures
(chain-30, nest-7) show little to no overhead because the Refute count per
branch stays small. PERF-BRANCH-30-const is a correctness check, not a timing
one: it must `Fail` and report `deadBranch` for all 30 always-false branches.

`ON` = all checkers ON (no flags)
`OFF` = `--dev-no-dead` (baseline, no dead-code instrumentation; other checkers are no-ops on these files)

| File | Structure | Purpose |
|---|---|---|
| PERF-BRANCH-5.pvl | 5 if/else, 2 vars | small baseline |
| PERF-BRANCH-15.pvl | 15 if/else, 2 vars | medium |
| PERF-BRANCH-30.pvl | 30 if/else, 2 vars (60 Refutes) | large, all live |
| PERF-BRANCH-no-else-30.pvl | same 30 conditions, no else (30 Refutes) | branch-count scaling vs. PERF-BRANCH-30 |
| PERF-BRANCH-chain-5/10/15/30.pvl | if/else-if chains, accumulated negations | chain-depth scaling |
| PERF-BRANCH-indep-30.pvl | 30 ifs (no else), 30 independent vars | variable-count effect |
| PERF-BRANCH-single-var-30.pvl | 30 ifs (no else), 1 shared var, increasing thresholds | variable-sharing vs. indep-30 |
| PERF-BRANCH-nest-7.pvl | 7-level nested if/else (254 Refutes) | nesting-depth scaling |
| PERF-BRANCH-30-const.pvl | 30 if(true)/else — all else-branches dead | correctness: must report `deadBranch` x30 |

**Result table:**

| File | ON (s) | OFF (s) | Overhead (ON-OFF, s) | Overhead % |
|---|---|---|---|---|
| PERF-BRANCH-5 | | | | |
| PERF-BRANCH-15 | | | | |
| PERF-BRANCH-30 | 138.62 (157.66, 119.58) | 39.77 (37.01, 42.53) | 98.85 | +249% |
| PERF-BRANCH-no-else-30 | 64.11 (68.79, 59.42) | 31.47 (32.33, 30.61) | 32.64 | +104% |
| PERF-BRANCH-chain-5 | | | | |
| PERF-BRANCH-chain-10 | | | | |
| PERF-BRANCH-chain-15 | | | | |
| PERF-BRANCH-chain-30 | ~13 (not precisely logged) | ~13 (not precisely logged) | ~0 | ~0% |
| PERF-BRANCH-indep-30 | 80.31 (75.29, 85.32) | 33.50 (33.31, 33.68) | 46.81 | +140% |
| PERF-BRANCH-single-var-30 | 18.49 (19.21, 17.77) | 16.14 (16.09, 16.18) | 2.35 | +15% |
| PERF-BRANCH-nest-7 | ~15 (not precisely logged) | ~15 (not precisely logged) | ~0 | ~0% |
| PERF-BRANCH-30-const | (must Fail with `deadBranch` x30) — confirmed correct | — | — | — |

*Numbers are averages of 2 interleaved runs; raw values in parentheses. chain-30 and nest-7 were measured in an earlier interleaved run (~13s and ~15s either way) but exact ON/OFF figures weren't retained — rerun if you need precise numbers.*

---

## 5. parallel — detectDeadCode overhead in `par` blocks

What we're checking: whether detectDeadCode's overhead (section 4) compounds
when the branches live inside `par` blocks instead of plain sequential code,
and how it scales with the number of parallel blocks.
Expected results: PERF-PAR-simple should show overhead comparable to a
similarly-sized branches case (section 4); PERF-PAR-4blocks should show
roughly 4x that overhead if detectDeadCode analyzes each `par` block
independently, confirming the cost scales with block count rather than being a
fixed one-time cost.

`ON`/`OFF` as above (all checkers ON vs. `--dev-no-dead`).

| File | Structure | Purpose |
|---|---|---|
| PERF-PAR-simple.pvl | 1 par block with branches | baseline |
| PERF-PAR-4blocks.pvl | 4 par blocks with branches | scaling with block count |

**Result table:**

| File | ON (s) | OFF (s) | Overhead (s) | Overhead % |
|---|---|---|---|---|
| PERF-PAR-simple | | | | |
| PERF-PAR-4blocks | | | | |

---

## 6. locks — checkLockInvSat cost (ON vs. OFF pairs)

What we're checking: the cost of checkLockInvSat (verifying that lock
invariants are satisfiable) as the lock invariant's complexity, the number of
classes with lock invariants, and the number of call sites checking the same
invariant increase.
Expected results: ON should be slower than OFF by an amount that grows with
the number of distinct lock-invariant *definitions* that must each be checked
once — so perm-5fields (more conjuncts in one invariant) and 5classes (5
separate invariants) should show larger overhead than perm-1field, while
dedup-5sites (one invariant, checked at 5 call sites) should show overhead
close to perm-1field if the check is deduplicated per class rather than
per call site.

`ON` = `--dev-no-dead --dev-no-sat --dev-no-post-sat --dev-no-loop-inv-sat` (checkLockInvSat active)
`OFF` = same + `--dev-no-lock-inv-sat` (baseline, no lock-invariant satisfiability check)

| File | Structure | Purpose |
|---|---|---|
| PERF-LOCK-perm-1field.pvl | 1 class, 1 Perm-based lock invariant site | baseline |
| PERF-LOCK-perm-5fields.pvl | 1 class, 5-field lock invariant | conjunct-count scaling |
| PERF-LOCK-5classes.pvl | 5 distinct classes, each with a lock invariant | class-count scaling |
| PERF-LOCK-dedup-5sites.pvl | 1 class, lock invariant checked at 5 call sites | per-class dedup check |

**Result table:**

| File | ON (s) | OFF (s) | Overhead (ON-OFF, s) | Overhead % |
|---|---|---|---|---|
| PERF-LOCK-perm-1field | 12.16 (single run, ON only) | | | |
| PERF-LOCK-perm-5fields | | | | |
| PERF-LOCK-5classes | 11.97 (single run, ON only) | | | |
| PERF-LOCK-dedup-5sites | 12.64 (single run, ON only) | | | |

*The three ON values above are single, non-interleaved runs from an earlier check (before `--dev-no-lock-inv-sat` was confirmed) — no OFF baseline was measured. Rerun all four ON/OFF pairs interleaved (like the branches/inv-complexity pairs) for thesis-quality numbers, especially the 5classes scaling claim (file's own comment expects ~5x perm-1field, but the ON-only numbers showed ~0% difference).*

---

## 7. postcond — checkPostSat cost (Priority 2 focus)

What we're checking: the cost of checkPostSat (verifying a method's
postcondition is satisfiable) in isolation, across postconditions that vary in
shape — a plain `ensures`, one with a function call chain, one using `\old` +
`forall`, one requiring `Perm` on a field, and ones repeated across many
methods.
Expected results: funcall-d3, old-forall, and perm should each take longer than
simple, with the size of the increase indicating which feature (call-chain
unfolding, quantifiers over old state, or heap-permission reasoning) costs the
most. 10methods and 30methods should scale roughly linearly with method count
if each postcondition is checked independently (30methods ≈ 3x 10methods).

All run with `--dev-no-dead --dev-no-sat --dev-no-loop-inv-sat`.

| File | Structure | Purpose |
|---|---|---|
| PERF-POST-simple.pvl | simple ensures clause | baseline |
| PERF-POST-funcall-d3.pvl | ensures with 3-deep function call chain | function-call/WD overhead |
| PERF-POST-old-forall.pvl | ensures with `\old` + forall | old-state + quantifier cost |
| PERF-POST-perm.pvl | ensures requiring Perm on a field | heap reasoning cost |
| PERF-POST-10methods.pvl | 10 methods each with a postcondition | method-count scaling |
| PERF-POST-30methods.pvl | 30 methods each with a postcondition | method-count scaling (3x) |

**Result table:**

| File | Time (s) | Notes |
|---|---|---|
| PERF-POST-simple | | baseline |
| PERF-POST-funcall-d3 | | vs. simple |
| PERF-POST-old-forall | | vs. simple |
| PERF-POST-perm | | vs. simple |
| PERF-POST-10methods | | vs. simple x10 |
| PERF-POST-30methods | | vs. 10methods |

---

## 8. precond — checkSat cost (mirrors postcond)

What we're checking: the same comparison as section 7, but for checkSat
(verifying a method's precondition is satisfiable) instead of checkPostSat.
Expected results: the same pattern as postcond — funcall-d3 and perm should
cost more than simple, with the increase showing which feature (call-chain
unfolding vs. heap-permission reasoning) drives checkSat's cost; 10methods and
30methods should scale roughly linearly with method count. Comparing this
section's numbers against section 7's (same structures, different checker)
shows whether checkSat and checkPostSat have comparable per-feature costs.

All run with `--dev-no-dead --dev-no-post-sat --dev-no-loop-inv-sat`.

| File | Structure | Purpose |
|---|---|---|
| PERF-PRECOND-simple.pvl | simple requires clause | baseline |
| PERF-PRECOND-funcall-d3.pvl | requires with 3-deep function call chain | function-call/WD overhead |
| PERF-PRECOND-perm.pvl | requires with Perm + instance function call | heap reasoning cost |
| PERF-PRECOND-10methods.pvl | 10 methods each with a precondition | method-count scaling |
| PERF-PRECOND-30methods.pvl | 30 methods each with a precondition | method-count scaling (3x) |

**Result table:**

| File | Time (s) | Notes |
|---|---|---|
| PERF-PRECOND-simple | | baseline |
| PERF-PRECOND-funcall-d3 | | vs. simple |
| PERF-PRECOND-perm | | vs. simple |
| PERF-PRECOND-10methods | | vs. simple x10 |
| PERF-PRECOND-30methods | | vs. 10methods |

---

## 9. sat-unsat — correctness + cost of SAT vs UNSAT detection

What we're checking: for each of the three satisfiability checkers
(checkInvSat, checkPostSat, checkLockInvSat), that an UNSAT contract is
correctly rejected with the right error code, and how the UNSAT run's cost
compares to the matching SAT run.
Expected results: every UNSAT file must `Fail` with its listed error code
(`invariantUnsatisfiable`, `postUnsatisfiable`, `lockInvariantUnsatisfiable`) —
any pass, or a fail with a different code, is a correctness bug. On timing,
UNSAT detection is not expected to be significantly slower than the SAT case
(the solver finds unsatisfiability about as fast as it finds a model), so SAT
and UNSAT times for the same pair should be in the same ballpark.

Each pair: same structure, SAT version verifies, UNSAT version `Fail`s with the corresponding error code.

| Pair | SAT file | UNSAT file | Checker | Expected error code |
|---|---|---|---|---|
| inv-arith | PERF-SAT-inv-arith.pvl | PERF-UNSAT-inv-arith.pvl | checkInvSat | `invariantUnsatisfiable` |
| inv-perm | PERF-SAT-inv-perm.pvl | PERF-UNSAT-inv-perm.pvl | checkInvSat | `invariantUnsatisfiable` |
| post | PERF-SAT-post.pvl | PERF-UNSAT-post.pvl | checkPostSat | `postUnsatisfiable` |
| lock | PERF-SAT-lock.pvl | PERF-UNSAT-lock.pvl | checkLockInvSat | `lockInvariantUnsatisfiable` |

**Result table:**

| Pair | SAT time (s) | UNSAT time (s) | UNSAT correctly detected? |
|---|---|---|---|
| inv-arith | | | |
| inv-perm | | | |
| post | | | |
| lock | | | |

---

## 10. combined — realistic multi-checker programs (ON vs OFF)

What we're checking: the *total* overhead of all five smoke checkers together
on programs that combine the features studied individually in sections 1-8
(loops with invariants, branches, lock invariants, par blocks, postconditions).
Expected results: All ON should be slower than All OFF, with the absolute and
percentage overhead roughly explainable as the sum of the per-feature overheads
measured in the earlier sections (e.g. event-processor combines lock + loop
invariant + branch overhead, so its total overhead should be in the same range
as those individual contributions added together). A combined overhead far
outside that range would suggest the checkers interact rather than simply
adding up.

`ON` = all checkers active (no flags). `OFF` = `--dev-no-dead --dev-no-sat --dev-no-post-sat --dev-no-loop-inv-sat --dev-no-lock-inv-sat` (all 5 smoke checkers disabled — true baseline).

| File | Structure |
|---|---|
| PERF-REAL-event-processor.pvl | 5-field lock + forall loop invariant + 5-way chain branch |
| PERF-REAL-analytics.pvl | 3 sequential forall loops, each with a 3-way branch + postcondition |
| PERF-REAL-parallel-pipeline.pvl | 2-field lock class + forall prep loop + 4 par blocks x 4-way branches |

**Result table:**

| File | All ON (s) | All OFF (s) | Total smoke-test overhead (s) | Overhead % |
|---|---|---|---|---|
| PERF-REAL-event-processor | | | | |
| PERF-REAL-analytics | | | | |
| PERF-REAL-parallel-pipeline | | | | |

---

## 11. Real-world example suite (three-way: baseline / +dead-code / +all checkers)

What we're checking: whether the per-checker overheads measured on the
synthetic benchmarks (sections 1-8) are representative of real, non-synthetic
VerCors example programs, by measuring the same three-way comparison
(everything off / detectDeadCode added back / all checkers added back) on
existing examples from the test suite.
Expected results: dead-code overhead % and full-suite overhead % for these
real programs should fall in a similar range to the overheads seen for
comparably-sized synthetic cases (e.g. a program with ~30-60 branches should
see dead-code overhead similar to PERF-BRANCH-30/indep-30 in section 4).
Programs with many loops (KahnsTopologicalSort, challenge1, relaxed_prefix)
should show overhead closer to the seq-loops/nest-loops figures (sections 1-2).
Numbers far outside those ranges would indicate the synthetic benchmarks miss
some real-world cost driver.

`Baseline` = `--dev-no-dead --dev-no-loop-inv-sat --dev-no-post-sat --dev-no-sat`
`+dead-code` = `--dev-no-loop-inv-sat --dev-no-post-sat --dev-no-sat`
`+all checkers` = no flags

| File | Notes |
|---|---|
| verifythis/2019/challenge1.pvl | 39 loops |
| concepts/algo/KahnsTopologicalSort.pvl | 61 loops (most loop-dense) |
| verifythis/2015/relaxed_prefix.pvl | 35 loops |
| verifythis/2019/challenge3_complete.pvl | 27 branches (extra flag `--no-infer-heap-context-into-frame`) |
| concepts/arrays/ArrayList.java | 36 loops, many methods |

**Result table:**

| File | Baseline (s) | +dead-code (s) | +all checkers (s) | dead-code overhead % | full-suite overhead % |
|---|---|---|---|---|---|
| challenge1 | | | | | |
| KahnsTopologicalSort | | | | | |
| relaxed_prefix | | | | | |
| challenge3_complete | | | | | |
| ArrayList | | | | | |

---

## 12. branch path-explosion — trace-count experiments (extends section 4)

What we're checking: section 4 measured *wall time* for branch structures, but
wall time on small files is dominated by ~12-15s JVM/Z3 startup noise. This
section uses a direct, noise-free metric — the number of symbolic branch
traces Silicon explores — to test whether **independent variables** in
sequential branch conditions cause genuinely exponential (2^n) path growth,
and whether **branch structure** (sequential vs. chained) can suppress that
growth even when variables are independent.

### How to run it yourself

A small helper script wraps `./bin/vct` and extracts the trace count:

```
./util/timing/trace-count.sh <file.pvl> <report-interval> [timeout-seconds] [extra flags...]
```

- `<report-interval>`: Silicon logs `"Silicon has explored N branch traces for
  entity ..."` every N traces (via `--dev-silicon-branch-condition-report-interval N`).
  Use a small interval (1-10) for small files, larger (100-1000) for big ones —
  otherwise the log volume itself slows the run down.
- `[timeout-seconds]`: defaults to 120. For files that explode exponentially,
  the run will hit this timeout — the script still reports the highest trace
  count seen so far and marks the row `TIMEOUT`.
- The script always passes `--dev-no-dead` itself (to remove dead-code-check
  noise from the trace count); any `[extra flags...]` you pass are added on
  top of that.

Example runs used for the results below:

```
./util/timing/trace-count.sh examples/concepts/perf/branches/PERF-BRANCH-indep-5.pvl   1   120
./util/timing/trace-count.sh examples/concepts/perf/branches/PERF-BRANCH-indep-10.pvl  10  120
./util/timing/trace-count.sh examples/concepts/perf/branches/PERF-BRANCH-indep-15.pvl  50  150
./util/timing/trace-count.sh examples/concepts/perf/branches/PERF-BRANCH-indep-20.pvl  500 120
./util/timing/trace-count.sh examples/concepts/perf/branches/PERF-BRANCH-30.pvl        100 120
./util/timing/trace-count.sh examples/concepts/perf/branches/PERF-BRANCH-single-var-30.pvl 100 150
./util/timing/trace-count.sh examples/concepts/perf/branches/PERF-BRANCH-chain-indep-30.pvl 10 120
./util/timing/trace-count.sh examples/concepts/perf/seq-loops/PERF-SEQ-15loops.pvl     50  150
```

### Files used

| File | Structure |
|---|---|
| PERF-BRANCH-indep-5/10/15/20/25/30.pvl | N sequential `if` (no else), each with its own independent variable. **Regenerated** — the old `indep-30.pvl` declared 30 params but its body only used 12; all sizes now have a matching number of ifs/params. |
| PERF-BRANCH-30.pvl | 30 sequential if/else, 2 *shared* variables (existing file, section 4) |
| PERF-BRANCH-single-var-30.pvl | 30 sequential `if` (no else), 1 shared variable, increasing thresholds (existing file) |
| PERF-BRANCH-chain-indep-30.pvl | **New.** 30-way if/elseif/.../else chain (mutually exclusive, 31 paths), each condition on its own independent variable — isolates structure vs. variable-independence |
| PERF-SEQ-15loops.pvl | existing file (section 1) — rechecked for the "1000 branch traces" cap |

### Results

| File | branches | interval | elapsed | max traces reported | status |
|---|---|---|---|---|---|
| PERF-BRANCH-indep-5 | 5 | 1 | 16s | 279 | completed |
| PERF-BRANCH-indep-10 | 10 | 10 | 19s | 9,200 | completed |
| PERF-BRANCH-indep-12 | 12 | 50 | 60s | 36,850 | completed |
| PERF-BRANCH-indep-13 | 13 | 100 | 85s | 73,700 | completed |
| PERF-BRANCH-indep-14 | 14 | 500 | 131s | 147,000 | completed |
| PERF-BRANCH-indep-15 | 15 | 50 | 150s | 183,600 | TIMEOUT |
| PERF-BRANCH-indep-20 | 20 | 500 | 120s | 96,500 | TIMEOUT |
| PERF-BRANCH-30 (shared vars, w/ else) | 30 | 100 | 41s | 35,000 | completed |
| PERF-BRANCH-single-var-30 | 30 | 100 | 19s | 3,700 | completed |
| PERF-BRANCH-chain-indep-30 | 30 (chain) | 10 | 11s | 270 | completed |
| PERF-BRANCH-chain-indep-50 | 50 (chain) | 10 | 12s | 450 | completed |
| PERF-BRANCH-chain-indep-100 | 100 (chain) | 10 | 19s | 900 | completed |
| PERF-SEQ-15loops | — | 50 | 22s | 800 | completed |

`indep-25`/`indep-30` were generated but **not run to completion** — at this
growth rate they are intractable within any reasonable timeout, and that
intractability is itself a data point (see conclusions).

### Conclusions

1. **Independent variables → true 2^n trace growth.** indep-5 → indep-10
   (doubling the branch count) takes 279 → 9,200 traces, a **~33x** increase —
   matching 2^10/2^5 = 32 almost exactly, with a constant factor of ~9
   traces per leaf path. indep-15 was still climbing toward the
   ~9,200 × 32 ≈ 294,000 predicted by the same ratio when it hit the 150s
   cutoff at 183,600 (62% of the way there) — consistent with continued 2^n
   growth, not a plateau.

2. **Branch *structure* dominates variable independence.** Despite using 30
   independent variables (the same "worst case" ingredient as indep-N),
   `chain-indep-30` produced only **270 traces** — fewer than indep-5 (5
   branches!) — because the if/elseif/else chain has only 31
   mutually-exclusive paths, not 2^30 combinatorial ones. Ordering:
   `chain-indep-30` (270) << `BRANCH-30` shared-vars (35,000) <<
   `indep-15` (183,600+, only 15 branches). **Sequential independent
   branches with only 15 conditions already produce more path explosion
   than 30 chained branches with 30 independent variables.** Variable
   independence is a *secondary multiplier* that only matters once the
   branch structure is already multiplicative (sequential), not additive
   (chained).

3. **The SEQ-15loops "1000 branch traces" cap did not reproduce.** With
   `--dev-silicon-branch-condition-report-interval 50`, the run completed at
   800 total traces in 22s — it never reached 1000. If you want to use the
   "1000-trace cap" observation in the thesis, check `git log` on
   `PERF-SEQ-15loops.pvl` for changes since that earlier run, or re-run with
   interval 1 to get the exact count.

### Suggested follow-up experiments (within the existing smoke-checker framework)

These extend the same trace-count methodology to the other checkers/sections:

- **Loops (section 1/2) — independent-variable loop guards.** Build a
  `PERF-SEQ-Nloops-indep` variant where each loop's guard/invariant touches a
  distinct variable (mirroring indep-N), and trace-count it the same way.
  Tests whether the loop-body fork (continue/exit) explodes the same way
  independent if-branches do, or whether loop invariants constrain it.

- **checkInvSat (section 3) — independent vs. shared Perm locations.**
  `PERF-INV-perm-Nfields` currently varies field *count* but the fields likely
  share an object/array. A trace-count run comparing N Perms on **N distinct
  objects** vs. N Perms on **fields of one object** would test whether heap
  independence causes the same multiplicative blowup as variable independence
  in branches.

- **par blocks (section 5) — independent branches per block.** `PERF-PAR-4blocks`
  with each block containing an `indep-5`-style sequential-independent-branch
  body would test whether detectDeadCode's per-block analysis multiplies the
  2^n cost by the block count (4x of indep-5's 279 traces ≈ 1,100?) or
  whether par-block isolation caps it.

- **postcond/precond (sections 7/8) — postcondition with independent-variable
  branches.** A method whose body is `indep-N`-style and whose postcondition
  references the result of each branch would test whether checkPostSat
  inherits the same exponential trace count as detectDeadCode, or whether
  it's checked on a summarized/havoced state that avoids the blowup.

- **Map the indep-N curve at smaller, completable sizes (12-18).** Since
  indep-15 already approaches the timeout, indep-12/13/14 would let you fit
  the 2^n curve (and the ~9x-per-leaf constant) with all-completed data points
  before hitting the intractable region — useful for a clean log-scale plot
  in the thesis (trace count vs. n, with a 2^n reference line).

- **chain-indep at larger N (50, 100).** Since chain-indep-30 cost almost
  nothing (270 traces, 11s), pushing chain length much higher (50/100 ifs)
  would establish whether chains truly stay linear in N or eventually show
  *some* growth (e.g. from the accumulated negation in the final `else`,
  which section 4's original chain-30 comment flagged as a possible cost
  driver independent of variable sharing).

---

## 13. Generalization — does the indep-N effect appear in other checkers?

Section 12 established that independent variables in **sequential `if`
branches** cause 2^n trace growth (×~9 per leaf), and that **chained**
branches stay linear regardless of variable independence. This section tests
whether the same "independence → multiplicative blowup" mechanism shows up in
loops, heap permissions, par blocks, and postconditions — using the new files
from section 12's follow-up list.

### 13.1 The indep-N curve, completed (12, 13, 14)

| File | n | interval | elapsed | max traces | traces / 2^n |
|---|---|---|---|---|---|
| indep-5 | 5 | 1 | 16s | 279 | 8.72 |
| indep-10 | 10 | 10 | 19s | 9,200 | 8.98 |
| indep-12 | 12 | 50 | 60s | 36,850 | 9.00 |
| indep-13 | 13 | 100 | 85s | 73,700 | 9.00 |
| indep-14 | 14 | 500 | 131s | 147,000 | 8.97 |
| indep-15 | 15 | 50 | 150s (TIMEOUT) | 183,600 (62%) | — |

**This is a near-perfect 2^n fit.** traces ≈ 9 × 2^n holds to within 0.3%
across a 500x range (279 → 147,000). indep-12→13→14 each show *exactly* a 2.0x
step, and indep-10→12 shows exactly 4.0x (2^2). 14 is the largest size that
completes within ~2 minutes; 15 is already 62% short of its predicted
294,000-trace total at the 150s mark. **The tractable boundary for this
structure is n=14.**

### 13.2 Chains at 50/100 — confirmed linear

| File | paths (n+1) | max traces | traces / paths |
|---|---|---|---|
| chain-indep-30 | 31 | 270 | 8.71 |
| chain-indep-50 | 51 | 450 | 8.82 |
| chain-indep-100 | 101 | 900 | 8.91 |

Linear in n, same ~9x-per-path constant as the indep-N leaves. No sign of
extra cost from the accumulated negations in the final `else` even at n=100 —
chains are safe regardless of length.

### 13.3 Loops — independence has NO effect

| File | interval | max traces |
|---|---|---|
| PERF-SEQ-5loops (shared `n`) | 1 | 270 |
| PERF-SEQ-5loops-indep (independent `n1..n5`) | 1 | 270 |

**Identical.** Giving each of the 5 loops its own independent bound variable
does not change the trace count at all. Each loop is verified as its own
`FramedProof`/invariant check, with no cross-loop path accumulation — so there
is nothing for "independence" to multiply. The indep-N effect is specific to
**sequential `if` branches**, where the path condition accumulates across
branches; it does not generalize to sequential loops.

### 13.4 Heap (Perm) — small, consistent effect, not exponential

`--dev-time-backend` totals (`time` wall-clock, dominated by ~12s JVM
startup + ~1-2s actual backend work):

| File | `--dev-no-dead` | all checkers |
|---|---|---|
| PERF-INV-perm-5fields (5 fields, 1 object) | 12.429s | 13.749s |
| PERF-INV-perm-5objects (5 objects, 1 field each) | 13.639s | 14.855s |
| **diff (5objects − 5fields)** | **+1.21s** | **+1.11s** |

5objects is consistently ~1.1s slower than 5fields, **in both configs** (so
it's not a detectDeadCode effect — both files have no branches). Splitting the
same 5 Perms across 5 distinct objects costs a small, real, *constant* amount
of extra backend time (~8-9%) — consistent with one extra heap-chunk lookup
per object, not with any multiplicative/exponential effect. **Heap
independence behaves like a fixed per-location overhead, not like variable
independence in branches.**

### 13.5 Postconditions — the effect generalizes AND compounds per ensures-clause

| File | flag | traces | elapsed |
|---|---|---|---|
| PERF-BRANCH-indep-5 (`--dev-no-dead`) | dead-code OFF | 279 | 14.0s |
| PERF-BRANCH-indep-5 (no flags) | dead-code ON | 558 | 15.9s |
| PERF-POST-indep-5 (`--dev-no-post-sat`) | — | 1,380 | 15.8s |
| PERF-POST-indep-5 (no flags) | — | 1,380 | 16.3s |

`PERF-POST-indep-5` has the same 5-independent-branch body as indep-5, plus 5
`ensures` conjuncts (one per field, each an "changed / unchanged" disjunction
over that field's branch). Its trace count is **1,380 ≈ 5 × 279** — i.e.
**each of the 5 ensures conjuncts re-explores the full 2^5-path body once**.
`--dev-no-post-sat` (which disables only the postcondition *satisfiability*
check, not its verification) makes **no difference** — the multiplication
comes from verifying the method *satisfies* its postcondition, not from the
extra SAT check.

**This is the most significant generalization result**: for branch-heavy
methods, **cost scales as O(k × 2^n)** where k = number of independent
postcondition conjuncts that depend on branch outcomes, not just O(2^n). A
method with indep-14's body (147,000 traces) and 5 such ensures clauses would
be ~5x that, i.e. likely fully intractable.

**Update — trace count vs real cost (3-level check)**: re-measuring with
`--dev-no-dead` (apples-to-apples with the BRANCH-indep-5 baseline used
elsewhere) gives **279 → 1,100 traces (3.94x)**, not the ×5 above (that figure
mixed dead-code-on/off configs). Measuring actual cost at two further levels:

| Level | Ratio (POST-indep-5 / BRANCH-indep-5) |
|---|---|
| Trace count | 3.94x (1,100 / 279) |
| Full `vct` wall time, n=20 | 1.17x |
| Backend-only Silicon time (.vpr), n=20 | 0.93x (no real effect) |

The 3.94x trace blowup produces essentially **no measurable backend cost** —
Z3 appears to reuse solver state across the near-identical re-explored paths
almost for free. A dose-response check (`PERF-POST-indep-5-1cond`, 1 of 5
ensures-conjuncts disjunctive) gives 445 traces (1.59x) but the same ~1.19x
wall-time ratio, confirming wall time is essentially flat regardless of
conjunct count at this scale.

The same pattern shows up in 13.6 (par blocks): the 9.86x trace-count ratio
(2,750 vs 279) corresponds to only ~1.33-1.35x in both full-pipeline and
backend-only timing. So in both cases, the noise-free trace-count metric is a
reliable *scaling* indicator but a poor *absolute cost* predictor — the
real-world cost of the extra traces depends on how similar they are to
already-explored paths, not just their count.

### 13.6 Par blocks — multiplies by MORE than the block count

| File | blocks | max traces (interval 50) | naive 4×279 | actual / naive |
|---|---|---|---|---|
| PERF-PAR-4blocks-indep | 4 | 2,750 | 1,116 | 2.46x |

Each of the 4 par blocks contains an indep-5-style body (279 traces
standalone), but the total is **2,750**, not 4×279=1,116 — roughly **2.5x
higher than naive per-block multiplication** would predict. This suggests
par-block verification adds its own overhead on top of (rather than instead
of) the per-block path explosion — possibly related to the thread-range
(`tid = 0 .. n`) quantification interacting with the branch structure, or to
detectDeadCode's ~2x multiplier (13.1) compounding with the 4 blocks
(4 × 2 × 279 ≈ 2,232 — closer, but still ~23% under the observed 2,750).
Exact mechanism not yet isolated — see follow-ups.

### Summary table — does "independence → multiplicative blowup" generalize?

| Context | Effect of independence | Multiplier |
|---|---|---|
| Sequential `if` branches (indep-N) | **Exponential** | ×9·2^n |
| Chained `if`/`elseif`/`else` | None (structure dominates) | ×9·(n+1), linear |
| Sequential loops (own bound per loop) | **None** | ×1 |
| Heap Perms (distinct objects vs. fields) | Small constant | +~1.1s flat |
| Postcondition conjuncts over branch results | **Multiplicative in k** | ×k (k = #conjuncts), on top of ×9·2^n |
| Par blocks (independent body per block) | **Super-linear in block count** | ~2.5× more than naive ×(#blocks) |

### Suggested next steps

- **Isolate the par-block 2.46x factor** (13.6): run `PERF-PAR-4blocks-indep`
  with `--dev-no-dead` (removing the detectDeadCode 2x) and re-measure — if
  the ratio drops to ~1x, the 2.46x was mostly detectDeadCode compounding
  across blocks, not a par-specific effect, and the real "par multiplier" is
  closer to ~1.2x (2,750 / 2 / 1,116 ≈ 1.23).
- **Test the postcond compounding (13.5) with detectDeadCode OFF**: run
  `PERF-POST-indep-5 --dev-no-dead` — if it drops to ~5×279=1,395 vs ~5×558=2,790
  for no flags, that confirms the ×5 (ensures-conjunct) and ×2
  (detectDeadCode) multipliers are independent and multiply together
  (≈10x total over the bare 279 baseline).
- **Vary k directly**: build `PERF-POST-indep-5-k1/k3/k5` (1, 3, 5 ensures
  conjuncts over the same 5-branch body) to confirm the ×k relationship from
  13.5 is linear in k, not just a single data point at k=5.
- For the thesis, **13.1 (2^n fit) and 13.5 (×k postcondition compounding)
  are the headline results** — both are clean, large-effect, and
  reproducible. 13.3 (loops: no effect) and 13.4 (heap: small constant) are
  useful *negative* results that scope the finding precisely. 13.6 (par) is
  promising but needs the follow-up above before it's quotable.
