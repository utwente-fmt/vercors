package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class ParallelSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/parallel/array_par.pvl"
  vercors should verify using silicon example "concepts/parallel/block-par.pvl"
  vercors should verify using silicon example "concepts/parallel/forward-host.pvl"
  vercors should verify using silicon example "concepts/parallel/ForWithinParallel.pvl"
  vercors should verify using silicon example "concepts/parallel/inv-test.pvl"
  vercors should verify using silicon example "concepts/parallel/inv-test-fail1.pvl"
  vercors should verify using silicon example "concepts/parallel/inv-test-fail2.pvl"
  vercors should verify using silicon example "concepts/parallel/InvariantParallelAtomic.pvl"
  vercors should verify using silicon example "concepts/parallel/kernel-example.pvl"
  vercors should verify using silicon example "concepts/parallel/kernel-example-v2.pvl"
  vercors should verify using silicon example "concepts/parallel/kernel-example-v3.pvl"
  // https://github.com/utwente-fmt/vercors/issues/819
  // vercors should verify using silicon example "concepts/parallel/monotonicBool.pvl"
  vercors should verify using silicon example "concepts/parallel/parallel-example1.pvl"
  vercors should verify using silicon example "concepts/parallel/ParBothRead.pvl"
  vercors should error withCode "resolutionError" example "concepts/parallel/ParBothWrite.pvl"
  vercors should error withCode "resolutionError" example "concepts/parallel/ParIterWrite.pvl"
  vercors should verify using silicon example "concepts/parallel/ParNestedInvariant.pvl"
  vercors should error withCode "resolutionError" example "concepts/parallel/ParNestedInvariantWrite.pvl"
  // https://github.com/utwente-fmt/vercors/issues/815
  // vercors should verify using silicon example "concepts/parallel/summation-kernel-0.pvl"
  // vercors should verify using silicon example "concepts/parallel/summation-kernel-1.pvl"
  // vercors should verify using silicon example "concepts/parallel/zero_mixed_array_2.pvl"
  vercors should verify using silicon example "concepts/parallel/vector-add.pvl"
  vercors should verify using silicon example "concepts/parallel/zero-kernel.pvl"
  // https://github.com/utwente-fmt/vercors/discussions/836
  // vercors should verify using silicon example "concepts/parallel/zero-loop.c"
  vercors should verify using silicon example "concepts/parallel/zero-many.pvl"
  vercors should verify using silicon example "concepts/parallel/zero-sub-array.c"
  vercors should verify using silicon example "concepts/parallel/zero_array_ic.pvl"
  // https://github.com/utwente-fmt/vercors/issues/791
  // vercors should verify using silicon example "concepts/parallel/zero_matrix_ic.pvl"
  vercors should verify using silicon example "concepts/parallel/ZeroArrayIC.java"
}
