package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class ArrayExamplesSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/arrays/array-example.pvl"

  // https://github.com/utwente-fmt/vercors/issues/815
  // vercors should verify using silicon example "concepts/arrays/fibSearch.java"
  // vercors should verify using silicon example "concepts/arrays/JavaArrayExamples.java"

  // https://github.com/utwente-fmt/vercors/issues/791
  // vercors should verify using silicon example "concepts/arrays/Transpose.pvl"

  // https://github.com/utwente-fmt/vercors/issues/820
  // vercors should verify using silicon example "concepts/arrays/access-sub-matrix-fail.c"
  // vercors should verify using silicon example "concepts/arrays/access-sub-matrix-pass.c"
  // vercors should verify using silicon example "concepts/arrays/zero-sub-matrix.c"

  // https://github.com/utwente-fmt/vercors/discussions/836
  // vercors should verify using silicon example "concepts/arrays/zero-array-ic-e1.c"
  // vercors should verify using silicon example "concepts/arrays/basic-examples.c"

  vercors should verify using silicon example "concepts/arrays/array.pvl"
  vercors should verify using silicon example "concepts/arrays/zero_array.pvl"
  vercors should verify using silicon example "concepts/arrays/array-problem.pvl"
}
