package integration.`new`

import integration.helper.VercorsSpec

class ArrayExamplesSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/arrays/array-example.pvl"
  vercors should verify using silicon example "concepts/arrays/basic-examples.c"
  vercors should verify using silicon example "concepts/arrays/JavaArrayExamples.java"
  vercors should verify using silicon example "concepts/arrays/Transpose.pvl"
  vercors should verify using silicon example "concepts/arrays/zero-array-ic-e1.c"
  vercors should verify using silicon example "concepts/arrays/access-sub-matrix-fail.c"
  vercors should verify using silicon example "concepts/arrays/access-sub-matrix-pass.c"
  vercors should verify using silicon example "concepts/arrays/zero-sub-matrix.c"
  vercors should verify using silicon example "concepts/arrays/array.pvl"
  vercors should verify using silicon example "concepts/arrays/zero_array.pvl"
}
