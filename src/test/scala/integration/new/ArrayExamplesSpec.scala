package integration.`new`

import integration.helper.VercorsSpec

class ArrayExamplesSpec extends VercorsSpec {
  vercors should verify using silicon example "arrays/array-example.pvl"
  vercors should verify using silicon example "arrays/basic-examples.c"
  vercors should verify using silicon example "arrays/JavaArrayExamples.java"
  vercors should verify using silicon example "arrays/Transpose.pvl"
  vercors should verify using silicon example "arrays/zero-array-ic-e1.c"
  vercors should verify using silicon example "arrays/access-sub-matrix-fail.c"
  vercors should verify using silicon example "arrays/access-sub-matrix-pass.c"
  vercors should verify using silicon example "arrays/zero-sub-matrix.c"
}
