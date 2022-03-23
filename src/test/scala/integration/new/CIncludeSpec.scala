package integration.`new`

import integration.helper.VercorsSpec

class CIncludeSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/clang/c-example-impl.c"
  vercors should verify using anyBackend example "concepts/clang/c-example-use.c"
}
