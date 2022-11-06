package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class PointerSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/pointerArithmetic/pointerSubtract.c"
}
