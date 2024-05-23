package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class VcllvmSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/llvm/cantor.c"
  vercors should verify using silicon example "concepts/llvm/cantor.ll"
  vercors should verify using silicon example "concepts/llvm/date.c"
  vercors should verify using silicon example "concepts/llvm/date.ll"
  vercors should verify using silicon example "concepts/llvm/fib.c"
  vercors should verify using silicon example "concepts/llvm/fib.ll"
}
