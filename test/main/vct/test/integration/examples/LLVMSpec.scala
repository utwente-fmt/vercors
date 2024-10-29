package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class LLVMSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/llvm/cantor.c"
  vercors should verify using silicon example "concepts/llvm/cantor.ll"
  vercors should verify using silicon example "concepts/llvm/date.c"
  vercors should fail withCode "preFailed:false" using silicon example "concepts/llvm/date.ll"
  vercors should verify using silicon example "concepts/llvm/fib.c"
  vercors should verify using silicon example "concepts/llvm/fib.ll"
}
