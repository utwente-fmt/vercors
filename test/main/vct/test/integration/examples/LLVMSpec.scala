package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class LLVMSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/llvm/cantor.c"
  vercors should verify using silicon example "concepts/llvm/cantor.ll"
  vercors should verify using silicon example "concepts/llvm/date.c"
  vercors should fail withCode "preFailed:false" using silicon example "concepts/llvm/date.ll"
  vercors should verify using silicon example "concepts/llvm/fib.c"
  vercors should verify using silicon example "concepts/llvm/fib.ll"
  vercors should verify using silicon example "concepts/llvm/cubed.c"
  vercors should verify using silicon flags("--contract-import-file", "examples/concepts/llvm/cubed-contracts.pvl") example "concepts/llvm/cubed.ll"
  vercors should verify using silicon flags("--contract-import-file", "examples/concepts/llvm/void-contracts.pvl") example "concepts/llvm/void.ll"
  vercors should fail withCode "unreachable" using silicon in "reaching an 'unreachable' statement" llvm
  """
  define void @foo() {
    unreachable
  }
  """
  vercors should verify using silicon in "an unreachable 'unreachable' statement" llvm
    """
  define void @foo() {
    br i1 0, label %1, label %2
    1: unreachable
    2: ret void
  }
  """

  // Pallas specifications:
  vercors should verify using silicon example "concepts/llvm/pallas/pallas_function_contract.ll"
  vercors should fail withCode "postFailed:false" using silicon example "concepts/llvm/pallas/pallas_function_contract_fail.ll"
  vercors should verify using silicon example "concepts/llvm/pallas/pallas_result.ll"
  vercors should verify using silicon example "concepts/llvm/pallas/pallas_c_perm.ll"
  vercors should fail withCode "postFailed:perm" using silicon example "concepts/llvm/pallas/pallas_c_perm_fail_1.ll"
  vercors should fail withCode "ptrPerm" using silicon example "concepts/llvm/pallas/pallas_c_perm_fail_2.ll"
  vercors should fail withCode "postFailed:perm" using silicon example "concepts/llvm/pallas/pallas_c_perm_fail_3.ll"
  vercors should verify using silicon example "concepts/llvm/pallas/pallas_c_old.ll"
  vercors should fail withCode "postFailed:false" using silicon example "concepts/llvm/pallas/pallas_c_old_fail.ll"
  vercors should verify using silicon example "concepts/llvm/pallas/pallas_c_quantifier.ll"
  vercors should fail withCode "postFailed:false" using silicon example "concepts/llvm/pallas/pallas_c_quantifier_fail.ll"
  vercors should verify using silicon example "concepts/llvm/pallas/pallas_c_multiply.ll"
  vercors should verify using silicon example "concepts/llvm/pallas/pallas_c_lower_bound.ll"
  vercors should fail withCode "notMaintained:false" using silicon example "concepts/llvm/pallas/pallas_c_square_fail.ll"
  vercors should verify using silicon example "concepts/llvm/pallas/pallas_c_fibonacci.ll"
}
