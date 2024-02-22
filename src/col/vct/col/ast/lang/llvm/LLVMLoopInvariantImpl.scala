package vct.col.ast.lang.llvm

import vct.col.ast.LLVMLoopInvariant
import vct.col.ast.ops.LLVMLoopInvariantOps

trait LLVMLoopInvariantImpl[G] extends LLVMLoopInvariantOps[G] {
  this: LLVMLoopInvariant[G] =>

}
