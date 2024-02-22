package vct.col.ast.lang.llvm

import vct.col.ast.LLVMLoop
import vct.col.ast.ops.LLVMLoopOps

trait LLVMLoopImpl[G] extends LLVMLoopOps[G] {
  this: LLVMLoop[G] =>

}
