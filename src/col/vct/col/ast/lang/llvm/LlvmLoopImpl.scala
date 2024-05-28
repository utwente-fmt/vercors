package vct.col.ast.lang.llvm

import vct.col.ast.LlvmLoop
import vct.col.ast.ops.LlvmLoopOps

trait LlvmLoopImpl[G] extends LlvmLoopOps[G] {
  this: LlvmLoop[G] =>

}
