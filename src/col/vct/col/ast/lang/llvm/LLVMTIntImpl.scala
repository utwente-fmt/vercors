package vct.col.ast.lang.llvm

import vct.col.ast.LLVMTInt
import vct.col.ast.ops.LLVMTIntOps
import vct.col.print._

trait LLVMTIntImpl[G] extends LLVMTIntOps[G] {
  this: LLVMTInt[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
