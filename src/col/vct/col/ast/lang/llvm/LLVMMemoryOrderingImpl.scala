package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemoryOrdering
import vct.col.ast.ops.LLVMMemoryOrderingFamilyOps
import vct.col.print._

trait LLVMMemoryOrderingImpl[G] extends LLVMMemoryOrderingFamilyOps[G] {
  this: LLVMMemoryOrdering[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
