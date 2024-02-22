package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemoryUnordered
import vct.col.ast.ops.LLVMMemoryUnorderedOps
import vct.col.print._

trait LLVMMemoryUnorderedImpl[G] extends LLVMMemoryUnorderedOps[G] {
  this: LLVMMemoryUnordered[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
