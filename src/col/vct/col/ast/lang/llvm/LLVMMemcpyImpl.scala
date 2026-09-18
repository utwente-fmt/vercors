package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemcpy
import vct.col.ast.ops.LLVMMemcpyOps
import vct.col.print._

trait LLVMMemcpyImpl[G] extends LLVMMemcpyOps[G] {
  this: LLVMMemcpy[G] =>

  override def layout(implicit ctx: Ctx): Doc = {
    Text("memcpy(") <+> dst.show <+> Text(", ") <+> src.show <+> Text(", ") <+>
      len.show <+> Text(", ") <+> volatile.show <+> Text(")")
  }
}
