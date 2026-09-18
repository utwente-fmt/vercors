package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemset
import vct.col.ast.ops.LLVMMemsetOps
import vct.col.print.{Text, _}

trait LLVMMemsetImpl[G] extends LLVMMemsetOps[G] {
  this: LLVMMemset[G] =>

  override def layout(implicit ctx: Ctx): Doc = {
    Text("memset(") <+> dest.show <+> Text(", ") <+> value.show <+>
      Text(", ") <+> len.show <+> Text(", ") <+> volatile.show <+> Text(")")
  }
}
