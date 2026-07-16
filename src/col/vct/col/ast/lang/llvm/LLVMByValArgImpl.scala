package vct.col.ast.lang.llvm

import vct.col.ast.LLVMByValArg
import vct.col.ast.ops.LLVMByValArgOps
import vct.col.print.{Ctx, Doc, Text}

trait LLVMByValArgImpl[G] extends LLVMByValArgOps[G] {
  this: LLVMByValArg[G] =>

  override def layout(implicit ctx: Ctx): Doc = {
    Text("byval(") <> t.show <> Text(")")
  }
}
