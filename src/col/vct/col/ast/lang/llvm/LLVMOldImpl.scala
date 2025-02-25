package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMOldOps
import vct.col.ast.{LLVMOld, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait LLVMOldImpl[G] extends LLVMOldOps[G] {
  this: LLVMOld[G] =>
  override def t: Type[G] = v.decl.t

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("old(") <> ctx.name(v.decl) <> ")"
}
