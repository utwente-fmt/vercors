package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMOrOps
import vct.col.ast.{LLVMOr, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}

trait LLVMOrImpl[G] extends LLVMOrOps[G] {
  this: LLVMOr[G] =>

  override def t: Type[G] = right.decl.t

  override def precedence: Int = Precedence.OR

  override def layout(implicit ctx: Ctx): Doc =
    Group(left.decl.show <+> "&&" <>> right.decl.show)
}
