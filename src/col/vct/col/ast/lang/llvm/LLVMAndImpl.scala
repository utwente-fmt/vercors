package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMAndOps
import vct.col.ast.{LLVMAnd, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}

trait LLVMAndImpl[G] extends LLVMAndOps[G] {
  this: LLVMAnd[G] =>

  override def t: Type[G] = right.decl.t

  override def precedence: Int = Precedence.AND
  override def layout(implicit ctx: Ctx): Doc =
    Group(left.decl.show <+> "&&" <>> right.decl.show)
}
