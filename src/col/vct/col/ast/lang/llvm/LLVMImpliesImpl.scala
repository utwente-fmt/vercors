package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMImpliesOps
import vct.col.ast.{LLVMImplies, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}

trait LLVMImpliesImpl[G] extends LLVMImpliesOps[G] {
  this: LLVMImplies[G] =>

  override def t: Type[G] = right.decl.t

  override def precedence: Int = Precedence.IMPLIES
  override def layout(implicit ctx: Ctx): Doc =
    Group(left.decl.show <+> "==>" <>> right.decl.show)
}
