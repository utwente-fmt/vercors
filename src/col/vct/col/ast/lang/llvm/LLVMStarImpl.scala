package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMStarOps
import vct.col.ast.{LLVMStar, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait LLVMStarImpl[G] extends LLVMStarOps[G] {
  this: LLVMStar[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.AND
  override def layout(implicit ctx: Ctx): Doc =
    Text("(") <> left.decl.show <+> "**" <+> right.decl.show <> Text(")")
}
