package vct.col.ast.lang.llvm

import vct.col.ast.{Expr, LLVMArithOpWithOverflow}
import vct.col.origin.{AssignFailed, Blame}
import vct.col.print._

trait LLVMArithOpWithOverflowImpl[G] {
  this: LLVMArithOpWithOverflow[G] =>
  def target: Expr[G]
  def left: Expr[G]
  def right: Expr[G]
  def signed: Boolean
  def blame: Blame[AssignFailed]

  def instRepr: Doc

  override def layout(implicit ctx: Ctx): Doc = {
    Group(
      target.show <+> Text("=") <+> instRepr <+> Text("(") <+> left.show <+>
        Text(", ") <+> right.show <+> Text(")")
    )
  }
}
