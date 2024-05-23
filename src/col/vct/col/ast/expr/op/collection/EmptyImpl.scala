package vct.col.ast.expr.op.collection

import vct.col.ast.{Empty, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.EmptyOps

trait EmptyImpl[G] extends EmptyOps[G] {
  this: Empty[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(obj) <> ".isEmpty"
}
