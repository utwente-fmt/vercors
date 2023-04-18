package vct.col.ast.expr.op.bool

import vct.col.ast.{Or, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait OrImpl[G] { this: Or[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.OR
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "||", right)
}