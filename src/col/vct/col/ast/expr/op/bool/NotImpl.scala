package vct.col.ast.expr.op.bool

import vct.col.ast.{Not, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait NotImpl[G] { this: Not[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc = Text("!") <> assoc(arg)
}