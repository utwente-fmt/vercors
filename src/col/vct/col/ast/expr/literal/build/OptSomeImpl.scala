package vct.col.ast.expr.literal.build

import vct.col.ast.{OptSome, TOption, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait OptSomeImpl[G] {
  this: OptSome[G] =>
  override def t: Type[G] = TOption(e.t)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = Text("Some(") <> e <> ")"
}
