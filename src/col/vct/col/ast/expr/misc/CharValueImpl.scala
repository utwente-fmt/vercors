package vct.col.ast.expr.misc

import vct.col.ast.{CharValue, TChar, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.CharValueOps

trait CharValueImpl[G] extends CharValueOps[G] {
  this: CharValue[G] =>
  def t: Type[G] = TChar()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("'") <> Character.toString(value) <> "'"
}
