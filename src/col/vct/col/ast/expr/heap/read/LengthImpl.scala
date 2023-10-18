package vct.col.ast.expr.heap.read

import vct.col.ast.{Length, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait LengthImpl[G] {
  this: Length[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(arr) <> ".length"
}
