package vct.col.ast.expr.heap.read

import vct.col.ast.{Length, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.LengthOps

trait LengthImpl[G] extends LengthOps[G] {
  this: Length[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(arr) <> ".length"
}
