package vct.col.ast.expr.misc

import vct.col.ast.{Any, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.AnyOps

trait AnyImpl[G] extends AnyOps[G] {
  this: Any[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("*")
}
