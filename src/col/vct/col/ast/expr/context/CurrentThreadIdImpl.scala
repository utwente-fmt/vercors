package vct.col.ast.expr.context

import vct.col.ast.{CurrentThreadId, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.CurrentThreadIdOps

trait CurrentThreadIdImpl[G] extends CurrentThreadIdOps[G] {
  this: CurrentThreadId[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("\\current_thread")
}
