package vct.col.ast.expr.heap.read

import vct.col.ast.{DerefVeyMontThread, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait DerefVeyMontThreadImpl[G] {
  this: DerefVeyMontThread[G] =>
  override def t: Type[G] = ref.decl.threadType

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(ref))
}
