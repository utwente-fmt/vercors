package vct.col.ast.expr.context

import vct.col.ast.{LocalThreadId, TInt}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait LocalThreadIdImpl[G] {
  this: LocalThreadId[G] =>
  override def t: TInt[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("\\ltid")
}
