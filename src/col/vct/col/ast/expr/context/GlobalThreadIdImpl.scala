package vct.col.ast.expr.context

import vct.col.ast.{GlobalThreadId, TInt}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.GlobalThreadIdOps

trait GlobalThreadIdImpl[G] extends GlobalThreadIdOps[G] { this: GlobalThreadId[G] =>
  override def t: TInt[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("\\gtid")
}
