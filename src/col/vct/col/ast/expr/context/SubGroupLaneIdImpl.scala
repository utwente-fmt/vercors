package vct.col.ast.expr.context

import vct.col.ast.ops.SubGroupLaneIdOps
import vct.col.ast.{SubGroupLaneId, TInt}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait SubGroupLaneIdImpl[G] extends SubGroupLaneIdOps[G] {
  this: SubGroupLaneId[G] =>
  override def t: TInt[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("\\sgtid")
}
