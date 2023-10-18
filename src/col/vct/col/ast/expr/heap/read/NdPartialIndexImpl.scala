package vct.col.ast.expr.heap.read

import vct.col.ast.{NdPartialIndex, TBool}
import vct.col.print._

trait NdPartialIndexImpl[G] {
  this: NdPartialIndex[G] =>
  override def t: TBool[G] = TBool()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("\\nd_partial_index(") <>
        Doc.args(indices ++ Seq(linearIndex) ++ dimensions) <> ")"
    )
}
