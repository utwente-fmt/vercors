package vct.col.ast.expr.literal.constant

import vct.col.ast.FloatInf
import vct.col.ast.ops.FloatInfOps
import vct.col.print._

trait FloatInfImpl[G] extends FloatInfOps[G] {
  this: FloatInf[G] =>
  val value: Double = Double.PositiveInfinity
  override def layout(implicit ctx: Ctx): Doc = Text("inf")
}
