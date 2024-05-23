package vct.col.ast.expr.literal.constant

import vct.col.ast.{FloatValue}
import vct.col.print._
import vct.col.ast.ops.FloatValueOps

trait FloatValueImpl[G] extends FloatValueOps[G] {
  this: FloatValue[G] =>
  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text(value.toString()) // PB: this should change when we reconsider floats.
}
