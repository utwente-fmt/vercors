package vct.col.ast.expr.constant

import scala.annotation.nowarn

@nowarn("msg=.*comparing values of types Any and .* using `equals`.*")
case class BooleanValue(val value:Boolean) extends Value {
  override def toString() = if (value) "true" else "false"
  override def equals(o:Any) = o.equals(value)
}
