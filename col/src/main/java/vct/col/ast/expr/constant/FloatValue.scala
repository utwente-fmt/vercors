package vct.col.ast.expr.constant

import scala.annotation.nowarn

/** Represents a constant float with value "`value`". */
@nowarn("msg=.*comparing values of types Any and .* using `equals`.*")
case class FloatValue(val value:Float) extends Value {
  override def equals(a:Any) = a.equals(value)
  override def toString() = value.toString()
}
