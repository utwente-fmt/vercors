package vct.col.ast.expr.constant

import scala.annotation.nowarn

/** Represents a constant double with value "`value`". */
@nowarn("msg=.*comparing values of types Any and .* using `equals`.*")
case class DoubleValue(val value:Double) extends Value {
  override def equals(a:Any) = a.equals(value)
  override def toString() = value.toString()
}
