package vct.col.ast.expr.constant

import scala.annotation.nowarn

/** Represents a constant integer with value "`value`". */
@nowarn("msg=.*comparing values of types Any and .* using `equals`.*")
case class IntegerValue(val value: Int) extends Value {
  override def toString() = Integer.toString(value)

  override def equals(o: Any) = o.equals(value)
}
