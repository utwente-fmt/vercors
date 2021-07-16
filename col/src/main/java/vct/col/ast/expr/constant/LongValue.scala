package vct.col.ast.expr.constant

import scala.annotation.nowarn

@nowarn("msg=.*comparing values of types Any and .* using `equals`.*")
case class LongValue(val value: Long) extends Value {
  override def equals(obj: Any) = obj.equals(this.value)

  override def toString() = value.toString()
}
