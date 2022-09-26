package vct.col.ast.expr.constant

import scala.annotation.nowarn

object IntegerValue {
  def unapply(iv: IntegerValue): Option[Int] = iv.toInt()
}

/** Represents a constant integer with value "`value`". */
@nowarn("msg=.*comparing values of types Any and .* using `equals`.*")
case class IntegerValue(val value:BigInt) extends Value {

  override def toString() = value.toString()
  override def equals(o:Any) = o.equals(value)

  def toInt(): Option[Int] = if (BigInt(Integer.MIN_VALUE) <= value && value <= BigInt(Integer.MAX_VALUE)) {
    Some(value.toInt)
  } else {
    None
  }
}
