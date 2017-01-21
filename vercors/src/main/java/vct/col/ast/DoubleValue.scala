package vct.col.ast

class DoubleValue(val value:Double) extends Value {
  override def equals(a:Any) = a.equals(value)
  override def toString() = value.toString()
}