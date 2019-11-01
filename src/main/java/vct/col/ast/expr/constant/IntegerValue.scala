package vct.col.ast.expr.constant

/** Represents a constant integer with value "`value`". */
case class IntegerValue(val value:Int) extends Value {
  override def toString() = Integer.toString(value)
  override def equals(o:Any) = o.equals(value)
}
