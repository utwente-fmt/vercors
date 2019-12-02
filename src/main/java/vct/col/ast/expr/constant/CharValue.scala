package vct.col.ast.expr.constant

/** Represents a constant integer with value "`value`". */
case class CharValue(val value:Char) extends Value {
  override def toString() = value + ""
  override def equals(o:Any) = o.equals(value)
}
