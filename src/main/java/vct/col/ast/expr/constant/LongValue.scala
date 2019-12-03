package vct.col.ast.expr.constant

case class LongValue(val value:Long) extends Value {
  override def equals(obj:Any) = obj.equals(this.value)
  override def toString() = value.toString()
}
