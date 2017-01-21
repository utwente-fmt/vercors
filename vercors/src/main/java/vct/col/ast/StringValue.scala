package vct.col.ast

class StringValue(val value:String) extends Value {
  def getStripped = value.substring(1, value.length() - 1)
  override def equals(o:Any) = o.equals(value)
  override def toString = value
}