package vct.col.ast.expr.literal.constant

import vct.col.ast.Constant

trait ConstantImpl[G] { this: Constant[G] =>
  def value: Any

  override def equals(obj: scala.Any): Boolean = obj match {
    case const: Constant[_] => this.value == const.value
    case _ => false
  }

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString
}
