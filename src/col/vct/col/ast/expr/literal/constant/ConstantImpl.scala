package vct.col.ast.expr.literal.constant

import vct.col.ast.Constant

trait ConstantImpl[G, T] { this: Constant[G, T] =>
  def value: T

  override def equals(obj: scala.Any): Boolean = obj match {
    case const: Constant[_, T] => this.value == const.value
    case _ => false
  }

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString
}
