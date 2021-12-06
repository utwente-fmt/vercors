package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.Constant

trait ConstantImpl[T] { this: Constant[T] =>
  def value: T

  override def equals(obj: scala.Any): Boolean = obj match {
    case const: Constant[T] => this.value == const.value
    case _ => false
  }

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString
}
