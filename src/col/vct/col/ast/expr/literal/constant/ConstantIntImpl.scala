package vct.col.ast.expr.literal.constant

import vct.col.ast.ConstantInt

trait ConstantIntImpl[G] {
  this: ConstantInt[G] =>
  override def value: BigInt
}
