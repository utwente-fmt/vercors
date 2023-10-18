package vct.col.ast.lang

import vct.col.ast.{JavaLiteralArray, Type}
import vct.col.print.Precedence

trait JavaLiteralArrayImpl[G] {
  this: JavaLiteralArray[G] =>
  override def t: Type[G] = typeContext.get

  override def precedence = Precedence.ATOMIC
}
