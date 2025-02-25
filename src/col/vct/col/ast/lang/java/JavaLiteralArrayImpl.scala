package vct.col.ast.lang.java

import vct.col.ast.{JavaLiteralArray, Type}
import vct.col.print.Precedence
import vct.col.ast.ops.JavaLiteralArrayOps

trait JavaLiteralArrayImpl[G] extends JavaLiteralArrayOps[G] {
  this: JavaLiteralArray[G] =>
  override def t: Type[G] = typeContext.get

  override def precedence = Precedence.ATOMIC
}
