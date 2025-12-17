package vct.col.ast.expr.heap.alloc

import vct.col.ast.{Expr, PointerConstructor, Type}
import vct.col.origin.{ArraySizeError, Blame}
import vct.col.print._

trait PointerConstructorImpl[G] {
  this: PointerConstructor[G] =>
  val blame: Blame[ArraySizeError]
  val element: Type[G]
  val size: Expr[G]

  override def precedence: Int = Precedence.POSTFIX
}
