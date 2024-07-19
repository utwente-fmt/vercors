package vct.col.ast.expr.heap.alloc

import vct.col.ast.{Expr, NewPointer, Type}
import vct.col.origin.{ArraySizeError, Blame}
import vct.col.print._

trait NewPointerImpl[G] {
  this: NewPointer[G] =>
  val blame: Blame[ArraySizeError]
  val element: Type[G]
  val size: Expr[G]

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> element <> "[" <> size <> "]"
}
