package vct.col.ast.expr.heap.alloc

import vct.col.ast.ops.NewNonNullPointerArrayOps
import vct.col.ast.{NewNonNullPointerArray, TNonNullPointer, Type}
import vct.col.print._

trait NewNonNullPointerArrayImpl[G] extends NewNonNullPointerArrayOps[G] {
  this: NewNonNullPointerArray[G] =>
  override lazy val t: Type[G] = TNonNullPointer(element)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> element <> "[" <> size <> "]"
}
