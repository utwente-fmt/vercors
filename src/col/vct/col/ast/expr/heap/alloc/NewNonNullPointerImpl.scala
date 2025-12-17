package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewNonNullPointer, TNonNullPointer, Type}
import vct.col.print._
import vct.col.ast.ops.NewNonNullPointerOps

trait NewNonNullPointerImpl[G] extends NewNonNullPointerOps[G] {
  this: NewNonNullPointer[G] =>
  override lazy val t: Type[G] = TNonNullPointer(element, unique)

  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <>
      (if (unique.nonEmpty)
         Text(" unique<" + unique.get.toString + ">")
       else
         Text("")) <+> element <> "[" <> size <> "]"
}
