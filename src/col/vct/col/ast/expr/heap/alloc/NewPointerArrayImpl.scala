package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewPointerArray, TPointer, Type}
import vct.col.print._
import vct.col.ast.ops.NewPointerArrayOps

trait NewPointerArrayImpl[G] extends NewPointerArrayOps[G] {
  this: NewPointerArray[G] =>
  override lazy val t: Type[G] = TPointer[G](element, unique)

  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <>
      (if (unique.nonEmpty)
         Text(" unique<" + unique.get.toString + ">")
       else
         Text("")) <+> element <> "[" <> size <> "]"
}
