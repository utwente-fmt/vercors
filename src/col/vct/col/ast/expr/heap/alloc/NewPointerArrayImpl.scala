package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewPointerArray, TPointer, Type}
import vct.col.print._
import vct.col.ast.ops.NewPointerArrayOps

trait NewPointerArrayImpl[G] extends NewPointerArrayOps[G] { this: NewPointerArray[G] =>
  override lazy val t: Type[G] = TPointer(element)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> element <> "[" <> size <> "]"
}