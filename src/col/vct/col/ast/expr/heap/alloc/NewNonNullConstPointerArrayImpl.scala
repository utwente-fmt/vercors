package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewNonNullConstPointerArray, Type, TNonNullConstPointer}
import vct.col.ast.ops.NewNonNullConstPointerArrayOps
import vct.col.print._

trait NewNonNullConstPointerArrayImpl[G]
    extends NewNonNullConstPointerArrayOps[G] {
  this: NewNonNullConstPointerArray[G] =>
  override lazy val t: Type[G] = TNonNullConstPointer[G](element)

  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> "const" <+> element <> "[" <> size <> "]"
}
