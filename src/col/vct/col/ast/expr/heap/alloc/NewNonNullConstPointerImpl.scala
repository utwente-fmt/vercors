package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewNonNullConstPointer, Type, TNonNullConstPointer}
import vct.col.print._
import vct.col.ast.ops.NewNonNullConstPointerOps

trait NewNonNullConstPointerImpl[G] extends NewNonNullConstPointerOps[G] {
  this: NewNonNullConstPointer[G] =>
  override lazy val t: Type[G] = TNonNullConstPointer[G](element)

  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> "const" <+> element <> "[" <> size <> "]"
}
