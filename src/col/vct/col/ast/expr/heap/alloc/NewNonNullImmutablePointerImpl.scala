package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewNonNullImmutablePointer, Type, TNonNullImmutablePointer}
import vct.col.print._
import vct.col.ast.ops.NewNonNullImmutablePointerOps

trait NewNonNullImmutablePointerImpl[G]
    extends NewNonNullImmutablePointerOps[G] {
  this: NewNonNullImmutablePointer[G] =>
  override lazy val t: Type[G] = TNonNullImmutablePointer[G](element)

  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> "immutable" <+> element <> "[" <> size <> "]"
}
