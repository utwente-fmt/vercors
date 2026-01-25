package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewImmutablePointer, TImmutablePointer, Type}
import vct.col.print._
import vct.col.ast.ops.NewImmutablePointerOps

trait NewImmutablePointerImpl[G] extends NewImmutablePointerOps[G] {
  this: NewImmutablePointer[G] =>
  override lazy val t: Type[G] = TImmutablePointer[G](element)

  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> "immutable" <+> element <> "[" <> size <> "]"
}
