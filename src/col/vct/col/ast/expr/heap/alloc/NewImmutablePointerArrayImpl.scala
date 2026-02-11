package vct.col.ast.expr.heap.alloc

import vct.col.ast.ops.NewImmutablePointerArrayOps
import vct.col.ast.{
  NewImmutablePointerArray,
  TNonNullImmutablePointerArray,
  Type,
}
import vct.col.print._

trait NewImmutablePointerArrayImpl[G] extends NewImmutablePointerArrayOps[G] {
  this: NewImmutablePointerArray[G] =>
  override def t: Type[G] =
    TNonNullImmutablePointerArray(element, dimensions.map(Some(_)))
  override def layout(implicit ctx: Ctx): Doc =
    Text("new immutable") <+> element <> dimensions.mkString("[", ",", "]")
}
