package vct.col.ast.expr.heap.alloc

import vct.col.ast.ops.NewConstPointerArrayOps
import vct.col.ast.{NewConstPointerArray, TNonNullConstPointerArray, Type}
import vct.col.print._

trait NewConstPointerArrayImpl[G] extends NewConstPointerArrayOps[G] {
  this: NewConstPointerArray[G] =>
  override def t: Type[G] =
    TNonNullConstPointerArray(element, dimensions.map(Some(_)))
  override def layout(implicit ctx: Ctx): Doc =
    Text("new const") <+> element <> dimensions.mkString("[", ",", "]")
}
