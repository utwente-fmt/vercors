package vct.col.ast.expr.heap.alloc

import vct.col.ast.ops.NewPointerArrayOps
import vct.col.ast.{NewPointerArray, TNonNullPointerArray, Type}
import vct.col.print._

trait NewPointerArrayImpl[G] extends NewPointerArrayOps[G] {
  this: NewPointerArray[G] =>
  override def t: Type[G] =
    TNonNullPointerArray(element, dimensions.map(Some(_)), unique)
  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> unique.map(u => Text(s"unique<$u>") <+> element)
      .getOrElse(element.show) <> dimensions.mkString("[", ",", "]")
}
