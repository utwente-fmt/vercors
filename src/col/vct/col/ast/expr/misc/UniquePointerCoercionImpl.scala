package vct.col.ast.expr.misc

import vct.col.ast.UniquePointerCoercion
import vct.col.ast.ops.UniquePointerCoercionOps
import vct.col.print._

trait UniquePointerCoercionImpl[G] extends UniquePointerCoercionOps[G] { this: UniquePointerCoercion[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
