package vct.col.ast.expr.heap.read

import vct.col.ast.ops.PointerArraySubscriptOps
import vct.col.ast.{PointerArraySubscript, TPointerArray, Type}
import vct.col.print._

trait PointerArraySubscriptImpl[G] extends PointerArraySubscriptOps[G] {
  this: PointerArraySubscript[G] =>
  override def t: Type[G] = {
    val arrayT = array.t.asPointerArray.get
    if (arrayT.dimensions.length == 1) { arrayT.element }
    else { arrayT.descend }
  }
  override def layout(implicit ctx: Ctx): Doc =
    array.show <> "[" <> index <> "]"
}
