package vct.col.ast.expr.heap.read

import vct.col.ast.ops.PointerToAdtOps
import vct.col.ast.{PointerToAdt, TAxiomatic}
import vct.col.print._

trait PointerToAdtImpl[G] extends PointerToAdtOps[G] {
  this: PointerToAdt[G] =>

  require(t.isInstanceOf[TAxiomatic[_]])

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("to_") <> t <> "(" <> pointer <> ")")
}
