package vct.col.ast.expr.heap.read

import vct.col.ast.ops.AddrOfUniqueCastOps
import vct.col.ast.{AddrOfUniqueCast, TUnique, Type}
import vct.col.print._

trait AddrOfUniqueCastImpl[G] extends AddrOfUniqueCastOps[G] {
  this: AddrOfUniqueCast[G] =>
  override lazy val t: Type[G] = TUnique(e.t, unique)

  override def layout(implicit ctx: Ctx): Doc =
    Text("uniqueCast(") <> e <> s", $unique)"
}
