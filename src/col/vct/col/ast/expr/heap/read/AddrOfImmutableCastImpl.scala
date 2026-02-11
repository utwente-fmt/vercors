package vct.col.ast.expr.heap.read

import vct.col.ast.{AddrOfImmutableCast, TImmutable}
import vct.col.ast.ops.AddrOfImmutableCastOps
import vct.col.print._

trait AddrOfImmutableCastImpl[G] extends AddrOfImmutableCastOps[G] {
  this: AddrOfImmutableCast[G] =>
  override lazy val t = TImmutable(e.t)

  override def layout(implicit ctx: Ctx): Doc =
    Text("immutableCast(") <> e <> ")"
}
