package vct.col.ast.expr.heap.read

import vct.col.ast.{AddrOfConstCast, TConst}
import vct.col.ast.ops.AddrOfConstCastOps
import vct.col.print._

trait AddrOfConstCastImpl[G] extends AddrOfConstCastOps[G] { this: AddrOfConstCast[G] =>
  override lazy val t = TConst(e.t)

  override def layout(implicit ctx: Ctx): Doc = Text("constCast(") <> e <> ")"
}
