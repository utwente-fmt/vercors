package vct.col.ast.expr.heap.read

import vct.col.ast.{AddrOf, TPointer, Type, DerefPointer, AmbiguousSubscript}
import vct.col.print._
import vct.col.ast.ops.AddrOfOps

trait AddrOfImpl[G] extends AddrOfOps[G] {
  this: AddrOf[G] =>
  override lazy val t: Type[G] = {
    e match {
      case DerefPointer(p) => p.t
      case AmbiguousSubscript(p, i) => p.t
      case _ => TPointer(e.t, None)
    }
  }

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc = Text("&") <> assoc(e)
}
