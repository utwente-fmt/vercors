package vct.col.ast.expr.heap.read

import vct.col.ast.{
  AddrOf,
  AddrOfConstCast,
  AddrOfUniqueCast,
  AmbiguousSubscript,
  Deref,
  DerefPointer,
  PointerSubscript,
  TConstPointer,
  TNonNullPointer,
  TPointer,
  Type,
  Unique,
}
import vct.col.print._
import vct.col.ast.ops.AddrOfOps
import vct.col.ref.Ref

trait AddrOfImpl[G] extends AddrOfOps[G] {
  this: AddrOf[G] =>
  override lazy val t: Type[G] = {
    e match {
      case DerefPointer(p) => p.t
      case AmbiguousSubscript(p, _) => p.t
      case PointerSubscript(p, _) => p.t
      case AddrOfConstCast(e) => TConstPointer(e.t)
      case AddrOfUniqueCast(e, unique) => TPointer(e.t, Some(unique))
      case d @ Deref(_, Ref(f)) =>
        TNonNullPointer(
          d.t,
          f.flags.collectFirst { case Unique(unique) => unique },
        )
      case _ => TPointer(e.t, None)
    }
  }

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc = Text("&") <> assoc(e)
}
