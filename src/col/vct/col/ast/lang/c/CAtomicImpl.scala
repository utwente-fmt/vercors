package vct.col.ast.lang.c

import vct.col.ast.CAtomic
import vct.col.ast.ops.CAtomicOps
import vct.col.print.{Ctx, Doc, Text}

trait CAtomicImpl[G] extends CAtomicOps[G] {
  this: CAtomic[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("_Atomic")
}
