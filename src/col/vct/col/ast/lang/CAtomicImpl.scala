package vct.col.ast.lang

import vct.col.ast.CAtomic
import vct.col.print.{Ctx, Doc, Text}

trait CAtomicImpl[G] {
  this: CAtomic[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("_Atomic")
}
