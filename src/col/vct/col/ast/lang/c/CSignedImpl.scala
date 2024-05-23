package vct.col.ast.lang.c

import vct.col.ast.CSigned
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CSignedOps

trait CSignedImpl[G] extends CSignedOps[G] { this: CSigned[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("signed")
}