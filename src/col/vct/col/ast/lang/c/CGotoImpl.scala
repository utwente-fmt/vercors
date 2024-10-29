package vct.col.ast.lang.c

import vct.col.ast.CGoto
import vct.col.ast.ops.CGotoOps
import vct.col.print.{Ctx, Doc, Text}

trait CGotoImpl[G] extends CGotoOps[G] {
  this: CGoto[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("goto") <+> label <> ";"
}
