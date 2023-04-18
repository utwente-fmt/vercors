package vct.col.ast.lang

import vct.col.ast.CGoto
import vct.col.print.{Ctx, Doc, Text}

trait CGotoImpl[G] { this: CGoto[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("goto") <+> label <> ";"
}