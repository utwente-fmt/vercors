package vct.col.ast.statement.terminal

import vct.col.ast.Extract
import vct.col.print.{Ctx, Doc, Show, Text}

trait ExtractImpl[G] {
  this: Extract[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.inlineSpec(Text("extract")) <+> contractedStatement
}
