package vct.col.ast.statement.terminal

import vct.col.ast.Extract
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.ExtractOps

trait ExtractImpl[G] extends ExtractOps[G] { this: Extract[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Text("extract")) <+> contractedStatement
}
