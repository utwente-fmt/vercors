package vct.col.ast.statement.composite

import vct.col.ast.Block
import vct.col.print._

trait BlockImpl[G] { this: Block[G] =>
  override def layout(implicit ctx: Ctx): Doc = layoutAsBlock
  override def blockElementsForLayout(implicit ctx: Ctx): Seq[Show] =
    statements.flatMap(_.blockElementsForLayout)
}