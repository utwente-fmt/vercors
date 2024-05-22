package vct.col.ast.statement.composite

import vct.col.ast.Block
import vct.col.print._
import vct.col.ast.ops.BlockOps

trait BlockImpl[G] extends BlockOps[G] { this: Block[G] =>
  override def layout(implicit ctx: Ctx): Doc = layoutAsBlock
  override def foldBlock(f: (Doc, Doc) => Doc)(implicit ctx: Ctx): Doc = NodeDoc(this, Doc.fold(statements.map(_.foldBlock(f)))(f))
}