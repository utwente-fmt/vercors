package vct.col.ast.statement.composite

import vct.col.ast.{Block, Node}
import vct.col.print._

trait BlockImpl[G] { this: Block[G] =>
  override def layout(implicit ctx: Ctx): Doc = layoutAsBlock
  override def foldBlock(f: (Doc, Doc) => Doc)(implicit ctx: Ctx): Doc = InfoDoc(Doc.fold(statements.map(_.foldBlock(f)))(f))(this)
}