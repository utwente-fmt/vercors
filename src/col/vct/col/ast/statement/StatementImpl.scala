package vct.col.ast.statement

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast._
import vct.col.check.{CheckContext, CheckError, SeqProgStatement}
import vct.col.print._
import vct.col.ast.ops.StatementFamilyOps

trait StatementImpl[G] extends NodeFamilyImpl[G] with StatementFamilyOps[G] { this: Statement[G] =>
  def layoutAsBlock(implicit ctx: Ctx): Doc =
    Text("{") <>> foldBlock(_ <+/> _) <+/> "}"

  def foldBlock(f: (Doc, Doc) => Doc)(implicit ctx: Ctx): Doc = show

}
