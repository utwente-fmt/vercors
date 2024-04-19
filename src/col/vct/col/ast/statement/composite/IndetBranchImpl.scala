package vct.col.ast.statement.composite

import vct.col.ast.IndetBranch
import vct.col.ast.ops.IndetBranchOps
import vct.col.ast.statement.StatementImpl
import vct.col.print._

trait IndetBranchImpl[G] extends StatementImpl[G] with IndetBranchOps[G] { this: IndetBranch[G] =>
  override def foldBlock(f: (Doc, Doc) => Doc)(implicit ctx: Ctx): Doc =
    branches match {
      case Seq(branch) => NodeDoc(this, branch.foldBlock(f))
      case _ => show
    }

  override def layout(implicit ctx: Ctx): Doc =
    Doc.fold(branches.zipWithIndex.map {
      case (branch, i) if i == branches.size - 1 =>
        branch.layoutAsBlock
      case (branch, _) =>
        Text("if(*)") <+> branch.layoutAsBlock
    })(_ <+> "else" <+> _)
}
