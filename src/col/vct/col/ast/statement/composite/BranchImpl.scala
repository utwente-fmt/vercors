package vct.col.ast.statement.composite

import vct.col.ast.{Branch, BooleanValue}
import vct.col.print._
import vct.col.ast.ops.BranchOps

trait BranchImpl[G] extends BranchOps[G] { this: Branch[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.fold(branches.zipWithIndex.map {
      case ((BooleanValue(true), body), i) if i == branches.size-1 =>
        body.layoutAsBlock
      case ((cond, body), _) =>
        Group(Text("if (") <> Doc.arg(cond) <> ")") <+> body.layoutAsBlock
    })(_ <+> "else" <+> _)
}