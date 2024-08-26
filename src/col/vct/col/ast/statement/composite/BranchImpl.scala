package vct.col.ast.statement.composite

import vct.col.ast.{BooleanValue, Branch, Expr, Statement}
import vct.col.print._
import vct.col.ast.ops.BranchOps

trait BranchImpl[G] extends BranchOps[G] {
  this: Branch[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.fold(branches.zipWithIndex.map {
      case ((BooleanValue(true), body), i) if i == branches.size - 1 =>
        body.layoutAsBlock
      case ((cond, body), _) =>
        Group(Text("if (") <> Doc.arg(cond) <> ")") <+> body.layoutAsBlock
    })(_ <+> "else" <+> _)

  def isBinary: Boolean = 1 <= branches.length && branches.length <= 2

  def cond: Expr[G] = {
    require(isBinary)
    branches.head._1
  }

  def yes: Statement[G] = {
    require(isBinary)
    branches.head._2
  }

  def no: Option[Statement[G]] = {
    require(isBinary)
    branches.tail.headOption.map(_._2)
  }
}
