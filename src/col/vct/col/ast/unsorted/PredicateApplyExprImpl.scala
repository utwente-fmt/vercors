package vct.col.ast.unsorted

import vct.col.ast.{AbstractPredicate, Expr, PredicateApplyExpr}
import vct.col.ast.ops.PredicateApplyExprOps
import vct.col.print._
import vct.col.ref.Ref

trait PredicateApplyExprImpl[G] extends PredicateApplyExprOps[G] {
  this: PredicateApplyExpr[G] =>
  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc = apply.show
  def ref: Ref[G, _ <: AbstractPredicate[G]] = apply.ref
  def args: Seq[Expr[G]] = apply.args
}
