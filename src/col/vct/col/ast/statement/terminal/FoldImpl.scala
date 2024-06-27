package vct.col.ast.statement.terminal

import vct.col.ast.{AmbiguousFoldTarget, Expr, Fold, PredicateApplyExpr, Scale, ScaledPredicateApply}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.FoldOps
import vct.col.origin.PanicBlame

trait FoldImpl[G] extends NodeFamilyImpl[G] with FoldOps[G] {
  this: Fold[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("fold") <+> res <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc = Text("fold") <+> res

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }

  override def expr: Expr[G] =
    res match {
      case ScaledPredicateApply(inv, perm) =>
        Scale(perm, PredicateApplyExpr(inv))(PanicBlame("TODO: wire through when #1012 is fixed"))
      case AmbiguousFoldTarget(e) => e
    }
}
