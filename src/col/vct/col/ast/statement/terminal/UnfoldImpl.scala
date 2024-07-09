package vct.col.ast.statement.terminal

import vct.col.ast.{AmbiguousFoldTarget, Expr, PredicateApplyExpr, Scale, ScaledPredicateApply, Unfold}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.UnfoldOps
import vct.col.origin.PanicBlame

trait UnfoldImpl[G] extends NodeFamilyImpl[G] with UnfoldOps[G] {
  this: Unfold[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("unfold") <+> res <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc = Text("unfold") <+> res

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
