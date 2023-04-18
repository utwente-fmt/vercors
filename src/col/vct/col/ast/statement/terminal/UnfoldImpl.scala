package vct.col.ast.statement.terminal

import vct.col.ast.Unfold
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.util.CheckFoldUnfoldTarget
import vct.col.print.{Ctx, Doc, Show, Text}

trait UnfoldImpl[G] extends NodeFamilyImpl[G] with CheckFoldUnfoldTarget[G] { this: Unfold[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("unfold") <+> res <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc =
    Text("unfold") <+> res

  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => layoutSilver
    case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
  }
}