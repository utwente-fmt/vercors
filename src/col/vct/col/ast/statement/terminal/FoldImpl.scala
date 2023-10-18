package vct.col.ast.statement.terminal

import vct.col.ast.Fold
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.util.CheckFoldUnfoldTarget
import vct.col.print.{Ctx, Doc, Show, Text}

trait FoldImpl[G] extends NodeFamilyImpl[G] with CheckFoldUnfoldTarget[G] {
  this: Fold[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("fold") <+> res <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc = Text("fold") <+> res

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }
}
