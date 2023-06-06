package vct.col.ast.statement.terminal

import vct.col.ast.Label
import vct.col.print.{Ctx, Doc, Show, Text}

trait LabelImpl[G] { this: Label[G] =>
  override def blockElementsForLayout(implicit ctx: Ctx): Seq[Show] = ctx.syntax match {
    case Ctx.PVL => Seq(layoutLabel) ++ stat.blockElementsForLayout
    case Ctx.Silver => Seq(layoutLabel) ++ stat.blockElementsForLayout
    case Ctx.Java => Seq(this)
    case Ctx.C | Ctx.Cuda | Ctx.OpenCL => Seq(layoutLabel) ++ stat.blockElementsForLayout
  }

  def layoutLabel(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.PVL => Text("label") <+> ctx.name(decl) <> ";"
    case Ctx.Silver => Text("label") <+> ctx.name(decl)
    case Ctx.Java => Text(ctx.name(decl)) <> ":"
    case Ctx.C | Ctx.Cuda | Ctx.OpenCL => Text(ctx.name(decl)) <> ":"
  }

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(blockElementsForLayout)
}