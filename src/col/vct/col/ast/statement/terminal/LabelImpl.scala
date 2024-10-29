package vct.col.ast.statement.terminal

import vct.col.ast.Label
import vct.col.print.{Ctx, Doc, NodeDoc, Show, Text}
import vct.col.ast.ops.LabelOps

trait LabelImpl[G] extends LabelOps[G] {
  this: Label[G] =>
  override def foldBlock(f: (Doc, Doc) => Doc)(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.PVL => f(layoutLabel, stat.foldBlock(f))
      case Ctx.Silver => f(layoutLabel, stat.foldBlock(f))
      case Ctx.Java => layoutLabel <+> stat.show
      case Ctx.C | Ctx.Cuda | Ctx.OpenCL | Ctx.CPP =>
        f(layoutLabel, stat.foldBlock(f))
    }

  def layoutLabel(implicit ctx: Ctx): Doc =
    NodeDoc(
      this,
      ctx.syntax match {
        case Ctx.PVL => Text("label") <+> ctx.name(decl) <> ";"
        case Ctx.Silver => Text("label") <+> ctx.name(decl)
        case Ctx.Java => Text(ctx.name(decl)) <> ":"
        case Ctx.C | Ctx.Cuda | Ctx.OpenCL | Ctx.CPP =>
          Text(ctx.name(decl)) <> ":"
      },
    )

  override def layout(implicit ctx: Ctx): Doc = foldBlock(_ <+/> _)
}
