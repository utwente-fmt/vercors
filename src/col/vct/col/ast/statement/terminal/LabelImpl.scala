package vct.col.ast.statement.terminal

import vct.col.ast.Label
import vct.col.print.{Ctx, Doc, Group, NodeDoc, Show, Text}
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
        case Ctx.PVL =>
          Doc.stack(
            Seq(contract, Group(Text("label") <+> ctx.name(decl) <> ";"))
          )
        case Ctx.Silver =>
          Doc.stack(Seq(Group(Text("label") <+> ctx.name(decl)), contract))
        case Ctx.Java =>
          Doc.stack(Seq(contract, Group(Text(ctx.name(decl)) <> ":")))
        case Ctx.C | Ctx.Cuda | Ctx.OpenCL | Ctx.CPP =>
          Doc.stack(Seq(contract, Group(Text(ctx.name(decl)) <> ":")))
      },
    )

  override def layout(implicit ctx: Ctx): Doc = foldBlock(_ <+/> _)
}
