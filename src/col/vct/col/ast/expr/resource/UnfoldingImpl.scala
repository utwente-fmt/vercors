package vct.col.ast.expr.resource

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{Type, Unfolding}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.UnfoldingOps

trait UnfoldingImpl[G] extends NodeFamilyImpl[G] with UnfoldingOps[G] {
  this: Unfolding[G] =>
  override def t: Type[G] = body.t

  def layoutPVL(implicit ctx: Ctx): Doc =
    Group(Text("unfolding") <+> res.show <+> "in" <>> body)

  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(Text("unfolding") <+> res.show <+> "in" <>> body)

  def layoutJava(implicit ctx: Ctx): Doc =
    Group(Doc.inlineSpec(Text("\\Unfolding") <+> res.show <+> "\\in") <>> body)

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(Text("\\unfolding") <+> res.show <+> "\\in" <>> body)

  override def precedence: Int = Precedence.PVL_UNFOLDING
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.PVL => layoutPVL
      case Ctx.Silver => layoutSilver
      case Ctx.Java => layoutJava
      case _ => layoutSpec
    }
}
