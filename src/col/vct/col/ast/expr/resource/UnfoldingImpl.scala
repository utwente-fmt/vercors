package vct.col.ast.expr.resource

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.util.CheckFoldUnfoldTarget
import vct.col.ast.{Type, Unfolding}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait UnfoldingImpl[G] extends NodeFamilyImpl[G] with CheckFoldUnfoldTarget[G] { this: Unfolding[G] =>
  override def t: Type[G] = body.t

  def layoutPVL(implicit ctx: Ctx): Doc =
    Group(Text("unfolding") <+> assoc(res) <+> "in" <>> body)

  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(Text("unfolding") <+> assoc(res) <+> "in" <>> body)

  def layoutJava(implicit ctx: Ctx): Doc =
    Group(Text("\\Unfolding") <+> assoc(res) <+> "\\in" <>> body)

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(Text("\\unfolding") <+> assoc(res) <+> "\\in" <>> body)

  override def precedence: Int = Precedence.PVL_UNFOLDING
  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.PVL => layoutPVL
    case Ctx.Silver => layoutSilver
    case Ctx.Java => layoutJava
    case _ => layoutSpec
  }
}