package vct.col.ast.expr.resource

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.ops.AssertingOps
import vct.col.ast.{Type, Asserting}
import vct.col.print._

trait AssertingImpl[G] extends NodeFamilyImpl[G] with AssertingOps[G] {
  this: Asserting[G] =>
  override def t: Type[G] = body.t

  def layoutPVL(implicit ctx: Ctx): Doc =
    Group(Text("asserting") <+> condition.show <+> "in" <>> body)

  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(Text("asserting") <+> condition.show <+> "in" <>> body)

  def layoutJava(implicit ctx: Ctx): Doc =
    Group(
      Doc.inlineSpec(Text("\\asserting") <+> condition.show <+> "\\in") <>> body
    )

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(Text("\\asserting") <+> condition.show <+> "\\in" <>> body)

  override def precedence: Int = Precedence.PVL_UNFOLDING
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.PVL => layoutPVL
      case Ctx.Silver => layoutSilver
      case Ctx.Java => layoutJava
      case _ => layoutSpec
    }
}
