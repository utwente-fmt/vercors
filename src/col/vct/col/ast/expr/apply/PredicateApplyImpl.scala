package vct.col.ast.expr.apply

import vct.col.ast.PredicateApply
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Precedence, Text}

trait PredicateApplyImpl[G] {
  this: PredicateApply[G] =>
  override def precedence: Int = Precedence.PREFIX

  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(
      Text("acc(") <> ctx.name(ref) <> "(" <> Doc.args(args) <> "), " <> perm <>
        ")"
    )

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(
      Text("[") <> perm <> "]" <> ctx.name(ref) <> "(" <> Doc.args(args) <> ")"
    )

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => layoutSpec
    }
}
