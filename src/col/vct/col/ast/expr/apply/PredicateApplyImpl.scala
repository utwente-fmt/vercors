package vct.col.ast.expr.apply

import vct.col.ast.{PredicateApply, WritePerm}
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Precedence, Text}
import vct.col.ast.ops.PredicateApplyOps

trait PredicateApplyImpl[G] extends PredicateApplyOps[G] {
  this: PredicateApply[G] =>
  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(
      Text("acc(") <> ctx.name(ref) <> "(" <> Doc.args(args) <> "), write)"
    )

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(
      Text(ctx.name(ref)) <> "(" <> Doc.args(args) <> ")"
    )

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => layoutSpec
    }
}
