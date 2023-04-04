package vct.col.ast.expr.apply

import vct.col.ast.InstancePredicateApply
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait InstancePredicateApplyImpl[G] { this: InstancePredicateApply[G] =>
  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("[") <> perm <> "]" <> bind(obj, Precedence.POSTFIX) <> "." <> ctx.name(ref) <> "(" <> Doc.args(args) <> ")")
}
