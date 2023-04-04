package vct.col.ast.expr.apply

import vct.col.ast.CoalesceInstancePredicateApply
import vct.col.print._

trait CoalesceInstancePredicateApplyImpl[G] { this: CoalesceInstancePredicateApply[G] =>
  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("[") <> perm <> "]" <> bind(obj, Precedence.POSTFIX) <> "?." <> ctx.name(ref) <> "(" <> Doc.args(args) <> ")")
}
