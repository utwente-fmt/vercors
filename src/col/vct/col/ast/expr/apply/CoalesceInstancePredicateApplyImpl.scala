package vct.col.ast.expr.apply

import vct.col.ast.CoalesceInstancePredicateApply
import vct.col.print._
import vct.col.ast.ops.CoalesceInstancePredicateApplyOps

trait CoalesceInstancePredicateApplyImpl[G] extends CoalesceInstancePredicateApplyOps[G] { this: CoalesceInstancePredicateApply[G] =>
  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("[") <> perm <> "]" <> obj.bind(Precedence.POSTFIX) <> "?." <> ctx.name(ref) <> "(" <> Doc.args(args) <> ")")
}
