package vct.col.ast.family.location

import vct.col.ast.InstancePredicateLocation
import vct.col.print._
import vct.col.ast.ops.InstancePredicateLocationOps

trait InstancePredicateLocationImpl[G] extends InstancePredicateLocationOps[G] { this: InstancePredicateLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(predicate) <> "(" <> Doc.args(args) <> ")")
}
