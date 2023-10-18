package vct.col.ast.family.location

import vct.col.ast.InstancePredicateLocation
import vct.col.print._

trait InstancePredicateLocationImpl[G] {
  this: InstancePredicateLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(predicate) <> "(" <>
        Doc.args(args) <> ")"
    )
}
