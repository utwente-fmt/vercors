package vct.col.ast.family.location

import vct.col.ast.PredicateLocation
import vct.col.print.{Ctx, Doc, Group, Text}

trait PredicateLocationImpl[G] {
  this: PredicateLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text(ctx.name(predicate)) <> "(" <> Doc.args(args) <> ")")
}
