package vct.col.ast.expr.resource

import vct.col.ast.{PointsTo, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait PointsToImpl[G] {
  this: PointsTo[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("PointsTo(") <> Doc.args(Seq(loc, perm, value)) <> ")")
}
