package vct.col.ast.expr.resource

import vct.col.ast.{Scale, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait ScaleImpl[G] { this: Scale[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("[") <> scale <> "]" <> assoc(res)
}