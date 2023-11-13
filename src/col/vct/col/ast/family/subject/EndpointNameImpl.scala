package vct.col.ast.family.subject

import vct.col.ast.EndpointName
import vct.col.print.{Ctx, Doc, Text}

trait EndpointNameImpl[G] { this: EndpointName[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(ref))
}
