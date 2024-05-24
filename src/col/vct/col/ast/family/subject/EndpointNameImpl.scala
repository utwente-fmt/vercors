package vct.col.ast.family.subject

import vct.col.ast.EndpointName
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.EndpointNameOps

trait EndpointNameImpl[G] extends EndpointNameOps[G] { this: EndpointName[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(ref))

  override def cls = ref.decl.cls.decl
}
