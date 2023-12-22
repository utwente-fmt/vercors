package vct.col.ast.expr.heap.read

import vct.col.ast.{EndpointUse, TClass, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.EndpointUseOps

trait EndpointUseImpl[G] extends EndpointUseOps[G] { this: EndpointUse[G] =>
  // TODO: Shouldn't there be the field that is dereferenced be involved somehow...?
  override def t: Type[G] = ref.decl.t

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(ref))
}
