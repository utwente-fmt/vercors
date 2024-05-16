package vct.col.ast.expr.heap.read

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{EndpointName, TClass, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.EndpointNameOps
import vct.col.ast.ops.{EndpointNameOps}

trait EndpointNameImpl[G] extends NodeFamilyImpl[G] with EndpointNameOps[G] { this: EndpointName[G] =>
  def t: Type[G] = ref.decl.t

  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(ref))
}
