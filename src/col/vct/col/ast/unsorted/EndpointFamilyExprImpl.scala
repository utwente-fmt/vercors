package vct.col.ast.unsorted

import vct.col.ast.{EndpointFamilyExpr, TSeq, Type}
import vct.col.ast.ops.EndpointFamilyExprOps
import vct.col.ast.serialize.TEndpointFamily
import vct.col.print._

trait EndpointFamilyExprImpl[G] extends EndpointFamilyExprOps[G] {
  this: EndpointFamilyExpr[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(ref))
  override def precedence: Int = Precedence.ATOMIC

  override def t: Type[G] = TSeq(ref.decl.t)
}
