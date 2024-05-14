package vct.col.ast.unsorted

import vct.col.ast.{EndpointNameExpr, Type}
import vct.col.ast.ops.EndpointNameExprOps
import vct.col.print._

trait EndpointNameExprImpl[G] extends EndpointNameExprOps[G] { this: EndpointNameExpr[G] =>
  override def t: Type[G] = name.t

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = name.layout
}
