package vct.col.ast.unsorted

import vct.col.ast.{EndpointIndex, Type}
import vct.col.ast.ops.EndpointIndexOps
import vct.col.print._

trait EndpointIndexImpl[G] extends EndpointIndexOps[G] {
  this: EndpointIndex[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  override def t: Type[G] = name.decl.t
}
