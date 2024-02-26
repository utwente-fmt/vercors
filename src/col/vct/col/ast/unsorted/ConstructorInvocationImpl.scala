package vct.col.ast.unsorted

import vct.col.ast.ConstructorInvocation
import vct.col.ast.ops.ConstructorInvocationOps
import vct.col.print._

trait ConstructorInvocationImpl[G] extends ConstructorInvocationOps[G] { this: ConstructorInvocation[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
