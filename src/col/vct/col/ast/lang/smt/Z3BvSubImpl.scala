package vct.col.ast.lang.smt

import vct.col.ast.Z3BvSub
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3BvSubOps

trait Z3BvSubImpl[G] extends Z3BvSubOps[G] { this: Z3BvSub[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
