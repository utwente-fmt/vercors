package vct.col.ast.lang.smt

import vct.col.ast.Z3BvNor
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3BvNorOps

trait Z3BvNorImpl[G] extends Z3BvNorOps[G] {
  this: Z3BvNor[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
