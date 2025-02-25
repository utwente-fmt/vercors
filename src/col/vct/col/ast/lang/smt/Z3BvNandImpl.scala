package vct.col.ast.lang.smt

import vct.col.ast.Z3BvNand
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3BvNandOps

trait Z3BvNandImpl[G] extends Z3BvNandOps[G] {
  this: Z3BvNand[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
