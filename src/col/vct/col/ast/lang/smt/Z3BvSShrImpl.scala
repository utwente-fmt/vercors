package vct.col.ast.lang.smt

import vct.col.ast.Z3BvSShr
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3BvSShrOps

trait Z3BvSShrImpl[G] extends Z3BvSShrOps[G] { this: Z3BvSShr[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
