package vct.col.ast.lang.smt

import vct.col.ast.Z3BvSShr
import vct.col.ast.Type
import vct.col.print._

trait Z3BvSShrImpl[G] {
  this: Z3BvSShr[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
