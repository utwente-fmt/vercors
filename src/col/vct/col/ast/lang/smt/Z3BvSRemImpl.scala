package vct.col.ast.lang.smt

import vct.col.ast.Z3BvSRem
import vct.col.ast.Type
import vct.col.print._

trait Z3BvSRemImpl[G] {
  this: Z3BvSRem[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
