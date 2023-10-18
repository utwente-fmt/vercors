package vct.col.ast.lang.smt

import vct.col.ast.Z3BvSMod
import vct.col.ast.Type
import vct.col.print._

trait Z3BvSModImpl[G] {
  this: Z3BvSMod[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
