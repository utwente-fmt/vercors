package vct.col.ast.lang.smt

import vct.col.ast.Z3BvNand
import vct.col.ast.Type
import vct.col.print._

trait Z3BvNandImpl[G] { this: Z3BvNand[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
