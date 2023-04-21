package vct.col.ast.lang.smt

import vct.col.ast.Z3BvSub
import vct.col.ast.Type
import vct.col.print._

trait Z3BvSubImpl[G] { this: Z3BvSub[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
