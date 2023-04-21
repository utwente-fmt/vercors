package vct.col.ast.lang.smt

import vct.col.ast.Z3BvNor
import vct.col.ast.Type
import vct.col.print._

trait Z3BvNorImpl[G] { this: Z3BvNor[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
