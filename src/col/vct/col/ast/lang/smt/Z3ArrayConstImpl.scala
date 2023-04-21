package vct.col.ast.lang.smt

import vct.col.ast.Z3ArrayConst
import vct.col.ast.Type
import vct.col.print._

trait Z3ArrayConstImpl[G] { this: Z3ArrayConst[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
