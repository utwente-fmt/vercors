package vct.col.ast.lang.smt

import vct.col.ast.Z3ArrayOfFunction
import vct.col.ast.Type
import vct.col.print._

trait Z3ArrayOfFunctionImpl[G] { this: Z3ArrayOfFunction[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
