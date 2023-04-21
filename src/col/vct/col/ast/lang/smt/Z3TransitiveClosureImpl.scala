package vct.col.ast.lang.smt

import vct.col.ast.Z3TransitiveClosure
import vct.col.ast.Type
import vct.col.print._

trait Z3TransitiveClosureImpl[G] { this: Z3TransitiveClosure[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
