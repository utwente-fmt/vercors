package vct.col.ast.lang.smt

import vct.col.ast.{TBool, Type, Z3TransitiveClosure}
import vct.col.print._

trait Z3TransitiveClosureImpl[G] {
  this: Z3TransitiveClosure[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
