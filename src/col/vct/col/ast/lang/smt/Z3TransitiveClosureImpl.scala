package vct.col.ast.lang.smt

import vct.col.ast.{TBool, Type, Z3TransitiveClosure}
import vct.col.print._
import vct.col.ast.ops.Z3TransitiveClosureOps

trait Z3TransitiveClosureImpl[G] extends Z3TransitiveClosureOps[G] {
  this: Z3TransitiveClosure[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
