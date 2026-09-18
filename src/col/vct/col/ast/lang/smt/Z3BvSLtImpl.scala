package vct.col.ast.lang.smt

import vct.col.ast.{TBool, Type, Z3BvSLt}
import vct.col.ast.ops.Z3BvSLtOps

trait Z3BvSLtImpl[G] extends Z3BvSLtOps[G] {
  this: Z3BvSLt[G] =>
  override def t: Type[G] = TBool[G]()
  // def layout(implicit ctx: Ctx): Doc = ???
}
