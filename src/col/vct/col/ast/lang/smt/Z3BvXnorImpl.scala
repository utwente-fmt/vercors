package vct.col.ast.lang.smt

import vct.col.ast.Z3BvXnor
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3BvXnorOps

trait Z3BvXnorImpl[G] extends Z3BvXnorOps[G] { this: Z3BvXnor[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
