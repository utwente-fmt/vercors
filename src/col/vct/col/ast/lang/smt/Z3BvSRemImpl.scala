package vct.col.ast.lang.smt

import vct.col.ast.Z3BvSRem
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3BvSRemOps

trait Z3BvSRemImpl[G] extends Z3BvSRemOps[G] { this: Z3BvSRem[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
