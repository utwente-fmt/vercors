package vct.col.ast.lang.smt

import vct.col.ast.{Z3BvXor, Type}
import vct.col.ast.ops.Z3BvXorOps

trait Z3BvXorImpl[G] extends Z3BvXorOps[G] {
  this: Z3BvXor[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
