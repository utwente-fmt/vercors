package vct.col.ast.lang.smt

import vct.col.ast.{TBool, Type, Z3SeqContains}
import vct.col.print._
import vct.col.ast.ops.Z3SeqContainsOps

trait Z3SeqContainsImpl[G] extends Z3SeqContainsOps[G] {
  this: Z3SeqContains[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
