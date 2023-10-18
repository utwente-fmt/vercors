package vct.col.ast.lang.smt

import vct.col.ast.{TBool, Type, Z3SeqContains}
import vct.col.print._

trait Z3SeqContainsImpl[G] {
  this: Z3SeqContains[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
