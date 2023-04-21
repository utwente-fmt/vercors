package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqContains
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqContainsImpl[G] { this: Z3SeqContains[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
