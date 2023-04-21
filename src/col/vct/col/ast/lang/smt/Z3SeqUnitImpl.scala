package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqUnit
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqUnitImpl[G] { this: Z3SeqUnit[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
