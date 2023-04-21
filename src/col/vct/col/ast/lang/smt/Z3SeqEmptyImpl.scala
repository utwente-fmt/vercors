package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqEmpty
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqEmptyImpl[G] { this: Z3SeqEmpty[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
