package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReContains
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReContainsImpl[G] { this: SmtlibReContains[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
