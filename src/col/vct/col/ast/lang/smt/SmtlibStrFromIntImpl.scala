package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrFromInt
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrFromIntImpl[G] { this: SmtlibStrFromInt[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
