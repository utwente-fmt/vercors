package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrIsDigit
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrIsDigitImpl[G] { this: SmtlibStrIsDigit[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
