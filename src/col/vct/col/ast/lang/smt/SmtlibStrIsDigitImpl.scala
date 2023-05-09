package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrIsDigit, TBool, Type}
import vct.col.print._

trait SmtlibStrIsDigitImpl[G] { this: SmtlibStrIsDigit[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
