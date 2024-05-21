package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrIsDigit, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrIsDigitOps

trait SmtlibStrIsDigitImpl[G] extends SmtlibStrIsDigitOps[G] { this: SmtlibStrIsDigit[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
