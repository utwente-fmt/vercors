package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrToCode, TInt, Type}
import vct.col.print._

trait SmtlibStrToCodeImpl[G] { this: SmtlibStrToCode[G] =>
  override def t: Type[G] = TInt()
  // def layout(implicit ctx: Ctx): Doc = ???
}
