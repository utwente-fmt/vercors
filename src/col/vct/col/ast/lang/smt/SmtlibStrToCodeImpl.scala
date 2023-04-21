package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrToCode
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrToCodeImpl[G] { this: SmtlibStrToCode[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
