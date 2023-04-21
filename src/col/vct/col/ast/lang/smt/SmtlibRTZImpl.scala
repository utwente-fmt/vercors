package vct.col.ast.lang.smt

import vct.col.ast.SmtlibRTZ
import vct.col.ast.Type
import vct.col.print._

trait SmtlibRTZImpl[G] { this: SmtlibRTZ[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
