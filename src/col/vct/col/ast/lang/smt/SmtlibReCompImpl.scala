package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReComp, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReCompImpl[G] {
  this: SmtlibReComp[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
