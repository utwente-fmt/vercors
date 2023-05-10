package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReOpt, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReOptImpl[G] { this: SmtlibReOpt[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
