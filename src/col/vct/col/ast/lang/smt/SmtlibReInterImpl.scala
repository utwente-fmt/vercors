package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReInter, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReInterImpl[G] { this: SmtlibReInter[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
