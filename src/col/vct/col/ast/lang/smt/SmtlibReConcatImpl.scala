package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReConcat, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReConcatImpl[G] {
  this: SmtlibReConcat[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
