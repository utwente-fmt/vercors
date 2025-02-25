package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReAllChar, TSmtlibRegLan, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibReAllCharOps

trait SmtlibReAllCharImpl[G] extends SmtlibReAllCharOps[G] {
  this: SmtlibReAllChar[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
