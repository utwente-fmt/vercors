package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReInter, TSmtlibRegLan, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibReInterOps

trait SmtlibReInterImpl[G] extends SmtlibReInterOps[G] {
  this: SmtlibReInter[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
