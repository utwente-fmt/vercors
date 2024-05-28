package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReOpt, TSmtlibRegLan, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibReOptOps

trait SmtlibReOptImpl[G] extends SmtlibReOptOps[G] {
  this: SmtlibReOpt[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
