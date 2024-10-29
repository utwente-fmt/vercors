package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReNone, TSmtlibRegLan, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibReNoneOps

trait SmtlibReNoneImpl[G] extends SmtlibReNoneOps[G] {
  this: SmtlibReNone[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
