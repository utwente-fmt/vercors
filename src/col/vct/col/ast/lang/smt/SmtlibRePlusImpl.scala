package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibRePlus, TSmtlibRegLan, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibRePlusOps

trait SmtlibRePlusImpl[G] extends SmtlibRePlusOps[G] {
  this: SmtlibRePlus[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
