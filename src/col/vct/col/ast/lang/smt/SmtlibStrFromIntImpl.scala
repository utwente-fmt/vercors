package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrFromInt, TSmtlibString, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrFromIntOps

trait SmtlibStrFromIntImpl[G] extends SmtlibStrFromIntOps[G] {
  this: SmtlibStrFromInt[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
