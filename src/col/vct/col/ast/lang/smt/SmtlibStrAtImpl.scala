package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrAt, TSmtlibString, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrAtOps

trait SmtlibStrAtImpl[G] extends SmtlibStrAtOps[G] {
  this: SmtlibStrAt[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
