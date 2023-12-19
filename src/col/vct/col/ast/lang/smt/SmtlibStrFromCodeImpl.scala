package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrFromCode, TSmtlibString, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrFromCodeOps

trait SmtlibStrFromCodeImpl[G] extends SmtlibStrFromCodeOps[G] { this: SmtlibStrFromCode[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
