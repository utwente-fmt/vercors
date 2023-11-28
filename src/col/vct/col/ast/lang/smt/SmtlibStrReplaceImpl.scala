package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrReplace, TSmtlibString, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrReplaceOps

trait SmtlibStrReplaceImpl[G] extends SmtlibStrReplaceOps[G] { this: SmtlibStrReplace[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
