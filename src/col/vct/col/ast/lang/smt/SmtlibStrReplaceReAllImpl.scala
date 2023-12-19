package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrReplaceReAll, TSmtlibString, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrReplaceReAllOps

trait SmtlibStrReplaceReAllImpl[G] extends SmtlibStrReplaceReAllOps[G] { this: SmtlibStrReplaceReAll[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
