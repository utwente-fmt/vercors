package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrReplaceRe, TSmtlibString, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrReplaceReOps

trait SmtlibStrReplaceReImpl[G] extends SmtlibStrReplaceReOps[G] { this: SmtlibStrReplaceRe[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
