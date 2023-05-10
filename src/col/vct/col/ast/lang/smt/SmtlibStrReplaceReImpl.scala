package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrReplaceRe, TSmtlibString, Type}
import vct.col.print._

trait SmtlibStrReplaceReImpl[G] { this: SmtlibStrReplaceRe[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
