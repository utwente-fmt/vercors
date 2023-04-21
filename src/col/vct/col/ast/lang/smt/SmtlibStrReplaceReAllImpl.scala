package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrReplaceReAll
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrReplaceReAllImpl[G] { this: SmtlibStrReplaceReAll[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
