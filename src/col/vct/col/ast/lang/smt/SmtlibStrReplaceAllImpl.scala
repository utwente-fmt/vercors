package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrReplaceAll
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrReplaceAllImpl[G] { this: SmtlibStrReplaceAll[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
