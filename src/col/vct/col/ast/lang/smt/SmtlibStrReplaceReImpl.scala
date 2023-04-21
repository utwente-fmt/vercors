package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrReplaceRe
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrReplaceReImpl[G] { this: SmtlibStrReplaceRe[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
