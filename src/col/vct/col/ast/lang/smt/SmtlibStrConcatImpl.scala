package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrConcat
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrConcatImpl[G] { this: SmtlibStrConcat[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
