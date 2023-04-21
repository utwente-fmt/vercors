package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReRepeat
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReRepeatImpl[G] { this: SmtlibReRepeat[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
