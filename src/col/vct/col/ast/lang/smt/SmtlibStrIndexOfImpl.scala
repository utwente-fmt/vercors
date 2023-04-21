package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrIndexOf
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrIndexOfImpl[G] { this: SmtlibStrIndexOf[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
