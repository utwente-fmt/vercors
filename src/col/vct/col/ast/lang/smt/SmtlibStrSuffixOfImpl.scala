package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrSuffixOf
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrSuffixOfImpl[G] { this: SmtlibStrSuffixOf[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
