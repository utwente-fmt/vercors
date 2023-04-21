package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrPrefixOf
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrPrefixOfImpl[G] { this: SmtlibStrPrefixOf[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
