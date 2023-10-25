package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrPrefixOf, TBool, Type}
import vct.col.print._

trait SmtlibStrPrefixOfImpl[G] { this: SmtlibStrPrefixOf[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
