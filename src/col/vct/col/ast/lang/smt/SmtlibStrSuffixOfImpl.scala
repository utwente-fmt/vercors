package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrSuffixOf, TBool, Type}
import vct.col.print._

trait SmtlibStrSuffixOfImpl[G] {
  this: SmtlibStrSuffixOf[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
