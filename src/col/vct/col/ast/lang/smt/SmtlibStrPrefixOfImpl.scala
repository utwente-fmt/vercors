package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrPrefixOf, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrPrefixOfOps

trait SmtlibStrPrefixOfImpl[G] extends SmtlibStrPrefixOfOps[G] {
  this: SmtlibStrPrefixOf[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
