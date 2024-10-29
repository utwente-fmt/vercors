package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrSuffixOf, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrSuffixOfOps

trait SmtlibStrSuffixOfImpl[G] extends SmtlibStrSuffixOfOps[G] {
  this: SmtlibStrSuffixOf[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
