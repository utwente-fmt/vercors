package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReContains, TBool, Type}
import vct.col.print._

trait SmtlibReContainsImpl[G] { this: SmtlibReContains[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
