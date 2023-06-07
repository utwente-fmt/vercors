package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpGt, TBool, Type}
import vct.col.print._

trait SmtlibFpGtImpl[G] { this: SmtlibFpGt[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
