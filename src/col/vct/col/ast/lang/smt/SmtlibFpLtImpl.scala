package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpLt, TBool, Type}
import vct.col.print._

trait SmtlibFpLtImpl[G] { this: SmtlibFpLt[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
