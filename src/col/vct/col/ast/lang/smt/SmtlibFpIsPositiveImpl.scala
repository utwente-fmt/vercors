package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpIsPositive
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpIsPositiveImpl[G] { this: SmtlibFpIsPositive[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
