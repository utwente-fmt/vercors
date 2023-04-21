package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpFromSInt
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpFromSIntImpl[G] { this: SmtlibFpFromSInt[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
