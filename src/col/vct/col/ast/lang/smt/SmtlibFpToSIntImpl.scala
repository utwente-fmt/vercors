package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpToSInt
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpToSIntImpl[G] { this: SmtlibFpToSInt[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
