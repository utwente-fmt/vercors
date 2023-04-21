package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpIsZero
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpIsZeroImpl[G] { this: SmtlibFpIsZero[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
