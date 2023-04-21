package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpNeg
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpNegImpl[G] { this: SmtlibFpNeg[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
