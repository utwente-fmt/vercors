package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpAbs
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpAbsImpl[G] { this: SmtlibFpAbs[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
