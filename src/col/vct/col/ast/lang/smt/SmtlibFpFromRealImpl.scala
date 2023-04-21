package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpFromReal
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpFromRealImpl[G] { this: SmtlibFpFromReal[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
