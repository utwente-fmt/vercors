package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpRem
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpRemImpl[G] { this: SmtlibFpRem[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
