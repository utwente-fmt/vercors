package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpIsInfinite
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpIsInfiniteImpl[G] { this: SmtlibFpIsInfinite[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
