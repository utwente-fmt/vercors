package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvURem
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBvURemImpl[G] { this: SmtlibBvURem[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
