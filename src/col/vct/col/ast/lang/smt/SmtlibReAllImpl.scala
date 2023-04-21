package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReAll
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReAllImpl[G] { this: SmtlibReAll[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
