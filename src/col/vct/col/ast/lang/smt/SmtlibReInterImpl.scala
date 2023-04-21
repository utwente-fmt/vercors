package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReInter
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReInterImpl[G] { this: SmtlibReInter[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
