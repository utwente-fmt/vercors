package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReNone
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReNoneImpl[G] { this: SmtlibReNone[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
