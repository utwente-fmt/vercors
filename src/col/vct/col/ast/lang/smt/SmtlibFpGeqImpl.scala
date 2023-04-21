package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpGeq
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpGeqImpl[G] { this: SmtlibFpGeq[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
