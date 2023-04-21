package vct.col.ast.lang.smt

import vct.col.ast.SmtlibToFp
import vct.col.ast.Type
import vct.col.print._

trait SmtlibToFpImpl[G] { this: SmtlibToFp[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
