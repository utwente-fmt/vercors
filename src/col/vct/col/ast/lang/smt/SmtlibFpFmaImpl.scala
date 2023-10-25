package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpFma
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpFmaImpl[G] { this: SmtlibFpFma[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
