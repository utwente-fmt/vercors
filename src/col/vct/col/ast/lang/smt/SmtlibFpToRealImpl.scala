package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpToReal
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpToRealImpl[G] { this: SmtlibFpToReal[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
