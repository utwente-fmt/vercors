package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpToReal, TRational, Type}
import vct.col.print._

trait SmtlibFpToRealImpl[G] {
  this: SmtlibFpToReal[G] =>
  override def t: Type[G] = TRational()
  // def layout(implicit ctx: Ctx): Doc = ???
}
