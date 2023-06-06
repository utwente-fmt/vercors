package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibRTP, TSmtlibRoundingMode, Type}
import vct.col.print._

trait SmtlibRTPImpl[G] { this: SmtlibRTP[G] =>
  override def t: Type[G] = TSmtlibRoundingMode()
  // def layout(implicit ctx: Ctx): Doc = ???
}
