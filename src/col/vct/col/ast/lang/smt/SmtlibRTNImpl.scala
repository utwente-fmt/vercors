package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibRTN, TSmtlibRoundingMode, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibRTNOps

trait SmtlibRTNImpl[G] extends SmtlibRTNOps[G] { this: SmtlibRTN[G] =>
  override def t: Type[G] = TSmtlibRoundingMode()
  // def layout(implicit ctx: Ctx): Doc = ???
}
