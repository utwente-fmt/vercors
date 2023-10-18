package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReDiff, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReDiffImpl[G] {
  this: SmtlibReDiff[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
