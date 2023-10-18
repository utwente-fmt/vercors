package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReUnion, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReUnionImpl[G] {
  this: SmtlibReUnion[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
