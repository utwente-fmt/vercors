package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibLiteralString, TSmtlibString, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibLiteralStringOps

trait SmtlibLiteralStringImpl[G] extends SmtlibLiteralStringOps[G] {
  this: SmtlibLiteralString[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
