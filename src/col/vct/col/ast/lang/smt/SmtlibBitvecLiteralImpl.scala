package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBitvecLiteral
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBitvecLiteralImpl[G] { this: SmtlibBitvecLiteral[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
