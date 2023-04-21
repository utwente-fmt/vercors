package vct.col.ast.lang.smt

import vct.col.ast.SmtlibExtract
import vct.col.ast.Type
import vct.col.print._

trait SmtlibExtractImpl[G] { this: SmtlibExtract[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
