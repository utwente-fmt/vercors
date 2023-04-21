package vct.col.ast.lang.smt

import vct.col.ast.SmtlibConcat
import vct.col.ast.Type
import vct.col.print._

trait SmtlibConcatImpl[G] { this: SmtlibConcat[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
