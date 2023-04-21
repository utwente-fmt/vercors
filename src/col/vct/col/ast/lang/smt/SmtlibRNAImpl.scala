package vct.col.ast.lang.smt

import vct.col.ast.SmtlibRNA
import vct.col.ast.Type
import vct.col.print._

trait SmtlibRNAImpl[G] { this: SmtlibRNA[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
