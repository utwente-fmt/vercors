package vct.col.ast.lang.smt

import vct.col.ast.SmtlibRTP
import vct.col.ast.Type
import vct.col.print._

trait SmtlibRTPImpl[G] { this: SmtlibRTP[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
