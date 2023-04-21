package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpCast
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpCastImpl[G] { this: SmtlibFpCast[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
