package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpFromUInt
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpFromUIntImpl[G] { this: SmtlibFpFromUInt[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
