package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpToUInt
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpToUIntImpl[G] { this: SmtlibFpToUInt[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
