package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvAdd
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBvAddImpl[G] { this: SmtlibBvAdd[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
