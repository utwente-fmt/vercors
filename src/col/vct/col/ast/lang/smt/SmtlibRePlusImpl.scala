package vct.col.ast.lang.smt

import vct.col.ast.SmtlibRePlus
import vct.col.ast.Type
import vct.col.print._

trait SmtlibRePlusImpl[G] { this: SmtlibRePlus[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
