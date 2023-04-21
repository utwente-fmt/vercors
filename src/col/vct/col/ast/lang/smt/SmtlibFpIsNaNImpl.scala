package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpIsNaN
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpIsNaNImpl[G] { this: SmtlibFpIsNaN[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
