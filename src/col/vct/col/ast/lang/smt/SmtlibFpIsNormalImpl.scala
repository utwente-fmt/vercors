package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpIsNormal
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpIsNormalImpl[G] { this: SmtlibFpIsNormal[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
