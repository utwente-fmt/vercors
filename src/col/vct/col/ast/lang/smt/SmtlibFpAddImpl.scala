package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpAdd
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpAddImpl[G] { this: SmtlibFpAdd[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
