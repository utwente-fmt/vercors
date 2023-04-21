package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvMul
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBvMulImpl[G] { this: SmtlibBvMul[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
