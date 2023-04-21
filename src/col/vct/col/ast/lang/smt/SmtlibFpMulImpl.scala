package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpMul
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpMulImpl[G] { this: SmtlibFpMul[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
