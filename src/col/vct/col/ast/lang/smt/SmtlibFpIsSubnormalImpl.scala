package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpIsSubnormal
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpIsSubnormalImpl[G] { this: SmtlibFpIsSubnormal[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
