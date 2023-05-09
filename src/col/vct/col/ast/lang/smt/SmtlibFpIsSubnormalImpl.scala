package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpIsSubnormal, TBool, Type}
import vct.col.print._

trait SmtlibFpIsSubnormalImpl[G] { this: SmtlibFpIsSubnormal[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
