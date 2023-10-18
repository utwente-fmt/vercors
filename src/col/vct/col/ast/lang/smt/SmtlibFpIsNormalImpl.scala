package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpIsNormal, TBool, Type}
import vct.col.print._

trait SmtlibFpIsNormalImpl[G] {
  this: SmtlibFpIsNormal[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
