package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpIsNaN, TBool, Type}
import vct.col.print._

trait SmtlibFpIsNaNImpl[G] {
  this: SmtlibFpIsNaN[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
