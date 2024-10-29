package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpIsPositive, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpIsPositiveOps

trait SmtlibFpIsPositiveImpl[G] extends SmtlibFpIsPositiveOps[G] {
  this: SmtlibFpIsPositive[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
