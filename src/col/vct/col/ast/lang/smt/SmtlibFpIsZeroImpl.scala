package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpIsZero, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpIsZeroOps

trait SmtlibFpIsZeroImpl[G] extends SmtlibFpIsZeroOps[G] {
  this: SmtlibFpIsZero[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
