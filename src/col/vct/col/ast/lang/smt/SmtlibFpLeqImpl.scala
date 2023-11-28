package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpLeq, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpLeqOps

trait SmtlibFpLeqImpl[G] extends SmtlibFpLeqOps[G] { this: SmtlibFpLeq[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
