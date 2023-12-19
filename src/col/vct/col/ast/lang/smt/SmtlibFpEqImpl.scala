package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpEq
import vct.col.ast.{Type, TBool}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpEqOps

trait SmtlibFpEqImpl[G] extends SmtlibFpEqOps[G] { this: SmtlibFpEq[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
