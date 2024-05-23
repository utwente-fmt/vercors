package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpLt, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpLtOps

trait SmtlibFpLtImpl[G] extends SmtlibFpLtOps[G] { this: SmtlibFpLt[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
