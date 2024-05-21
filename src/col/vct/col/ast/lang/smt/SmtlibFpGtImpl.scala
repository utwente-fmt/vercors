package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpGt, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpGtOps

trait SmtlibFpGtImpl[G] extends SmtlibFpGtOps[G] { this: SmtlibFpGt[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
