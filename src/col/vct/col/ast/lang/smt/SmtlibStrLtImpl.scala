package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrLt, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrLtOps

trait SmtlibStrLtImpl[G] extends SmtlibStrLtOps[G] { this: SmtlibStrLt[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
