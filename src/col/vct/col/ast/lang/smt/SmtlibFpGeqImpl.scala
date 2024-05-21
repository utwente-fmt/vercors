package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpGeq, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpGeqOps

trait SmtlibFpGeqImpl[G] extends SmtlibFpGeqOps[G] { this: SmtlibFpGeq[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
