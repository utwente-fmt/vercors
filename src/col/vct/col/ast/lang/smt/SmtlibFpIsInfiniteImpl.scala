package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpIsInfinite, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpIsInfiniteOps

trait SmtlibFpIsInfiniteImpl[G] extends SmtlibFpIsInfiniteOps[G] { this: SmtlibFpIsInfinite[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
