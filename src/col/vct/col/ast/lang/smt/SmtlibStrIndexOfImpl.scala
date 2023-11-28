package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrIndexOf, TInt, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrIndexOfOps

trait SmtlibStrIndexOfImpl[G] extends SmtlibStrIndexOfOps[G] { this: SmtlibStrIndexOf[G] =>
  override def t: Type[G] = TInt()
  // def layout(implicit ctx: Ctx): Doc = ???
}
