package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrLen, TInt, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrLenOps

trait SmtlibStrLenImpl[G] extends SmtlibStrLenOps[G] {
  this: SmtlibStrLen[G] =>
  override def t: Type[G] = TInt()
  // def layout(implicit ctx: Ctx): Doc = ???
}
