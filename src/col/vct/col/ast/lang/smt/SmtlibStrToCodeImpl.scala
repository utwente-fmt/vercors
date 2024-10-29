package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrToCode, TInt, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibStrToCodeOps

trait SmtlibStrToCodeImpl[G] extends SmtlibStrToCodeOps[G] {
  this: SmtlibStrToCode[G] =>
  override def t: Type[G] = TInt()
  // def layout(implicit ctx: Ctx): Doc = ???
}
