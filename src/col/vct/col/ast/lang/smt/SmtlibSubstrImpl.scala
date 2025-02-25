package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibSubstr, TSmtlibString, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibSubstrOps

trait SmtlibSubstrImpl[G] extends SmtlibSubstrOps[G] {
  this: SmtlibSubstr[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
