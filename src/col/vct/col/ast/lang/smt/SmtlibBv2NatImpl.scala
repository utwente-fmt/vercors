package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibBv2Nat, TInt, Type}
import vct.col.ast.ops.SmtlibBv2NatOps

trait SmtlibBv2NatImpl[G] extends SmtlibBv2NatOps[G] {
  this: SmtlibBv2Nat[G] =>
  override def t: Type[G] = TInt()
  // def layout(implicit ctx: Ctx): Doc = ???
}
