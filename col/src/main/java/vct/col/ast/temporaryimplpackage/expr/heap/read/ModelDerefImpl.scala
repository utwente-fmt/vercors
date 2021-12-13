package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{ModelDeref, Type}
import vct.col.check.{CheckContext, CheckError}

trait ModelDerefImpl[G] { this: ModelDeref[G] =>
  override def t: Type[G] = ref.decl.t
  override def check(context: CheckContext[G]): Seq[CheckError] =
    context.checkInScope(this, ref)
}