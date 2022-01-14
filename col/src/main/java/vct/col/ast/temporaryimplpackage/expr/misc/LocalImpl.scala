package vct.col.ast.temporaryimplpackage.expr.misc

import vct.col.ast.temporaryimplpackage.expr.ExprImpl
import vct.col.ast.{Local, Type}
import vct.col.check.{CheckContext, CheckError}

trait LocalImpl[G] extends ExprImpl[G] { this: Local[G] =>
  override def t: Type[G] = ref.decl.t
  override def check(context: CheckContext[G]): Seq[CheckError] =
    context.checkInScope(this, ref)
}