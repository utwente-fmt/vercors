package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.temporaryimplpackage.expr.ExprImpl
import vct.col.ast.{Deref, Type}
import vct.col.check.{Check, CheckContext, CheckError}

trait DerefImpl extends ExprImpl { this: Deref =>
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    Check.inOrder(
      super.check(context),
      obj.t.asClass.get.cls.decl.checkDefines(ref.decl, this)
    )
}