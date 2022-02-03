package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.temporaryimplpackage.expr.ExprImpl
import vct.col.ast.{ModelDeref, Type}
import vct.col.check.{Check, CheckContext, CheckError}

trait ModelDerefImpl[G] extends ExprImpl[G] { this: ModelDeref[G] =>
  override def t: Type[G] = ref.decl.t
  override def check(context: CheckContext[G]): Seq[CheckError] = {
    Check.inOrder(
      super.check(context),
      obj.t.asModel.get.model.decl.checkDefines(ref.decl, this),
    )
  }
}