package vct.col.ast.expr.heap.read

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{ModelDeref, Type}
import vct.col.check.{Check, CheckContext, CheckError}
import vct.col.print.{Ctx, Doc, Precedence}

trait ModelDerefImpl[G] extends ExprImpl[G] {
  this: ModelDeref[G] =>
  override def t: Type[G] = ref.decl.t
  override def check(context: CheckContext[G]): Seq[CheckError] = {
    Check.inOrder(
      super.check(context),
      obj.t.asModel.get.model.decl.checkDefines(ref.decl, this),
    )
  }

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(obj) <> "." <> ctx.name(ref)
}
