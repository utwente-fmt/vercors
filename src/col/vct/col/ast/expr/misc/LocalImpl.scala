package vct.col.ast.expr.misc

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{Local, Type}
import vct.col.check.{CheckContext, CheckError}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.LocalOps

trait LocalImpl[G] extends ExprImpl[G] with LocalOps[G] { this: Local[G] =>
  override def t: Type[G] = ref.decl.t
  override def check(context: CheckContext[G]): Seq[CheckError] =
    context.checkInScope(this, ref)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(ref))
}