package vct.col.ast.expr.veymont

import vct.col.ast.expr.ExprImpl
import vct.col.ast.ops.MessageOps
import vct.col.ast.{Message, Type}
import vct.col.check.{CheckContext, CheckError}
import vct.col.print._

trait MessageImpl[G] extends MessageOps[G] with ExprImpl[G] {
  this: Message[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("\\msg")
  override def precedence: Int = Precedence.ATOMIC
  override def t: Type[G] = ref.decl.msg.t

  // We don't check for being in a channel invariant here because the only other place
  // where you can use a `\msg` is as the message of a communicate. In that cause, you get
  // infinite recursion in the typechecker anyway, so might as well not check for this here.
  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context)
}
