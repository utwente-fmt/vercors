package vct.col.ast.expr.veymont

import vct.col.ast.expr.ExprImpl
import vct.col.ast.ops.ReceiverOps
import vct.col.ast.{Receiver, Type}
import vct.col.check.{CheckContext, CheckError, OnlyInChannelInvariant}
import vct.col.print._

trait ReceiverImpl[G] extends ReceiverOps[G] with ExprImpl[G] {
  this: Receiver[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("\\receiver")
  override def precedence: Int = Precedence.ATOMIC
  override def t: Type[G] = ref.decl.receiver.get.decl.t

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (context.inCommunicateInvariant match {
        case Some(_) => Seq()
        case None => Seq(OnlyInChannelInvariant(this))
      })
}
