package vct.col.ast.expr.veymont

import vct.col.ast.expr.ExprImpl
import vct.col.ast.ops.SenderOps
import vct.col.ast.{Sender, Type}
import vct.col.check.{CheckContext, CheckError, OnlyInChannelInvariant}
import vct.col.print._

trait SenderImpl[G] extends SenderOps[G] with ExprImpl[G] {
  this: Sender[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("\\sender")
  override def precedence: Int = Precedence.ATOMIC
  override def t: Type[G] = ref.decl.sender.get.decl.t

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (context.inCommunicateInvariant match {
        case Some(_) => Seq()
        case None => Seq(OnlyInChannelInvariant(this))
      })
}
