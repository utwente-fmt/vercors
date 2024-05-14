package vct.col.ast.expr.heap.read

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{Deref, EndpointUse, Expr, TClass, Type}
import vct.col.check.{Check, CheckContext, CheckError, SeqProgReceivingEndpoint}
import vct.col.print.{Ctx, Doc, Group, Precedence}
import vct.col.ref.Ref
import vct.col.ast.ops.DerefOps

trait DerefImpl[G] extends ExprImpl[G] with DerefOps[G] { this: Deref[G] =>
  override def t: Type[G] =
    obj.t.asClass.map(_.instantiate(ref.decl.t)).getOrElse(ref.decl.t)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    Check.inOrder(
      super.check(context),
      Check.inOrder(obj.t.asClass.get.cls.decl.checkDefines(ref.decl, this),
        currentEndpointCheck(context))
    )

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(obj) <> "." <> ctx.name(ref)

  def root(): Expr[G] = this match {
    case Deref(inner: Deref[G], _) => inner.root()
    case _ => obj
  }

  def currentEndpointCheck(context: CheckContext[G]): Seq[CheckError] =
    (context.currentChoreography, context.currentReceiverEndpoint) match {
    case (Some(_), Some(currentReceiver)) => root() match {
      case EndpointUse(Ref(receiver)) if currentReceiver != receiver =>
        Seq(SeqProgReceivingEndpoint(this))
      case _ => Seq()
    }
    case _ => Seq()
  }
}